use std::sync::atomic::{AtomicU8, Ordering};
use std::mem;
use std::cell::UnsafeCell;

use point::*;
use colour::*;
use {Intersection, Ray};

#[derive(Debug, Clone, new)]
pub struct Sphere {
    center: Point,
    radius: f32,
    material: Material,
}

pub struct Polygon {
    p1: Point,
    p2: Point,
    p3: Point,
    material: Material,
    // 0 = yes, 1 = computing, 2 = no
    internals_present: AtomicU8,
    // May be uninitialized.
    internals: UnsafeCell<PolygonInternals>,
}

struct PolygonInternals {
    normal: Point,
    u: Point,
    v: Point,
    uv: f32,
    uu: f32,
    vv: f32,
    triangle_denom: f32,
}

pub trait Object: Sync + Send {
    fn intersects(&self, ray: &Ray) -> Option<Intersection>;
    fn translate(&mut self, point: Point);
    fn transform(&mut self, m: &Matrix);
    fn material(&self) -> &Material;
}

impl Object for Sphere {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        let rel_center = ray.origin - self.center;

        let l_dot_rel_center = dot(ray.direction, rel_center);
        let rel_center_sq = dot(rel_center, rel_center);
        let under_sqrt = l_dot_rel_center * l_dot_rel_center - rel_center_sq + self.radius * self.radius;

        if under_sqrt >= 0.0 {
            let sqrt = under_sqrt.sqrt();
            let t = -sqrt - l_dot_rel_center;
            if t <= 0.1 {
                // Intersection is behind the origin of the ray.
                return None;
            }
            let point = (ray.direction * t) + ray.origin;
            let normal = (point - self.center).normalise();
            Some(Intersection::new(self, normal, point, t))
        } else {
            None
        }
    }

    fn translate(&mut self, v: Point) {
        self.center -= v;
    }

    // Note that this won't apply scales properly.
    fn transform(&mut self, m: &Matrix) {
        self.center = self.center.post_mult(m);
    }

    fn material(&self) -> &Material {
        &self.material
    }
}

impl Polygon {
    pub fn new(p1: Point, p2: Point, p3: Point, material: Material) -> Polygon {
        Polygon {
            p1: p1,
            p2: p2,
            p3: p3,
            material: material,
            internals_present: AtomicU8::new(2),
            internals: UnsafeCell::new(unsafe { mem::uninitialized() }),
        }
    }

    fn compute_normal(&self) {
        let u = self.p2 - self.p1;
        let v = self.p3 - self.p1;
        let uv = dot(u, v);
        let uu = dot(u, u);
        let vv = dot(v, v);
        let triangle_denom = uv * uv - uu * vv;

        unsafe {
            *(self.internals.get() as *mut PolygonInternals) = PolygonInternals {
                normal: cross(u, v).normalise(),
                u: u,
                v: v,
                uv: uv,
                uu: uu,
                vv: vv,
                triangle_denom: triangle_denom,            
            };
        }
        self.internals_present.store(0, Ordering::SeqCst);
    }
}

unsafe impl Sync for Polygon {}

impl Object for Polygon {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        if self.internals_present.load(Ordering::SeqCst) != 0 {
            if self.internals_present.compare_and_swap(2, 1, Ordering::SeqCst) == 2 {
                self.compute_normal()
            } else {
                while self.internals_present.load(Ordering::SeqCst) == 1 {}
            }
        }

        let internals = unsafe { &*self.internals.get() };

        let normal = internals.normal;

        // First test if the ray intersects the plane of the polygon.
        let plane_denom = dot(normal, ray.direction);
        if plane_denom <= 0.001 && plane_denom >= -0.001 {
            // Ray is parallel to plane.
            return None;
        }

        let t = dot(normal, self.p1 - ray.origin) / plane_denom;
        if t <= 0.001 {
            // Plane is behind ray's origin.
            return None;
        }
        let point = (ray.direction * t) + ray.origin;

        // Test if the intersection point is within the triangle.
        let w = point - self.p1;
        let wu = dot(w, internals.u);
        let wv = dot(w, internals.v);

        let r = (internals.uv * wv - internals.vv * wu) / internals.triangle_denom;
        let s = (internals.uv * wu - internals.uu * wv) / internals.triangle_denom;

        if r <= 0.001 || s <= 0.001 || r + s >= 1.0 {
            return None;
        }

        Some(Intersection::new(self, normal, point, t))
    }

    fn translate(&mut self, v: Point) {
        self.p1 -= v;
        self.p2 -= v;
        self.p3 -= v;
        self.internals_present.store(2, Ordering::SeqCst)
    }

    fn transform(&mut self, m: &Matrix) {
        self.p1 = self.p1.post_mult(m);
        self.p2 = self.p2.post_mult(m);
        self.p3 = self.p3.post_mult(m);        
        self.internals_present.store(2, Ordering::SeqCst)
    }

    fn material(&self) -> &Material {
        &self.material
    }
}
