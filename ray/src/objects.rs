use std::mem;
use std::cell::UnsafeCell;

use point::*;
use colour::*;
use {Intersection, Ray};

#[derive(Debug, Clone)]
pub struct Sphere {
    center: Point,
    radius: f32,
    material: Material,
    bounding_box: Aabb,
}

pub struct Polygon {
    p1: Point,
    p2: Point,
    p3: Point,
    material: Material,
    bounding_box: Aabb,
    // May be uninitialized.
    internals: UnsafeCell<PolygonInternals>,
}

#[derive(Debug, Clone, new)]
pub struct Aabb {
    min: Point,
    max: Point,
}

impl Aabb {
    pub fn intersects(&self, ray: &Ray) -> bool {
        let t_min = (self.min - ray.origin) * ray.inverse;
        let t_max = (self.max - ray.origin) * ray.inverse;

        let min = t_min.min(t_max).h_max();
        let max = t_min.max(t_max).h_min();

        max >= min
    }
}

impl Clone for Polygon {
    fn clone(&self) -> Polygon {
        Polygon {
            p1: self.p1,
            p2: self.p2,
            p3: self.p3,
            material: self.material.clone(),
            bounding_box: self.bounding_box.clone(),
            internals: unsafe { UnsafeCell::new((*self.internals.get()).clone()) },
        }        
    }
}

#[derive(Clone)]
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

impl Sphere {
    pub fn new(center: Point, radius: f32, material: Material) -> Sphere {
        Sphere {
            center: center,
            radius: radius,
            material: material,
            bounding_box: Aabb::new(Point::new(0.0, 0.0, 0.0), Point::new(0.0, 0.0, 0.0)),
        }
    }

    pub fn pre_compute(&mut self) {
        self.bounding_box = Aabb::new(self.center - self.radius, self.center + self.radius);
    }
}

impl Object for Sphere {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        // if !self.bounding_box.intersects(ray) {
        //     return None;
        // }

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
            internals: UnsafeCell::new(unsafe { mem::uninitialized() }),
            bounding_box: Aabb::new(Point::new(0.0, 0.0, 0.0), Point::new(0.0, 0.0, 0.0)),
        }
    }

    pub fn pre_compute(&mut self) {
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

        let min = self.p1.min(self.p2).min(self.p3);
        let max = self.p1.max(self.p2).max(self.p3);
        self.bounding_box = Aabb::new(min, max);
    }
}

unsafe impl Sync for Polygon {}

impl Object for Polygon {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        // if !self.bounding_box.intersects(ray) {
        //     return None;
        // }

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
    }

    fn transform(&mut self, m: &Matrix) {
        self.p1 = self.p1.post_mult(m);
        self.p2 = self.p2.post_mult(m);
        self.p3 = self.p3.post_mult(m);        
    }

    fn material(&self) -> &Material {
        &self.material
    }
}
