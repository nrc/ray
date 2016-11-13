use std::mem;

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

#[derive(Clone, Debug)]
pub struct Polygon {
    p1: Point,
    p2: Point,
    p3: Point,
    material: Material,
    bounding_box: Aabb,
    // May be uninitialized.
    internals: PolygonInternals,
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

    fn contains_point(&self, p: Point) -> bool {
        p >= self.min && p <= self.max
    }
}

#[derive(Clone, Debug)]
struct PolygonInternals {
    normal: Point,
    u: Point,
    v: Point,
    uv: f32,
    uu: f32,
    vv: f32,
    triangle_denom: f32,
}

pub trait Object: Send {
    fn intersects(&self, ray: &Ray) -> Option<Intersection>;
    fn translate(&mut self, point: Point);
    fn transform(&mut self, m: &Matrix);
    fn material(&self) -> &Material;
    fn pre_compute(&mut self);
    fn bounding_box(&self) -> &Aabb;
    fn overlaps(&self, p: &Aabb) -> bool;
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

    fn pre_compute(&mut self) {
        self.bounding_box = Aabb::new(self.center - self.radius, self.center + self.radius);
    }

    fn bounding_box(&self) -> &Aabb {
        &self.bounding_box
    }
    fn overlaps(&self, _bb: &Aabb) -> bool {
        unimplemented!();
    }
}

impl Polygon {
    pub fn new(p1: Point, p2: Point, p3: Point, material: Material) -> Polygon {
        Polygon {
            p1: p1,
            p2: p2,
            p3: p3,
            material: material,
            internals: unsafe { mem::uninitialized() },
            bounding_box: Aabb::new(Point::new(0.0, 0.0, 0.0), Point::new(0.0, 0.0, 0.0)),
        }
    }
}

unsafe impl Sync for Polygon {}

impl Object for Polygon {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        // if !self.bounding_box.intersects(ray) {
        //     return None;
        // }

        let internals = &self.internals;

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

        if r <= 0.00001 || s <= 0.00001 || r + s >= 1.00001 {
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

    fn pre_compute(&mut self) {
        let u = self.p2 - self.p1;
        let v = self.p3 - self.p1;
        let uv = dot(u, v);
        let uu = dot(u, u);
        let vv = dot(v, v);
        let triangle_denom = uv * uv - uu * vv;

        self.internals = PolygonInternals {
            normal: cross(u, v).normalise(),
            u: u,
            v: v,
            uv: uv,
            uu: uu,
            vv: vv,
            triangle_denom: triangle_denom,            
        };

        let min = self.p1.min(self.p2).min(self.p3);
        let max = self.p1.max(self.p2).max(self.p3);
        self.bounding_box = Aabb::new(min, max);
    }

    fn bounding_box(&self) -> &Aabb {
        &self.bounding_box
    }

    // This isn't quite correct, since it is possible for a polygon to overlap
    // a box even if none of the polygon's points are inside the box. However,
    // for all uses of this function, that is OK - the polygon will be caught by
    // a different box.
    fn overlaps(&self, bb: &Aabb) -> bool {
        bb.contains_point(self.p1) ||
        bb.contains_point(self.p2) ||
        bb.contains_point(self.p3)
    }
}

#[derive(Clone)]
pub struct Group<T: Object> {
    objects: Vec<T>,
    div_objects: Vec<Group<T>>,
    bounding_box: Aabb,
}

impl<T: Object + ::std::fmt::Debug> Group<T> {
    pub fn new(objects: Vec<T>) -> Group<T> {
        Group {
            objects: objects,
            div_objects: vec![],
            bounding_box: Aabb::new(Point::new(0.0, 0.0, 0.0), Point::new(0.0, 0.0, 0.0)),
        }
    }

    pub fn pre_compute(&mut self) {
        let mut min = Point::new(::std::f32::INFINITY, ::std::f32::INFINITY, ::std::f32::INFINITY);
        let mut max = Point::new(::std::f32::NEG_INFINITY, ::std::f32::NEG_INFINITY, ::std::f32::NEG_INFINITY);
        for o in &mut self.objects {
            o.pre_compute();
            let bb = o.bounding_box();
            min = min.min(bb.min);
            max = max.max(bb.max);
        }
        self.bounding_box = Aabb::new(min, max);

        self.split(0);
    }

    fn split(&mut self, depth: usize) {
        if depth >= ::HBV_DEPTH || self.objects.len() < 60 {
            return;
        }

        let min = self.bounding_box.min;
        let max = self.bounding_box.max;
        let mid = (min + max) / 2.0;
        let mut groups = vec![Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(min.x(), min.y(), min.z()), Point::new(mid.x(), mid.y(), mid.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(mid.x(), min.y(), min.z()), Point::new(max.x(), mid.y(), mid.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(min.x(), min.y(), mid.z()), Point::new(mid.x(), mid.y(), max.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(mid.x(), min.y(), mid.z()), Point::new(max.x(), mid.y(), max.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(min.x(), mid.y(), min.z()), Point::new(mid.x(), max.y(), mid.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(mid.x(), mid.y(), min.z()), Point::new(max.x(), max.y(), mid.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(min.x(), mid.y(), mid.z()), Point::new(mid.x(), max.y(), max.z())),
                              },
                              Group {
                                  objects: vec![],
                                  div_objects: vec![],
                                  bounding_box: Aabb::new(Point::new(mid.x(), mid.y(), mid.z()), Point::new(max.x(), max.y(), max.z())),
                              }];
        'outer: for o in self.objects.drain(..) {
            for g in &mut groups {
                if o.overlaps(&g.bounding_box) {
                    g.objects.push(o);
                    continue 'outer;
                }
            }
            unreachable!("Object did not fit in any group");
        }

        // Extend the bounding box to cover all the polygons.
        for g in &mut groups {
            for o in &g.objects {
                let bb = o.bounding_box();
                let min = g.bounding_box.min.min(bb.min);
                let max = g.bounding_box.max.max(bb.max);
                g.bounding_box = Aabb::new(min, max);
            }
        }

        self.div_objects = groups.into_iter().filter(|g| g.objects.len() > 0).collect();

        for g in &mut self.div_objects {
            g.split(depth + 1);
        }
    }

    pub fn intersects<'a>(&'a self, ray: &Ray) -> Option<Intersection<'a>> {
        if !self.bounding_box.intersects(ray) {
            return None;
        }
        self.div_objects
            .iter()
            .filter_map(|o| o.intersects(ray))
            .chain(self.objects.iter().filter_map(|o| o.intersects(ray)))
            .min()
    }

    pub fn translate(&mut self, v: Point) {
        for o in &mut self.objects {
            o.translate(v);
        }
    }

    pub fn transform(&mut self, m: &Matrix) {
        for o in &mut self.objects {
            o.transform(m);
        }
    }
}
