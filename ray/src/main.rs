#![feature(proc_macro)]

extern crate image;
extern crate rand;
#[macro_use]
extern crate derive_new;

use image::ColorType;
use image::png::PNGEncoder;

use std::cell::RefCell;
use std::cmp::Ordering;
use std::env;
use std::fs::File;

const RAY_DEPTH: u8 = 4;
const SUPER_SAMPLES: u8 = 2;

// TODO only ambient light?

// Because std::cmp::min/max needs Ord, not PartialOrd.
fn min(v1: f64, v2: f64) -> f64 {
    if v1 >= v2 { v2 } else { v1 }
}

fn max(v1: f64, v2: f64) -> f64 {
    if v2 >= v1 { v2 } else { v1 }
}

fn init() -> Scene {
    let mut objects: Vec<Box<Object>> = vec![];
    let mut lights = vec![];

    // objects.push(new Sphere(new Point(20, 20, 0), 20, red_plastic()));
    objects.push(Box::new(Sphere::new(Point::new(-100.0, 0.0, 0.0), 40.0, Material::blue_plastic())));
    objects.push(Box::new(Sphere::new(Point::new(30.0, 0.0, 0.0), 20.0, Material::blue_plastic())));
    // objects.push(new Polygon(new Point(150, -50, 250), new Point(150, -50, -150), new Point(-150, -50, -150), mirror()));

    // attenuation: { distance: 500, moderation: 0.5 }
    lights.push(Light::Point(PointLight::new(Point::new(0.0, 0.0, 0.0), Colour::new(0.7, 0.7, 0.7), None)));
    // lights.push(new SphereLight(new Point(100, 150, 0), 20, new Colour(0.7, 0.7, 0.7)));
    // lights.push(new PointLight(new Point(-150, 0, -150), new Colour(0.5, 0.5, 0.5), null));

    Scene {
        objects: objects,
        lights: lights,
        ambient_light: Colour::new(0.2, 0.2, 0.2),
        eye: Eye {
            from: Point::new(0.0, 0.0, -300.0),
            at: Point::new(0.0, 0.0, 0.0),
            length: 50.0,
            width: 50.0,
            height: 50.0,
        },
        background: Colour::black(),
    }
}

struct Rendered {
    data: Vec<u8>,
    width: u32,
    height: u32,
}

impl Rendered {
    fn new(width: u32, height: u32) -> Rendered {
        Rendered {
            data: vec![0; (width * height * 4) as usize],
            width: width,
            height: height,
        }
    }

    fn set_pixel(&mut self, x: u32, y: u32, colour: Colour) {
        let offset = 4 * (x + y * self.width) as usize;
        self.data[offset] = min(colour.r * 255.0, 255.0) as u8;
        self.data[offset + 1] = min(colour.g * 255.0, 255.0) as u8;
        self.data[offset + 2] = min(colour.b * 255.0, 255.0) as u8;
        self.data[offset + 3] = 255;
    }
}

fn render(mut scene: Scene) -> Rendered {
    let mut result = Rendered::new(400, 400);

    world_transform(&mut scene);

    scene.render(&mut result);

    // println!("transform: " + (t2 - t1) + "ms");
    // println!("rendering: " + (t3 - t2) + "ms");
    // println!("write to canvas: " + (t4 - t3) + "ms");

    result
}

// Here we do some transforms to the world to make it easier to render.
fn world_transform(scene: &mut Scene) {
    // Translate the world so that eye.from is at the origin.
    let from = scene.eye.from;
    for o in &mut scene.objects {
        o.translate(from);
    }
    for l in &mut scene.lights {
        l.from().translate(from);
    }
    scene.eye.at.translate(from);
    // Should be (0, 0, 0);
    scene.eye.from.translate(from);

    // console.log("translate");
    // console.log(object.center);
    // console.log(light.from);
    // console.log(eye.at);
    // console.log(eye.from);

    // Rotate the world so that eye.at is on the z-axis (i.e., eye.at.x == eye.at.y == 0).
    // Compute the rotation matrix.
    let sqrt_ax_sq_plus_az_sq = (scene.eye.at.x * scene.eye.at.x + scene.eye.at.z * scene.eye.at.z).sqrt();
    let sqrt_ax_sq_plus_ay_sq_plus_az_sq = (scene.eye.at.x * scene.eye.at.x + scene.eye.at.y * scene.eye.at.y + scene.eye.at.z * scene.eye.at.z).sqrt();

    let sin_tilt = scene.eye.at.y / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let cos_tilt = sqrt_ax_sq_plus_az_sq / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let sin_pan = scene.eye.at.x / sqrt_ax_sq_plus_az_sq;
    let cos_pan = scene.eye.at.z / sqrt_ax_sq_plus_az_sq;

    let rot_at_matrix = [[cos_tilt * cos_pan, -sin_tilt, cos_tilt * sin_pan],
                         [sin_tilt * cos_pan, cos_tilt, sin_tilt * sin_pan],
                         [-sin_pan, 0.0, cos_pan]];

    for o in &mut scene.objects {
        o.transform(&rot_at_matrix);
    }
    for l in &mut scene.lights {
        l.from().post_mult(&rot_at_matrix);
    }
    scene.eye.at.post_mult(&rot_at_matrix);

    // console.log("rotate");
    // console.log(object.center);
    // console.log(light.from);
    // console.log(eye.from);
    // console.log(eye.at);

    // At this stage we are looking directly down the Z-axis from the origin to positive infinity.
}

fn trace(ray: Ray, depth: u8, scene: &Scene) -> Colour {
    if depth >= RAY_DEPTH {
        return Colour::black();
    }

    match intersects(scene, &ray) {
        Some(intersection) => {
            let material = intersection.object.material().clone();
            let mut result = Colour::black();

            let ambient = mult_colours(material.ambient, scene.ambient_light);
            result = result.add(ambient);

            let reflect_vec = subtract_points(ray.direction, mult_point_scalar(intersection.normal, dot(ray.direction, intersection.normal) * 2.0));
            let reflect_ray = Ray::new(intersection.point, reflect_vec);
            let reflected = trace(reflect_ray, depth + 1, scene);
            result = result.add(mult_colours(material.reflected, reflected));

            for light in &scene.lights {
                // Only compute specular illumination for primary rays.
                let view_vec = if depth == 0 {
                    Some(ray.direction)
                } else {
                    None
                };
                result = result.add(light.illuminate(scene, intersection.point, intersection.normal, material.clone(), view_vec));
            }

            result
        }
        None => scene.background,
    }
}

fn intersects<'a>(scene: &'a Scene, ray: &Ray) -> Option<Intersection<'a>> {
    let mut results: Vec<Intersection<'a>> = scene.objects.iter().filter_map(|o| o.intersects(ray)).collect();

    results.sort();

    results.into_iter().next()
}

impl Scene {
    fn render(&self, dest: &mut Rendered) {
        // We must translate and scale the pixel on to the image plane.
        let trans_x = dest.width as f64 / 2.0;
        let trans_y = dest.height as f64 / 2.0;
        let scale_x = self.eye.width / dest.width as f64;
        let scale_y = -self.eye.height / dest.height as f64;

        let sub_const = (SUPER_SAMPLES * 2) as f64;
        let sub_pixel_x = scale_x / sub_const;
        let sub_pixel_y = scale_y / sub_const;

        for y in 0..dest.height {
            let image_y = (y as f64 - trans_y) * scale_y;
            for x in 0..dest.width {
                let image_x = (x as f64 - trans_x) * scale_x;

                // Super-sampling.
                let mut points = vec![];
                let mut yy = image_y - (scale_y / 2.0);
                for _ in 0..SUPER_SAMPLES {
                    let mut xx = image_x - (scale_x / 2.0);
                    for _ in 0..SUPER_SAMPLES {
                        points.push(Point::new(xx, yy, self.eye.length));
                        xx += sub_pixel_x;
                    }
                    yy += sub_pixel_y;
                }

                let mut sum = Colour::black();
                for p in &points {
                    // Note that due to the world transform, eye.from is the origin.
                    let ray = Ray::new(self.eye.from, p.normalise());
                    sum = sum.add(trace(ray, 0, self));
                }
                dest.set_pixel(x, y, sum.mult_scalar(1.0 / (SUPER_SAMPLES * SUPER_SAMPLES) as f64));
            }
        }
    }
}

type Matrix = [[f64; 3]; 3];

fn mult_point_scalar(point: Point, scalar: f64) -> Point {
    Point::new(point.x * scalar,
               point.y * scalar,
               point.z * scalar)
}

fn add_points(a: Point, b: Point) -> Point {
    Point::new(a.x + b.x,
               a.y + b.y,
               a.z + b.z)
}

fn subtract_points(a: Point, b: Point) -> Point {
    Point::new(a.x - b.x,
               a.y - b.y,
               a.z - b.z)
}

fn dot(a: Point, b: Point) -> f64 {
    a.x * b.x +
    a.y * b.y +
    a.z * b.z
}

fn cross(a: Point, b: Point) -> Point {
    Point::new(a.y * b.z - a.z * b.y,
               a.z * b.x - a.x * b.z,
               a.x * b.y - a.y * b.x)
}

fn mult_colours(a: Colour, b: Colour) -> Colour {
    Colour::new(a.r * b.r,
                a.g * b.g,
                a.b * b.b)
}

fn mult_colour_scalar(colour: Colour, scalar: f64) -> Colour {
    Colour::new(colour.r * scalar,
                colour.g * scalar,
                colour.b * scalar)
}

fn add_colours(a: Colour, b: Colour) -> Colour {
    Colour::new(a.r + b.r,
                a.g + b.g,
                a.b + b.b)
}

#[derive(Debug, Clone, Copy, new)]
struct Point {
    x: f64, y: f64, z: f64
}

impl Point {
    fn translate(mut self, by: Point) -> Point {
        self.x -= by.x;
        self.y -= by.y;
        self.z -= by.z;

        self
    }

    fn add(mut self, other: Point) -> Point {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;

        self
    }

    fn post_mult(mut self, matrix: &Matrix) -> Point {
        let new_x = self.x * matrix[0][0] + self.y * matrix[1][0] + self.z * matrix[2][0];
        let new_y = self.x * matrix[0][1] + self.y * matrix[1][1] + self.z * matrix[2][1];
        let new_z = self.x * matrix[0][2] + self.y * matrix[1][2] + self.z * matrix[2][2];

        self.x = new_x;
        self.y = new_y;
        self.z = new_z;

        self
    }

    fn mult_scalar(mut self, other: f64) -> Point {
        self.x *= other;
        self.y *= other;
        self.z *= other;

        self
    }

    fn normalise(mut self) -> Point {
        let magnitude = self.magnitude();
        self.x /= magnitude;
        self.y /= magnitude;
        self.z /= magnitude;

        self
    }

    fn magnitude(&self) -> f64 {
        (self.x.powf(2.0) + self.y.powf(2.0) + self.z.powf(2.0)).sqrt()
    }
}

#[derive(Debug, Clone, Copy, new)]
struct Colour {
    r: f64, b: f64, g: f64
}

impl Colour {
    fn red() -> Colour {
        Colour::new(0.5, 0.0, 0.0)
    }

    fn blue() -> Colour {
        Colour::new(0.0, 0.0, 1.0)
    }

    fn white() -> Colour {
        Colour::new(1.0, 1.0, 1.0)
    }

    fn grey() -> Colour {
        Colour::new(0.75, 0.75, 0.75)
    }

    fn dark_grey() -> Colour {
        Colour::new(0.25, 0.25, 0.25)
    }

    fn black() -> Colour {
        Colour::new(0.0, 0.0, 0.0)
    }

    fn add(mut self, other: Colour) -> Colour {
        self.r += other.r;
        self.g += other.g;
        self.b += other.b;

        self
    }

    fn mult(mut self, other: Colour) -> Colour {
        self.r *= other.r;
        self.g *= other.g;
        self.b *= other.b;

        self
    }

    fn mult_scalar(mut self, other: f64) -> Colour {
        self.r *= other;
        self.g *= other;
        self.b *= other;

        self
    }
}

#[derive(Debug, Clone, new)]
struct Ray {
    origin: Point,
    direction: Point,
}

#[derive(Debug, Clone, new)]
struct Material {
    diffuse: Colour,
    specular: Colour,
    ambient: Colour,
    reflected: Colour,
    shininess: f64,
}

impl Material {
    fn red_plastic() -> Material {
        Material::new(Colour::red(), Colour::grey(), Colour::red(), Colour::dark_grey(), 8.0)
    }

    fn blue_plastic() -> Material {
        Material::new(Colour::blue(), Colour::grey(), Colour::blue(), Colour::dark_grey(), 8.0)
    }

    fn mirror() -> Material {
        Material::new(Colour::dark_grey(), Colour::grey(), Colour::dark_grey(), Colour::grey(), 2.0)
    }
}

#[derive(Clone, new)]
struct Intersection<'scene> {
    object: &'scene Object,
    normal: Point,
    point: Point,
    t: f64,
}

impl<'scene> PartialOrd for Intersection<'scene> {
    fn partial_cmp(&self, other: &Intersection) -> Option<Ordering> {
        self.t.partial_cmp(&other.t)
    }
}

impl<'scene> Ord for Intersection<'scene> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap_or(Ordering::Equal)
    }
}

impl<'scene> PartialEq for Intersection<'scene> {
    fn eq(&self, other: &Intersection) -> bool {
        self.t == other.t
    }
}

impl<'scene> Eq for Intersection<'scene> {}

#[derive(Debug, Clone, new)]
struct Sphere {
    center: Point,
    radius: f64,
    material: Material,
}

struct Polygon {
    p1: Point,
    p2: Point,
    p3: Point,
    material: Material,
    internals: RefCell<Option<PolygonInternals>>,
}

struct PolygonInternals {
    normal: Point,
    u: Point,
    v: Point,
    uv: f64,
    uu: f64,
    vv: f64,
    triangle_denom: f64,
}

trait Object {
    fn intersects(&self, ray: &Ray) -> Option<Intersection>;
    fn translate(&mut self, point: Point);
    fn transform(&mut self, m: &Matrix);
    fn material(&self) -> &Material;
}

impl Object for Sphere {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        let rel_center = subtract_points(ray.origin, self.center);

        let l_dot_rel_center = dot(ray.direction, rel_center);
        let rel_center_sq = dot(rel_center, rel_center);
        let under_sqrt = l_dot_rel_center.powf(2.0) - rel_center_sq + self.radius.powf(2.0);

        if under_sqrt >= 0.0 {
            let sqrt = under_sqrt.sqrt();
            let t = -sqrt - l_dot_rel_center;
            if t <= 0.001 {
                // Intersection is behind the origin of the ray.
                return None;
            }
            let point = mult_point_scalar(ray.direction, t).add(ray.origin);
            let normal = subtract_points(point, self.center).normalise();
            Some(Intersection::new(self, normal, point, t))
        } else {
            None
        }
    }

    fn translate(&mut self, v: Point) {
        self.center.translate(v);
    }

    // Note that this won't apply scales properly.
    fn transform(&mut self, m: &Matrix) {
        self.center.post_mult(m);
    }

    fn material(&self) -> &Material {
        &self.material
    }
}

impl Polygon {
    fn new(p1: Point, p2: Point, p3: Point, material: Material) -> Polygon {
        Polygon {
            p1: p1,
            p2: p2,
            p3: p3,
            material: material,
            internals: RefCell::new(None),
        }
    }

    fn compute_normal(&self) {
        let u = subtract_points(self.p2, self.p1);
        let v = subtract_points(self.p3, self.p1);
        let uv = dot(u, v);
        let uu = dot(u, u);
        let vv = dot(v, v);
        let triangle_denom = uv * uv - uu * vv;

        *self.internals.borrow_mut() = Some(PolygonInternals {
            normal: cross(u, v).normalise(),
            u: u,
            v: v,
            uv: uv,
            uu: uu,
            vv: vv,
            triangle_denom: triangle_denom,            
        });
    }
}

impl Object for Polygon {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        if self.internals.borrow().is_none() {
            self.compute_normal();
        }

        let internals = self.internals.borrow();
        let internals = internals.as_ref().unwrap();

        let normal = internals.normal;

        // First test if the ray intersects the plane of the polygon.
        let plane_denom = dot(normal, ray.direction);
        if plane_denom <= 0.001 && plane_denom >= -0.001 {
            // Ray is parallel to plane.
            return None;
        }

        let t = dot(normal, subtract_points(self.p1, ray.origin)) / plane_denom;
        if t <= 0.001 {
            // Plane is behind ray's origin.
            return None;
        }
        let point = mult_point_scalar(ray.direction, t).add(ray.origin);

        // Test if the intersection point is within the triangle.
        let w = subtract_points(point, self.p1);
        let wu = dot(w, internals.u);
        let wv = dot(w, internals.v);

        let r = (internals.uv * wv - internals.vv * wu) / internals.triangle_denom;
        let s = (internals.uv * wu - internals.uu * wv) / internals.triangle_denom;

        if r <= 0.001 || s <= 0.001 || r + s >= 1.0 {
            return None;
        }

        Some(Intersection::new(self, point, normal, t))
    }

    fn translate(&mut self, v: Point) {
        self.p1.translate(v);
        self.p2.translate(v);
        self.p3.translate(v);
        *self.internals.borrow_mut() = None;
    }

    fn transform(&mut self, m: &Matrix) {
        self.p1.post_mult(m);
        self.p2.post_mult(m);
        self.p3.post_mult(m);        
        *self.internals.borrow_mut() = None;
    }

    fn material(&self) -> &Material {
        &self.material
    }
}


// TODO trait
#[derive(Debug, Clone)]
enum Light {
    Point(PointLight),
    Sphere(SphereLight),
}

impl Light {
    // If view_vec is None, illuminate will not take account of specular illumination.
    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, view_vec: Option<Point>) -> Colour {
        match *self {
            Light::Point(ref pl) => pl.illuminate(scene, point, normal, material, view_vec),
            Light::Sphere(ref sl) => sl.illuminate(scene, point, normal, material, view_vec),
        }
    }

    fn from(&mut self) -> &mut Point {
        match *self {
            Light::Point(ref mut pl) => &mut pl.from,
            Light::Sphere(ref mut sl) => &mut sl.from,
        }        
    }
}

#[derive(Debug, Clone, new)]
struct PointLight {
    from: Point,
    colour: Colour,
    attenuation: Option<Attenuation>,
}

impl PointLight {
    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, view_vec: Option<Point>) -> Colour {
        let result = Colour::black();

        let light_vec = subtract_points(self.from, point);

        let distance = light_vec.magnitude();
        let attenuation_factor = self.attenuation_factor(distance);
        if attenuation_factor == 0.0 {
            return result;
        }

        // Normalise
        light_vec.mult_scalar(1.0 / distance);
        let light_ray = Ray::new(point, light_vec);
        if intersects(scene, &light_ray).is_some() {
            // In shadow.
            return result;
        }

        let diffuse_dot = dot(light_vec, normal);
        if diffuse_dot <= 0.0 {
            // Facing away from light.
            return result;
        }
        let attenuated_colour = mult_colour_scalar(self.colour, attenuation_factor);
        let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot).mult(attenuated_colour);
        result.add(diffuse);

        if let Some(view_vec) = view_vec {
            let light_reflect_vec = subtract_points(normal, light_vec).mult_scalar(dot(light_vec, normal) * 2.0);
            let specular_dot = min(1.0, max(0.0, dot(mult_point_scalar(view_vec, -1.0), light_reflect_vec)));
            let specular = mult_colour_scalar(material.specular, specular_dot.powf(material.shininess)).mult(attenuated_colour);
            result.add(specular);
        }

        result
    }

    fn attenuation_factor(&self, d: f64) -> f64 {
        match self.attenuation {
            Some(attenuation) => {
                if d >= attenuation.distance {
                    return 0.0;
                }

                let k = attenuation.distance * attenuation.moderation;
                let dk = d / k;
                let dm = d / attenuation.distance;
                1.0 / (dk / (1.0 - dm.powf(2.0)) + 1.0).powf(2.0)
            }
            None => 1.0,
        }
    }
}

#[derive(Debug, Clone, Copy, new)]
struct Attenuation {
    distance: f64,
    moderation: f64,
}

#[derive(Debug, Clone)]
struct SphereLight{
    from: Point,
    radius: f64,
    colour: Colour,
    samples: u8,
}

impl SphereLight {
    fn new(from: Point, radius: f64, colour: Colour) -> SphereLight {
        SphereLight {
            from: from,
            radius: radius,
            colour: colour,
            samples: 16,
        }
    }

    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, _view_vec: Option<Point>) -> Colour {
        let result = Colour::black();

        for _ in 0..self.samples {
            let light_point = self.random_point();
            let light_vec = subtract_points(light_point, point).normalise();
            let light_ray = Ray::new(point, light_vec);
            if intersects(scene, &light_ray).is_some() {
                // In shadow.
                continue;
            }

            let diffuse_dot = dot(light_vec, normal);
            if diffuse_dot <= 0.0 {
                // Facing away from light.
                continue;
            }
            let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot / self.samples as f64).mult(self.colour);
            result.add(diffuse);

            // Could compute a specular component, but seems expensive and maybe inappropriate.
        }

        result
    }

    // Return a random point on the surface of the sphere.
    fn random_point(&self) -> Point {
        loop {
            // Compute a random point in an axis-aligned cube at the origin, covering -1 to 1.
            let dx = rand::random::<f64>() * 2.0 - 1.0;
            let dy = rand::random::<f64>() * 2.0 - 1.0;
            let dz = rand::random::<f64>() * 2.0 - 1.0;

            // Check if our random point is in the sphere (happens apx 50% of the time).
            let sum: f64 = dx * dx + dy * dy + dz * dz;
            if sum <= 1.0 {
                // Project the point in the sphere on to the surface by normalising.
                let sqrt_sum = sum.sqrt();
                let dx = dx / sqrt_sum;
                let dy = dy / sqrt_sum;
                let dz = dz / sqrt_sum;

                // Scale and translate to the desired sphere.
                return Point::new(self.from.x + dx * self.radius,
                                  self.from.y + dy * self.radius,
                                  self.from.z + dz * self.radius);
            }
        }
    }

}

struct Scene {
    objects: Vec<Box<Object>>,
    lights: Vec<Light>,
    ambient_light: Colour,
    eye: Eye,
    background: Colour,
}

struct Eye {
    from: Point,
    at: Point,
    length: f64,
    width: f64,
    height: f64,
}

fn run(file_name: &str) {
    let scene = init();
    let data = render(scene);

    let file = File::create(file_name).unwrap();
    let encoder = PNGEncoder::new(file);
    encoder.encode(&data.data, data.width, data.height, ColorType::RGBA(8)).unwrap();
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Requires a single argument - the name of the file to emit");
        return;
    }

    run(&args[1])
}
