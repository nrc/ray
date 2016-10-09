extern crate image;

use image::ColorType;
use image::png::PNGEncoder;

use std::env;
use std::fs::File;

const RAY_DEPTH: u8 = 4;
const SUPER_SAMPLES: u8 = 2;


fn init() -> Scene {
    let mut objects = vec![];
    let mut lights = vec![];

    // objects.push(new Sphere(new Point(20, 20, 0), 20, red_plastic()));
    objects.push(Object::Sphere(Sphere::new(Point::new(-100, 0, 0), 40, Material::blue_plastic())));
    objects.push(Object::Sphere(Sphere::new(Point::new(30, 0, 0), 20, Material::blue_plastic())));
    // objects.push(new Polygon(new Point(150, -50, 250), new Point(150, -50, -150), new Point(-150, -50, -150), mirror()));

    // attenuation: { distance: 500, moderation: 0.5 }
    lights.push(Light::Point(PointLight::new(Point::new(0, 0, 0), Colour::new(0.7, 0.7, 0.7), None)));
    // lights.push(new SphereLight(new Point(100, 150, 0), 20, new Colour(0.7, 0.7, 0.7)));
    // lights.push(new PointLight(new Point(-150, 0, -150), new Colour(0.5, 0.5, 0.5), null));

    Scene {
        objects: objects,
        lights: lights
        ambient_light: Colour(0.2, 0.2, 0.2),
        eye: Eye {
            from: Point::new(0, 0, -300),
            at: Point::new(0, 0, 0),
            length: 50,
            width: 50,
            height: 50,
        },
        background: Colour::black(),
    }
}

struct Rendered {
    data: Vec<u8>,
    width: usize,
    height: usize,
}

impl Rendered {
    fn new(width: usize, height: usize) -> Rendered {
        Rendered {
            data = vec![u8; width + height * 4],
            width: width,
            height: height,
        }
    }

    fn set_pixel(&mut self, x: usize, y: usize, colour: &Colour) {
        let offset = 4 * (x + y * self.width);
        self.data[offset] = Math.min(colour.r * 255, 255);
        self.data[offset + 1] = Math.min(colour.g * 255, 255);
        self.data[offset + 2] = Math.min(colour.b * 255, 255);
        self.data[offset + 3] = 255;
    }
}

fn render(mut scene: Scene) -> Rendered {
    let mut result = Rendered::new(400, 400);

    world_transform(&mut scene);

    scene.render(&scene, &mut result);

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
        l.from.translate(from);
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
    let sqrt_ax_sq_plus_az_sq = Math.sqrt(scene.eye.at.x * scene.eye.at.x + scene.eye.at.z * scene.eye.at.z);
    let sqrt_ax_sq_plus_ay_sq_plus_az_sq = Math.sqrt(scene.eye.at.x * scene.eye.at.x + scene.eye.at.y * scene.eye.at.y + scene.eye.at.z * scene.eye.at.z);

    let sin_tilt = scene.eye.at.y / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let cos_tilt = sqrt_ax_sq_plus_az_sq / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let sin_pan = scene.eye.at.x / sqrt_ax_sq_plus_az_sq;
    let cos_pan = scene.eye.at.z / sqrt_ax_sq_plus_az_sq;

    let rot_at_matrix = [[cos_tilt * cos_pan, -sin_tilt, cos_tilt * sin_pan],
                         [sin_tilt * cos_pan, cos_tilt, sin_tilt * sin_pan],
                         [-sin_pan, 0, cos_pan]];

    for o in &mut scene.objects {
        o.transform(&rot_at_matrix);
    }
    for l in &mut scene.lights {
        l.from.post_mult(&rot_at_matrix);
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

    match intersects(scene, ray) {
        Some(intersection) => {
            let material = intersection.object.material;
            let mut result = black();

            let ambient = mult_colours(material.ambient, ambient_light.colour);
            result.add(ambient);

            let reflect_vec = subtract_points(ray.direction, mult_point_scalar(intersection.normal, dot(ray.direction, intersection.normal) * 2));
            let reflect_ray = new Ray(intersection.point, reflect_vec);
            let reflected = trace(reflect_ray, depth + 1, scene);
            result.add(mult_colours(material.reflected, reflected));

            for light in &scene.lights {
                // Only compute specular illumination for primary rays.
                let view_vec = if depth == 0 {
                    Some(ray.direction)
                } else {
                    None
                };
                result.add(light.illuminate(intersection.point, intersection.normal, material, view_vec));
            }

            result
        }
        None => scene.background,
    }
}

fn intersects(scene: &Scene, ray: Ray) -> Option<Intersection> {
    let mut results = scene.objects.iter().filter_map(|o| o.intersects(ray)).collect();

    results.sort();

    results.get(0)
}

impl Scene {
    fn render(&self, dest: &mut Rendered) {
        // We must translate and scale the pixel on to the image plane.
        let trans_x = dest.width / 2;
        let trans_y = dest.height / 2;
        let scale_x = self.eye.width / dest.width;
        let scale_y = -self.eye.height / dest.height;

        let sub_const = SUPER_SAMPLES * 2;
        let sub_pixel_x = scale_x / sub_const;
        let sub_pixel_y = scale_y / sub_const;

        for y in 0..dest.height {
            let image_y = (y - trans_y) * scale_y;
            for x in 0..dest.width {
                let image_x = (x - trans_x) * scale_x;

                // Super-sampling.
                let mut points = vec![];
                let yy = image_y - (scale_y / 2);
                for sy in 0..SUPER_SAMPLES {
                    let xx = image_x - (scale_x / 2);
                    for sx in 0..SUPER_SAMPLES {
                        points.push(new Point(xx, yy, self.eye.length))
                        xx += sub_pixel_x;
                    }
                    yy += sub_pixel_y;
                }

                let mut sum = Colour::black();
                for p in &points {
                    // Note that due to the world transform, eye.from is the origin.
                    let ray = Ray::new(self.eye.from, p.normalise());
                    sum.add(trace(ray, 0));
                }
                dest.setPixel(x, y, sum.mult_scalar(1 / (SUPER_SAMPLES * SUPER_SAMPLES)));
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
    fn translate(&mut self, by) -> &Point {
        self.x -= by.x;
        self.y -= by.y;
        self.z -= by.z;

        self
    };

    fn add(&mut self, other) -> &Point {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;

        self
    };

    fn post_mult(&mut self, matrix) -> &Point {
        let new_x = self.x * matrix[0][0] + self.y * matrix[1][0] + self.z * matrix[2][0];
        let new_y = self.x * matrix[0][1] + self.y * matrix[1][1] + self.z * matrix[2][1];
        let new_z = self.x * matrix[0][2] + self.y * matrix[1][2] + self.z * matrix[2][2];

        self.x = new_x;
        self.y = new_y;
        self.z = new_z;

        self
    };

    fn mult_scalar(&mut self, other) -> &Point {
        self.x *= other;
        self.y *= other;
        self.z *= other;

        self
    }

    fn normalise(&mut self) -> &Point {
        let magnitude = self.magnitude();
        self.x /= magnitude;
        self.y /= magnitude;
        self.z /= magnitude;

        self
    }

    fn magnitude(&self) -> f64 {
        Math.sqrt(Math.pow(self.x, 2) + Math.pow(self.y, 2) + Math.pow(self.z, 2))
    }
}

#[derive(Debug, Clone, Copy, new)]
struct Colour {
    r: f64, b: f64, g: f64
}

impl Colour {
    fn red() -> Colour {
        Colour::new(0.5, 0, 0)
    }

    fn blue() -> Colour {
        Colour::new(0, 0, 1)
    }

    fn white() -> Colour {
        Colour::new(1, 1, 1)
    }

    fn grey() -> Colour {
        Colour::new(0.75, 0.75, 0.75)
    }

    fn dark_grey() -> Colour {
        Colour::new(0.25, 0.25, 0.25)
    }

    fn black() -> Colour {
        Colour::new(0, 0, 0)
    }

    fn add(&mut self, other) -> &Colour {
        self.r += other.r;
        self.g += other.g;
        self.b += other.b;

        self
    }

    fn mult(&mut self, other) -> &Colour {
        self.r *= other.r;
        self.g *= other.g;
        self.b *= other.b;

        self
    }

    fn mult_scalar(&mut self, other) -> &Colour {
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
        Material::new(Colour::red(), Colour::grey(), Colour::red(), Colour::dark_grey(), 8)
    }

    fn blue_plastic() -> Material {
        Material::new(Colour::blue(), Colour::grey(), Colour::blue(), Colour::dark_grey(), 8)
    }

    fn mirror() -> Material {
        Material::new(Colour::dark_grey(), Colour::grey(), Colour::dark_grey(), Colour::grey(), 2)
    }
}

#[derive(Debug, Clone, new)]
struct Intersection<'scene> {
    object: &'scene Object,
    normal: Point,
    point: Point,
    t: f64,
}

impl<'scene> PartialOrd for Intersection<'scene> {
    fn foo(&self, other: &Intersection<'scene>) {
        self.t - other.t
    }
}

// TODO should be trait
#[derive(Debug, Clone)]
enum Object {
    Sphere(Sphere),
    Polygon(Polygon),
}

#[derive(Debug, Clone, new)]
struct Sphere {
    center: Point,
    radius: f64,
    material: Material,
}

#[derive(Debug, Clone)]
struct Polygon {
    p1: Point,
    p2: Point,
    p3: Point,
    material: Material,
    normal: Option<Point>,
    u: Point,
    v: Point,
    uv: f64,
    uu: f64,
    vv: f64,
    triangle_denom f64,
}

impl Object {
    fn intersects(&mut self, ray: &Ray) -> Option<Intersection> {
        match *self {
            Object::Sphere(ref s) => s.intersects(ray),
            Object::Polygon(ref mut p) => p.intersects(ray),
        }
    }

    fn translate(&mut self, p: Point) {
        match *self {
            Object::Sphere(ref mut s) => s.translate(p),
            Object::Polygon(ref mut p) => p.translate(p),
        }
    }

    fn transform(&mut self, m: &Matrix) {
        match *self {
            Object::Sphere(ref mut s) => s.transform(m),
            Object::Polygon(ref mut p) => p.transform(m),
        }
    }
}

impl Sphere {
    fn intersects(&self, ray: &Ray) -> Option<Intersection> {
        let rel_center = subtract_points(ray.origin, this.center);

        let l_dot_rel_center = dot(ray.direction, rel_center);
        let rel_center_sq = dot(rel_center, rel_center);
        let under_sqrt = Math.pow(l_dot_rel_center, 2) - rel_center_sq + Math.pow(this.radius, 2);

        if under_sqrt >= 0 {
            let sqrt = Math.sqrt(under_sqrt);
            let t = -sqrt - l_dot_rel_center;
            if t <= 0.001 {
                // Intersection is behind the origin of the ray.
                return None;
            }
            let point = mult_point_scalar(ray.direction, t).add(ray.origin);
            let normal = subtract_points(point, this.center).normalise();
            Some(Intersection::new(this, normal, point, t))
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
}

impl Polygon {
    fn new(p1: Point, p2: Point, p3: Point, material: Material) -> Polygon {
        Polygon {
            p1: p1,
            p2: p2,
            p3: p3,
            material: material,
            normal: None,
            u: Point::new(0, 0, 0),
            v: Point::new(0, 0, 0),
            uv: 0,
            uu: 0,
            vv: 0,
            triangle_denom 0,
        }
    }

    fn intersects(&mut self, ray: &Ray) -> Option<Intersection> {
        if self.normal.is_none() {
            self.compute_normal();
        }

        let normal = self.normal.unwrap();

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
        let wu = dot(w, self.u);
        let wv = dot(w, self.v);

        let r = (self.uv * wv - self.vv * wu) / self.triangle_denom;
        let s = (self.uv * wu - self.uu * wv) / self.triangle_denom;

        if r <= 0.001 || s <= 0.001 || r + s >= 1 {
            return None;
        }

        Some(Intersection::new(self, point, normal, t))
    }

    fn compute_normal(&mut self) {
        self.u = subtract_points(p2, p1);
        self.v = subtract_points(p3, p1);
        self.normal = cross(self.u, self.v).normalise();
        self.uv = dot(self.u, self.v);
        self.uu = dot(self.u, self.u);
        self.vv = dot(self.v, self.v);
        self.triangle_denom = self.uv * self.uv - self.uu * self.vv;
    }

    fn translate(&mut self, v: Point) {
        self.p1.translate(v);
        self.p2.translate(v);
        self.p3.translate(v);
        self.normal = None;
    }

    fn transform(&mut self, m: &Matrix) {
        self.p1.post_mult(m);
        self.p2.post_mult(m);
        self.p3.post_mult(m);        
        self.normal = None;
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
            Light::Point(ref pl) => pl.illuminate(point, scene, normal, material, view_vec),
            Light::Sphere(ref sl) => sl.illuminate(point, scene, normal, material, view_vec),
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
        if attenuation_factor == 0 {
            return result;
        }

        // Normalise
        light_vec.mult_scalar(1 / distance);
        let light_ray = new Ray(point, light_vec);
        if intersects(scene, light_ray) {
            // In shadow.
            return result;
        }

        let diffuse_dot = dot(light_vec, normal);
        if diffuse_dot <= 0 {
            // Facing away from light.
            return result;
        }
        let attenuated_colour = mult_colour_scalar(self.colour, attenuation_factor);
        let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot).mult(attenuated_colour);
        result.add(diffuse);

        if let Some(view_vec) = view_vec {
            let light_reflect_vec = subtract_points(normal, light_vec).mult_scalar(dot(light_vec, normal) * 2);
            let specular_dot = Math.min(1, Math.max(0, dot(mult_point_scalar(view_vec, -1), light_reflect_vec)));
            let specular = mult_colour_scalar(material.specular, Math.pow(specular_dot, material.shininess)).mult(attenuated_colour);
            result.add(specular);
        }

        result
    }

    fn attenuation_factor(&self, d: f64) -> f64 {
        match self.attenuation {
            Some(attenuation) => {
                if d >= attenuation.distance {
                    return 0;
                }

                let k = attenuation.distance * attenuation.moderation;
                let dk = d / k;
                let dm = d / attenuation.distance;
                1 / Math.pow(dk / (1 - Math.pow(dm, 2)) + 1, 2)
            }
            None => 1,
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

        for i in 0..self.samples {
            let light_point = self.random_point();
            let light_vec = subtract_points(light_point, point).normalise();
            let light_ray = Ray::new(point, light_vec);
            if intersects(scene, light_ray).is_some() {
                // In shadow.
                continue;
            }

            let diffuse_dot = dot(light_vec, normal);
            if diffuse_dot <= 0 {
                // Facing away from light.
                continue;
            }
            let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot / self.samples).mult(self.colour);
            result.add(diffuse);

            // Could compute a specular component, but seems expensive and maybe inappropriate.
        }

        result
    }

    // Return a random point on the surface of the sphere.
    fn random_point(&self) -> Point {
        loop {
            // Compute a random point in an axis-aligned cube at the origin, covering -1 to 1.
            let dx = Math.random() * 2 - 1;
            let dy = Math.random() * 2 - 1;
            let dz = Math.random() * 2 - 1;

            // Check if our random point is in the sphere (happens apx 50% of the time).
            let sum = dx * dx + dy * dy + dz * dz;
            if (sum <= 1) {
                // Project the point in the sphere on to the surface by normalising.
                let sqrt_sum = Math.pow(sum, 0.5);
                dx /= sqrt_sum;
                dy /= sqrt_sum;
                dz /= sqrt_sum;

                // Scale and translate to the desired sphere.
                return Point::new(self.from.x + dx * self.radius,
                                  self.from.y + dy * self.radius,
                                  self.from.z + dz * self.radius);
            }
        }
    }

}

struct Scene {
    objects: Vec<Object>,
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
