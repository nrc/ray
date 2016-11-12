#![feature(proc_macro)]
#![feature(link_llvm_intrinsics)]

extern crate image;
extern crate rand;
#[macro_use]
extern crate derive_new;
extern crate simd;

use image::ColorType;
use image::png::PNGEncoder;

use std::sync::Arc;
use std::cell::UnsafeCell;
use std::cmp::Ordering;
use std::env;
use std::fs::File;
use std::sync::Mutex;
use std::time::Instant;
use std::thread;

use colour::*;
use lights::*;
use objects::*;
use point::*;

mod colour;
mod lights;
mod point;
mod objects;

const RAY_DEPTH: u8 = 4;
const SUPER_SAMPLES: u8 = 2;
const THREADS: u32 = 4;

extern {
    #[link_name = "llvm.minnum.f32"]
    fn min_intrinsic(v1: f32, v2: f32) -> f32;
    #[link_name = "llvm.maxnum.f32"]
    fn max_intrinsic(v1: f32, v2: f32) -> f32;
}

fn min(v1: f32, v2: f32) -> f32 {
    unsafe { min_intrinsic(v1, v2) }
}
fn max(v1: f32, v2: f32) -> f32 {
    unsafe { max_intrinsic(v1, v2) }
}

pub trait Scene: Clone + Send + 'static {
    fn intersects<'a>(&'a self, ray: &Ray) -> Option<Intersection<'a>>;
    fn pre_compute(&mut self);
    fn translate(&mut self, v: Point);
    fn transform(&mut self, m: &Matrix);
    fn eye(&self) -> &Eye;
    fn ambient_light(&self) -> Colour;
    fn background(&self) -> Colour;
    fn for_each_point_light<F>(&self, f: F) -> Colour
        where F: Fn(&PointLight) -> Colour;
    fn for_each_sphere_light<F>(&self, f: F) -> Colour
        where F: Fn(&SphereLight) -> Colour;

    // Here we do some transforms to the world to make it easier to render.
    fn world_transform(&mut self) {
        // Translate the world so that eye.from is at the origin.
        let from = self.eye().from;
        self.translate(from);

        // Rotate the world so that eye.at is on the z-axis (i.e., eye.at.x == eye.at.y == 0).
        // We first rotate around the y-axis until we are aligned with the z-axis,
        // then around the z-axis until we are on the x/z plane.
        // Compute the rotation matrix.
        let rot_y_matrix = {
            let eye = self.eye();
            let hyp_y = (eye.at.x() * eye.at.x() + eye.at.z() * eye.at.z()).sqrt();
            let sin_y = eye.at.x() / hyp_y;
            let cos_y = eye.at.z() / hyp_y;
            [[cos_y, 0.0, sin_y], [0.0, 1.0, 0.0], [-sin_y, 0.0, cos_y]]
        };
        self.transform(&rot_y_matrix);

        let rot_x_matrix = {
            let eye = self.eye();
            let hyp_x = (eye.at.y() * eye.at.y() + eye.at.z() * eye.at.z()).sqrt();
            let sin_x = eye.at.y() / hyp_x;
            let cos_x = eye.at.z() / hyp_x;
            [[1.0, 0.0, 0.0], [0.0, cos_x, sin_x], [0.0, -sin_x, cos_x]]
        };
        self.transform(&rot_x_matrix);

        // println!("post-transform: {:?}, {:?}, {:?}",
        //          self.point_lights[0].from(),
        //          self.eye().from,
        //          self.eye().at);

        // At this stage we are looking directly down the Z-axis from the origin to positive infinity.
    }

    fn render(mut self, dest: Arc<Rendered>) {
        self.world_transform();

        // We must translate and scale the pixel on to the image plane.
        let (width, height) = {
            (dest.width, dest.height)
        };

        let trans_x = width as f32 / 2.0;
        let trans_y = height as f32 / 2.0;
        let scale_x = self.eye().width / width as f32;
        let scale_y = -self.eye().height / height as f32;

        let sub_const = (SUPER_SAMPLES * 2) as f32;
        let sub_pixel_x = scale_x / sub_const;
        let sub_pixel_y = scale_y / sub_const;

        // Must be done after the world_transform, but before we clone the scene
        // for each thread.
        self.pre_compute();

        // TODO could be an atomic, rather than a mutex
        let running_count = Arc::new(Mutex::new(THREADS));

        for t in 0..THREADS {
            let this = self.clone();
            let running_count = running_count.clone();
            let current = thread::current();
            let dest = dest.clone();
            thread::spawn(move || {
                let mut y = t;
                loop {
                    if y >= height {
                        break;
                    }
                    let image_y = (y as f32 - trans_y) * scale_y;
                    for x in 0..width {
                        let image_x = (x as f32 - trans_x) * scale_x;

                        let mut sum = Colour::black();
                        let mut yy = image_y - (scale_y / 2.0);
                        for _ in 0..SUPER_SAMPLES {
                            let mut xx = image_x - (scale_x / 2.0);
                            for _ in 0..SUPER_SAMPLES {
                                // Note that due to the world transform, eye.from should be the origin.
                                let eye = this.eye();
                                let p = Point::new(xx, yy, eye.length).normalise();
                                let ray = Ray::new(eye.from, p);
                                sum += trace(ray, 0, &this);

                                xx += sub_pixel_x;
                            }
                            yy += sub_pixel_y;
                        }

                        dest.set_pixel(x, y, sum * (1.0 / (SUPER_SAMPLES * SUPER_SAMPLES) as f32));
                    }
                    y += THREADS;
                }

                {
                    let mut running_count = running_count.lock().unwrap();
                    *running_count -= 1;
                }
                current.unpark();
            });
        }

        loop {
            {
                let running_count = running_count.lock().unwrap();
                if *running_count == 0 {
                    break;
                }
            }
            thread::park();
        }
    }
}

#[derive(Clone)]
struct Scene1 {
    balls1: Group<Sphere>,
    balls2: Group<Sphere>,
    triangles: Group<Polygon>,
    sphere_lights: Vec<SphereLight>,
    point_lights: Vec<PointLight>,
    ambient_light: Colour,
    eye: Eye,
    background: Colour,
}

impl Scene1 {
    fn new() -> Scene1 {
        let mut balls1: Vec<Sphere> = vec![];
        for i in 0..3 {
            for j in 0..3 {
                balls1.push(Sphere::new(Point::new(i as f32 * 50.0 - 50.0, 0.0, j as f32 * 50.0 - 50.0), (i + j + 1) as f32 * 5.0, Material::red_plastic()));
            }
        }

        let mut balls2: Vec<Sphere> = vec![];
        for i in 0..2 {
            for j in 0..2 {
                balls2.push(Sphere::new(Point::new(i as f32 * 50.0 - 25.0, 50.0, j as f32 * 50.0 - 25.0), 20.0, Material::blue_plastic()));
            }
        }

        let mut triangles: Vec<Polygon> = vec![];
        triangles.push(Polygon::new(Point::new(0.0, -50.0, 0.0),
                                    Point::new(0.0, -50.0, -100.0),
                                    Point::new(-100.0, -50.0, 0.0),
                                    Material::matte_grey()));
        triangles.push(Polygon::new(Point::new(0.0, -50.0, -100.0),
                                    Point::new(-100.0, -50.0, -100.0),
                                    Point::new(-100.0, -50.0, 0.0),
                                    Material::matte_grey()));
        triangles.push(Polygon::new(Point::new(100.0, -50.0, 0.0),
                                    Point::new(100.0, -50.0, -100.0),
                                    Point::new(0.0, -50.0, 0.0),
                                    Material::mirror()));
        triangles.push(Polygon::new(Point::new(100.0, -50.0, -100.0),
                                    Point::new(0.0, -50.0, -100.0),
                                    Point::new(0.0, -50.0, 0.0),
                                    Material::mirror()));
        triangles.push(Polygon::new(Point::new(0.0, -50.0, 100.0),
                                    Point::new(0.0, -50.0, 0.0),
                                    Point::new(-100.0, -50.0, 100.0),
                                    Material::mirror()));
        triangles.push(Polygon::new(Point::new(0.0, -50.0, 0.0),
                                    Point::new(-100.0, -50.0, 0.0),
                                    Point::new(-100.0, -50.0, 100.0),
                                    Material::mirror()));
        triangles.push(Polygon::new(Point::new(100.0, -50.0, 100.0),
                                    Point::new(100.0, -50.0, 0.0),
                                    Point::new(0.0, -50.0, 100.0),
                                    Material::matte_grey()));
        triangles.push(Polygon::new(Point::new(100.0, -50.0, 0.0),
                                    Point::new(0.0, -50.0, 0.0),
                                    Point::new(0.0, -50.0, 100.0),
                                    Material::matte_grey()));

        // attenuation: { distance: 500, moderation: 0.5 }
        let sl = SphereLight::new(Point::new(-60.0, 100.0, -80.0), 30.0, Colour::new(0.8, 0.8, 0.8));
        let pl = PointLight::new(Point::new(60.0, 80.0, -80.0), Colour::new(0.2, 0.2, 0.2), None);

        Scene1 {
            balls1: Group::new(balls1),
            balls2: Group::new(balls2),
            triangles: Group::new(triangles),
            point_lights: vec![pl],
            sphere_lights: vec![sl],

            ambient_light: Colour::new(0.2, 0.2, 0.2),
            eye: Eye {
                from: Point::new(140.0, 100.0, -300.0),
                at: Point::new(0.0, 0.0, 0.0),
                length: 70.0,
                width: 50.0,
                height: 50.0,
            },
            background: Colour::black(),
        }
    }
}

impl Scene for Scene1 {
    fn intersects<'a>(&'a self, ray: &Ray) -> Option<Intersection<'a>> {
        let balls1 = self.balls1.intersects(ray);
        let balls2 = self.balls2.intersects(ray);
        let triangles = self.triangles.intersects(ray);

        balls1.into_iter().chain(balls2.into_iter()).chain(triangles.into_iter()).min()
    }

   fn pre_compute(&mut self) {
        self.balls1.pre_compute();
        self.balls2.pre_compute();
        self.triangles.pre_compute();
        for l in &mut self.sphere_lights {
            l.pre_compute();
        }
    }

    fn translate(&mut self, v: Point) {
        self.balls1.translate(v);
        self.balls2.translate(v);
        self.triangles.translate(v);
        for l in &mut self.point_lights {
            *l.from() -= v;
        }
        for l in &mut self.sphere_lights {
            *l.from() -= v;
        }
        self.eye.at -= v;
        self.eye.from -= v;
    }

    fn transform(&mut self, m: &Matrix) {
        self.balls1.transform(m);
        self.balls2.transform(m);
        self.triangles.transform(m);
        for l in &mut self.point_lights {
            *l.from() = l.from().post_mult(m);
        }
        for l in &mut self.sphere_lights {
            *l.from() = l.from().post_mult(m);
        }
        self.eye.at = self.eye.at.post_mult(m);
    }

    fn eye(&self) -> &Eye {
        &self.eye
    }

    fn ambient_light(&self) -> Colour {
        self.ambient_light
    }

    fn background(&self) -> Colour {
        self.background
    }

    fn for_each_point_light<F: Fn(&PointLight) -> Colour>(&self, f: F) -> Colour {
        let mut result = Colour::black();
        for light in &self.point_lights {
            result += f(light);
        }
        result
    }

    fn for_each_sphere_light<F: Fn(&SphereLight) -> Colour>(&self, f: F) -> Colour {
        let mut result = Colour::black();
        for light in &self.sphere_lights {
            result += f(light);
        }
        result
    }
}

pub struct Rendered {
    data: UnsafeCell<Vec<u8>>,
    width: u32,
    height: u32,
}

unsafe impl Sync for Rendered {}

impl Rendered {
    fn new(width: u32, height: u32) -> Rendered {
        Rendered {
            data: UnsafeCell::new(vec![0; (width * height * 4) as usize]),
            width: width,
            height: height,
        }
    }

    fn set_pixel(&self, x: u32, y: u32, colour: Colour) {
        let offset = 4 * (x + y * self.width) as usize;
        unsafe {
            (&mut *self.data.get())[offset] = min(colour.r() * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 1] = min(colour.g() * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 2] = min(colour.b() * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 3] = 255;
        }
    }
}

// TODO uses of scene
fn trace<S: Scene>(ray: Ray, depth: u8, scene: &S) -> Colour {
    if depth >= RAY_DEPTH {
        return Colour::black();
    }

    match scene.intersects(&ray) {
        Some(intersection) => {
            let material = &intersection.object.material();
            let mut result = Colour::black();

            let ambient = material.ambient * scene.ambient_light();
            result += ambient;

            let reflect_vec = ray.direction - intersection.normal * dot(ray.direction, intersection.normal) * 2.0;
            let reflect_ray = Ray::new(intersection.point, reflect_vec);
            let reflected = trace(reflect_ray, depth + 1, scene);
            result += material.reflected * reflected;

            result += scene.for_each_point_light(|l| {
                // Only compute specular illumination for primary rays.
                let view_vec = if depth == 0 {
                    Some(ray.direction)
                } else {
                    None
                };
                l.illuminate(scene, intersection.point, intersection.normal, material, view_vec)
            });
            result += scene.for_each_sphere_light(|l| {
                // Only compute specular illumination for primary rays.
                let view_vec = if depth == 0 {
                    Some(ray.direction)
                } else {
                    None
                };
                l.illuminate(scene, intersection.point, intersection.normal, material, view_vec)
            });

            result
        }
        None => scene.background(),
    }
}

#[derive(Debug, Clone)]
pub struct Ray {
    origin: Point,
    direction: Point,
    inverse: Point,
}

impl Ray {
    fn new(origin: Point, direction: Point) -> Ray {
        Ray {
            origin: origin,
            direction: direction,
            inverse: direction.inverse(),
        }
    }
}

#[derive(Clone, new)]
pub struct Intersection<'scene> {
    object: &'scene Object,
    normal: Point,
    point: Point,
    t: f32,
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

#[derive(Debug, Clone, Copy, new)]
pub struct Attenuation {
    distance: f32,
    moderation: f32,
}

#[derive(Clone)]
pub struct Eye {
    from: Point,
    at: Point,
    length: f32,
    width: f32,
    height: f32,
}

fn run(file_name: &str) {
    let scene = Scene1::new();
    let data = Arc::new(Rendered::new(800, 800));

    let t = Instant::now();

    scene.render(data.clone());

    let t = t.elapsed();

    let file = File::create(file_name).unwrap();
    let encoder = PNGEncoder::new(file);
    unsafe {
        encoder.encode(&*data.data.get(), data.width, data.height, ColorType::RGBA(8)).unwrap();
    }

    println!("Time: {}s", t.as_secs() as f64 + t.subsec_nanos() as f64 / 1_000_000_000.0);
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Requires a single argument - the name of the file to emit");
        return;
    }

    run(&args[1])
}
