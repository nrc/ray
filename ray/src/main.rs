#![feature(proc_macro)]
#![feature(integer_atomics)]

extern crate image;
extern crate rand;
#[macro_use]
extern crate derive_new;

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

// Because std::cmp::min/max needs Ord, not PartialOrd.
fn min(v1: f32, v2: f32) -> f32 {
    if v1 >= v2 { v2 } else { v1 }
}

fn max(v1: f32, v2: f32) -> f32 {
    if v2 >= v1 { v2 } else { v1 }
}

fn init() -> Scene {
    let mut objects: Vec<Box<Object>> = vec![];

    for i in 0..3 {
        for j in 0..3 {
            objects.push(Box::new(Sphere::new(Point::new(i as f32 * 50.0 - 50.0, 0.0, j as f32 * 50.0 - 50.0), (i + j + 1) as f32 * 5.0, Material::red_plastic())));
        }
    }
    for i in 0..2 {
        for j in 0..2 {
            objects.push(Box::new(Sphere::new(Point::new(i as f32 * 50.0 - 25.0, 50.0, j as f32 * 50.0 - 25.0), 20.0, Material::blue_plastic())));
        }
    }

    objects.push(Box::new(Polygon::new(Point::new(0.0, -50.0, 0.0),
                                       Point::new(0.0, -50.0, -100.0),
                                       Point::new(-100.0, -50.0, 0.0),
                                       Material::matte_grey())));
    objects.push(Box::new(Polygon::new(Point::new(0.0, -50.0, -100.0),
                                       Point::new(-100.0, -50.0, -100.0),
                                       Point::new(-100.0, -50.0, 0.0),
                                       Material::matte_grey())));
    objects.push(Box::new(Polygon::new(Point::new(100.0, -50.0, 0.0),
                                       Point::new(100.0, -50.0, -100.0),
                                       Point::new(0.0, -50.0, 0.0),
                                       Material::mirror())));
    objects.push(Box::new(Polygon::new(Point::new(100.0, -50.0, -100.0),
                                       Point::new(0.0, -50.0, -100.0),
                                       Point::new(0.0, -50.0, 0.0),
                                       Material::mirror())));
    objects.push(Box::new(Polygon::new(Point::new(0.0, -50.0, 100.0),
                                       Point::new(0.0, -50.0, 0.0),
                                       Point::new(-100.0, -50.0, 100.0),
                                       Material::mirror())));
    objects.push(Box::new(Polygon::new(Point::new(0.0, -50.0, 0.0),
                                       Point::new(-100.0, -50.0, 0.0),
                                       Point::new(-100.0, -50.0, 100.0),
                                       Material::mirror())));
    objects.push(Box::new(Polygon::new(Point::new(100.0, -50.0, 100.0),
                                       Point::new(100.0, -50.0, 0.0),
                                       Point::new(0.0, -50.0, 100.0),
                                       Material::matte_grey())));
    objects.push(Box::new(Polygon::new(Point::new(100.0, -50.0, 0.0),
                                       Point::new(0.0, -50.0, 0.0),
                                       Point::new(0.0, -50.0, 100.0),
                                       Material::matte_grey())));

    // attenuation: { distance: 500, moderation: 0.5 }
    let mut lights: Vec<Box<Light>> = vec![];
    lights.push(Box::new(SphereLight::new(Point::new(-60.0, 100.0, -80.0), 30.0, Colour::new(0.8, 0.8, 0.8))));
    lights.push(Box::new(PointLight::new(Point::new(60.0, 80.0, -80.0), Colour::new(0.2, 0.2, 0.2), None)));

    Scene {
        objects: objects,
        lights: lights,
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

struct Rendered {
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
            (&mut *self.data.get())[offset] = min(colour.r * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 1] = min(colour.g * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 2] = min(colour.b * 255.0, 255.0) as u8;
            (&mut *self.data.get())[offset + 3] = 255;
        }
    }
}

fn render(mut scene: Scene) -> Arc<Rendered> {
    let result = Arc::new(Rendered::new(800, 800));

    world_transform(&mut scene);

    scene.render(result.clone());

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
        *l.from() -= from;
    }
    scene.eye.at -= from;
    // Should be (0, 0, 0);
    scene.eye.from -= from;

    // Rotate the world so that eye.at is on the z-axis (i.e., eye.at.x == eye.at.y == 0).
    // We first rotate around the y-axis until we are aligned with the z-axis,
    // then around the z-axis until we are on the x/z plane.
    // Compute the rotation matrix.
    let hyp_y = (scene.eye.at.x * scene.eye.at.x + scene.eye.at.z * scene.eye.at.z).sqrt();
    let sin_y = scene.eye.at.x / hyp_y;
    let cos_y = scene.eye.at.z / hyp_y;
    let rot_y_matrix = [[cos_y, 0.0, sin_y],
                        [0.0, 1.0, 0.0],
                        [-sin_y, 0.0, cos_y]];
    scene.transform(&rot_y_matrix);

    let hyp_x = (scene.eye.at.y * scene.eye.at.y + scene.eye.at.z * scene.eye.at.z).sqrt();
    let sin_x = scene.eye.at.y / hyp_x;
    let cos_x = scene.eye.at.z / hyp_x;
    let rot_x_matrix = [[1.0, 0.0, 0.0],
                        [0.0, cos_x, sin_x],
                        [0.0, -sin_x, cos_x]];
    scene.transform(&rot_x_matrix);

    // println!("post-transform: {:?}, {:?}, {:?}",
    //          scene.lights[0].from(),
    //          scene.eye.from,
    //          scene.eye.at);

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

            let ambient = material.ambient * scene.ambient_light;
            result += ambient;

            let reflect_vec = ray.direction - intersection.normal * dot(ray.direction, intersection.normal) * 2.0;
            let reflect_ray = Ray::new(intersection.point, reflect_vec);
            let reflected = trace(reflect_ray, depth + 1, scene);
            result += material.reflected * reflected;

            for light in &scene.lights {
                // Only compute specular illumination for primary rays.
                let view_vec = if depth == 0 {
                    Some(ray.direction)
                } else {
                    None
                };
                result += light.illuminate(scene, intersection.point, intersection.normal, material.clone(), view_vec);
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
    fn render(self, dest: Arc<Rendered>) {
        // We must translate and scale the pixel on to the image plane.
        let (width, height) = {
            (dest.width, dest.height)
        };

        let trans_x = width as f32 / 2.0;
        let trans_y = height as f32 / 2.0;
        let scale_x = self.eye.width / width as f32;
        let scale_y = -self.eye.height / height as f32;

        let sub_const = (SUPER_SAMPLES * 2) as f32;
        let sub_pixel_x = scale_x / sub_const;
        let sub_pixel_y = scale_y / sub_const;

        let this = Arc::new(self);

        // TODO could be an atomic, rather than a mutex
        let running_count = Arc::new(Mutex::new(THREADS));

        for t in 0..THREADS {
            let this = this.clone();
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
                                // Note that due to the world transform, eye.from is the origin.
                                let p = Point::new(xx, yy, this.eye.length).normalise();
                                let ray = Ray::new(this.eye.from, p);
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

    fn transform(&mut self, m: &Matrix) {
        for o in &mut self.objects {
            o.transform(m);
        }
        for l in &mut self.lights {
            *l.from() = l.from().post_mult(m);
        }
        self.eye.at = self.eye.at.post_mult(m);
    }
}

#[derive(Debug, Clone, new)]
pub struct Ray {
    origin: Point,
    direction: Point,
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

pub struct Scene {
    objects: Vec<Box<Object>>,
    lights: Vec<Box<Light>>,
    ambient_light: Colour,
    eye: Eye,
    background: Colour,
}

struct Eye {
    from: Point,
    at: Point,
    length: f32,
    width: f32,
    height: f32,
}

fn run(file_name: &str) {
    let scene = init();

    let t = Instant::now();

    let data = render(scene);

    let t = t.elapsed();

    let file = File::create(file_name).unwrap();
    let encoder = PNGEncoder::new(file);
    unsafe {
        encoder.encode(&*data.data.get(), data.width, data.height, ColorType::RGBA(8)).unwrap();
    }

    println!("Time: {}s", t.as_secs() as f32 + t.subsec_nanos() as f32 / 1_000_000_000.0);
}

fn main() {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Requires a single argument - the name of the file to emit");
        return;
    }

    run(&args[1])
}
