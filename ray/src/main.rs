#![feature(proc_macro)]

extern crate image;
extern crate rand;
#[macro_use]
extern crate derive_new;

use image::ColorType;
use image::png::PNGEncoder;

use std::cmp::Ordering;
use std::env;
use std::fs::File;

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
    lights.push(Light::Point(PointLight::new(Point::new(0.0, 40.0, -100.0), Colour::new(0.7, 0.7, 0.7), None)));
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
        *l.from() -= from;
    }
    scene.eye.at -= from;
    // Should be (0, 0, 0);
    scene.eye.from -= from;

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

    // println!("post-translation: {:?}, {:?}, {:?}",
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
                    sum += trace(ray, 0, self);
                }
                dest.set_pixel(x, y, sum * (1.0 / (SUPER_SAMPLES * SUPER_SAMPLES) as f64));
            }
        }
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

#[derive(Debug, Clone, Copy, new)]
pub struct Attenuation {
    distance: f64,
    moderation: f64,
}

pub struct Scene {
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
