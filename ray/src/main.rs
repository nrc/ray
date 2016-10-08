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
    objects.push(Object::Sphere(Point(-100, 0, 0), 40, Material::blue_plastic()));
    objects.push(Object::Sphere(Point(30, 0, 0), 20, Material::blue_plastic()));
    // objects.push(new Polygon(new Point(150, -50, 250), new Point(150, -50, -150), new Point(-150, -50, -150), mirror()));

    // attenuation: { distance: 500, moderation: 0.5 }
    lights.push(Light::PointLight(Point(0, 0, 0), Colour(0.7, 0.7, 0.7), null));
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
function world_transform() {
    // Translate the world so that eye.from is at the origin.
    for (let o of objects) {
        o.translate(eye.from);
    }
    for (let l of lights) {
        l.from.translate(eye.from);
    }
    eye.at.translate(eye.from);
    // Should be (0, 0, 0);
    eye.from.translate(eye.from);

    // console.log("translate");
    // console.log(object.center);
    // console.log(light.from);
    // console.log(eye.at);
    // console.log(eye.from);

    // Rotate the world so that eye.at is on the z-axis (i.e., eye.at.x == eye.at.y == 0).
    // Compute the rotation matrix.
    let sqrt_ax_sq_plus_az_sq = Math.sqrt(eye.at.x * eye.at.x + eye.at.z * eye.at.z);
    let sqrt_ax_sq_plus_ay_sq_plus_az_sq = Math.sqrt(eye.at.x * eye.at.x + eye.at.y * eye.at.y + eye.at.z * eye.at.z);

    let sin_tilt = eye.at.y / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let cos_tilt = sqrt_ax_sq_plus_az_sq / sqrt_ax_sq_plus_ay_sq_plus_az_sq;
    let sin_pan = eye.at.x / sqrt_ax_sq_plus_az_sq;
    let cos_pan = eye.at.z / sqrt_ax_sq_plus_az_sq;

    let rot_at_matrix = [[cos_tilt * cos_pan, -sin_tilt, cos_tilt * sin_pan],
                         [sin_tilt * cos_pan, cos_tilt, sin_tilt * sin_pan],
                         [-sin_pan, 0, cos_pan]];

    for (let o of objects) {
        o.transform(rot_at_matrix);
    }
    for (let l of lights) {
        l.from.post_mult(rot_at_matrix);
    }
    eye.at.post_mult(rot_at_matrix);

    // console.log("rotate");
    // console.log(object.center);
    // console.log(light.from);
    // console.log(eye.from);
    // console.log(eye.at);

    // At this stage we are looking directly down the Z-axis from the origin to positive infinity.
}

// Returns a colour.
function trace(ray, depth) {
    if (depth >= RAY_DEPTH) {
        return black();
    }

    let intersection = intersects(ray);
    if (intersection) {
        let material = intersection.object.material;
        let result = black();

        let ambient = mult_colours(material.ambient, ambient_light.colour);
        result.add(ambient);

        let reflect_vec = subtract_points(ray.direction, mult_point_scalar(intersection.normal, dot(ray.direction, intersection.normal) * 2));
        let reflect_ray = new Ray(intersection.point, reflect_vec);
        let reflected = trace(reflect_ray, depth + 1);
        result.add(mult_colours(material.reflected, reflected));

        for (let light of lights) {
            // Only compute specular illumination for primary rays.
            let view_vec = null;
            if (depth == 0) {
                view_vec = ray.direction;
            }
            result.add(light.illuminate(intersection.point, intersection.normal, material, view_vec));
        }

        return result;
    } else {
        return background;
    }
}

function intersects(ray) {
    let results = [];
    for (let o of objects) {
        let i = o.intersects(ray);
        if (i) {
            results.push(i);
        }
    }

    if (results.length == 0) {
        return null;
    }

    results.sort((a, b) => a.t - b.t);

    return results[0];
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

        for (let y = 0; y < height; y += 1) {
            let image_y = (y - trans_y) * scale_y;
            for (let x = 0; x < width; x += 1) {
                let image_x = (x - trans_x) * scale_x;

                // Super-sampling.
                let points = []
                let yy = image_y - (scale_y / 2);
                for (let sy = 0; sy < SUPER_SAMPLES; sy += 1) {
                    let xx = image_x - (scale_x / 2);
                    for (let sx = 0; sx < SUPER_SAMPLES; sx += 1) {
                        points.push(new Point(xx, yy, eye.length))
                        xx += sub_pixel_x;
                    }
                    yy += sub_pixel_y;
                }

                let sum = new Colour(0, 0, 0);
                for (let p of points) {
                    // Note that due to the world transform, eye.from is the origin.
                    let ray = new Ray(eye.from, p.normalise());
                    sum.add(trace(ray, 0));
                }
                setPixel(x, y, data, width, sum.mult_scalar(1 / (SUPER_SAMPLES * SUPER_SAMPLES)));
            }
        }
    }
}

function setPixel(x, y, data, canvas_width, colour) {
    let offset = 4 * (x + y * canvas_width);
    data[offset] = Math.min(colour.r * 255, 255);
    data[offset + 1] = Math.min(colour.g * 255, 255);
    data[offset + 2] = Math.min(colour.b * 255, 255);
    data[offset + 3] = 255;
}

function mult_point_scalar(point, scalar) {
    return new Point(point.x * scalar,
                     point.y * scalar,
                     point.z * scalar);
}

function add_points(a, b) {
    return new Point(a.x + b.x,
                     a.y + b.y,
                     a.z + b.z);
}

function subtract_points(a, b) {
    return new Point(a.x - b.x,
                     a.y - b.y,
                     a.z - b.z);
}

function dot(a, b) {
    return a.x * b.x +
           a.y * b.y +
           a.z * b.z;
}

function cross(a, b) {
    return new Point(a.y * b.z - a.z * b.y,
                     a.z * b.x - a.x * b.z,
                     a.x * b.y - a.y * b.x);
}

function mult_colours(a, b) {
    return new Colour(a.r * b.r,
                      a.g * b.g,
                      a.b * b.b);
}

function mult_colour_scalar(colour, scalar) {
    return new Colour(colour.r * scalar,
                      colour.g * scalar,
                      colour.b * scalar);
}

function add_colours(a, b) {
    return new Colour(a.r + b.r,
                      a.g + b.g,
                      a.b + b.b);
}


function Point(x, y, z) {
    this.x = x;
    this.y = y;
    this.z = z;

    this.translate = function(by) {
        this.x -= by.x;
        this.y -= by.y;
        this.z -= by.z;

        return this;
    };

    this.add = function(other) {
        this.x += other.x;
        this.y += other.y;
        this.z += other.z;

        return this;
    };

    this.post_mult = function(matrix) {
        let new_x = this.x * matrix[0][0] + this.y * matrix[1][0] + this.z * matrix[2][0];
        let new_y = this.x * matrix[0][1] + this.y * matrix[1][1] + this.z * matrix[2][1];
        let new_z = this.x * matrix[0][2] + this.y * matrix[1][2] + this.z * matrix[2][2];

        this.x = new_x;
        this.y = new_y;
        this.z = new_z;

        return this;
    };

    this.mult_scalar = function(other) {
        this.x *= other;
        this.y *= other;
        this.z *= other;

        return this;
    }

    this.normalise = function() {
        let magnitude = this.magnitude();
        this.x /= magnitude;
        this.y /= magnitude;
        this.z /= magnitude;

        return this;
    }

    this.magnitude = function() {
        return Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2) + Math.pow(this.z, 2));
    }
}

function Colour(r, g, b) {
    this.r = r;
    this.g = g;
    this.b = b;

    this.add = function(other) {
        this.r += other.r;
        this.g += other.g;
        this.b += other.b;

        return this;
    }

    this.mult = function(other) {
        this.r *= other.r;
        this.g *= other.g;
        this.b *= other.b;

        return this;
    }

    this.mult_scalar = function(other) {
        this.r *= other;
        this.g *= other;
        this.b *= other;

        return this;
    }
}

function Ray(origin, direction) {
    this.origin = origin;
    this.direction = direction;
}

function Material(diffuse, specular, ambient, reflected, shininess) {
    this.diffuse = diffuse;
    this.specular = specular;
    this.ambient = ambient;
    this.reflected = reflected;
    this.shininess = shininess;
}

function red_plastic() {
    return new Material(red(), grey(), red(), dark_grey(), 8);
}

function blue_plastic() {
    return new Material(blue(), grey(), blue(), dark_grey(), 8);
}

function mirror() {
    return new Material(dark_grey(), grey(), dark_grey(), grey(), 2);
}

function red() {
    return new Colour(0.5, 0, 0);
}

function blue() {
    return new Colour(0, 0, 1);
}

function white() {
    return new Colour(1, 1, 1);
}

function grey() {
    return new Colour(0.75, 0.75, 0.75);
}

function dark_grey() {
    return new Colour(0.25, 0.25, 0.25);
}

function black() {
    return new Colour(0, 0, 0);
}

function Sphere(center, r, material) {
    this.center = center;
    this.radius = r;
    this.material = material;

    this.intersects = function(ray) {
        let rel_center = subtract_points(ray.origin, this.center);

        let l_dot_rel_center = dot(ray.direction, rel_center);
        let rel_center_sq = dot(rel_center, rel_center);
        let under_sqrt = Math.pow(l_dot_rel_center, 2) - rel_center_sq + Math.pow(this.radius, 2);

        if (under_sqrt >= 0) {
            let sqrt = Math.sqrt(under_sqrt);
            let t = -sqrt - l_dot_rel_center;
            if (t <= 0.001) {
                // Intersection is behind the origin of the ray.
                return null;
            }
            let point = mult_point_scalar(ray.direction, t).add(ray.origin);
            let normal = subtract_points(point, this.center).normalise();
            return {
                object: this,
                normal: normal,
                point: point,
                t: t
            };
        } else {
            return null;
        }
    }

    this.translate = function(v) {
        this.center.translate(v);
    }

    // Note that this won't apply scales properly.
    this.transform = function(m) {
        this.center.post_mult(m);
    }
}

function Polygon(p1, p2, p3, material) {
    this.p1 = p1;
    this.p2 = p2;
    this.p3 = p3;
    this.material = material;

    this.compute_normal = function() {
        this.u = subtract_points(p2, p1);
        this.v = subtract_points(p3, p1);
        this.normal = cross(this.u, this.v).normalise();
        this.uv = dot(this.u, this.v);
        this.uu = dot(this.u, this.u);
        this.vv = dot(this.v, this.v);
        this.triangle_denom = this.uv * this.uv - this.uu * this.vv;
    }

    this.intersects = function(ray) {
        if (!this.normal) {
            this.compute_normal();
        }

        // First test if the ray intersects the plane of the polygon.
        let plane_denom = dot(this.normal, ray.direction);
        if (plane_denom <= 0.001 && plane_denom >= -0.001) {
            // Ray is parallel to plane.
            return null;
        }

        let t = dot(this.normal, subtract_points(this.p1, ray.origin)) / plane_denom;
        if (t <= 0.001) {
            // Plane is behind ray's origin.
            return null;
        }
        let point = mult_point_scalar(ray.direction, t).add(ray.origin);

        // Test if the intersection point is within the triangle.
        let w = subtract_points(point, this.p1);
        let wu = dot(w, this.u);
        let wv = dot(w, this.v);

        let r = (this.uv * wv - this.vv * wu) / this.triangle_denom;
        let s = (this.uv * wu - this.uu * wv) / this.triangle_denom;

        if (r <= 0.001 || s <= 0.001 || r + s >= 1) {
            return null;
        }

        return {
            object: this,
            point: point,
            normal: this.normal,
            t: t
        };
    }

    this.translate = function(v) {
        this.p1.translate(v);
        this.p2.translate(v);
        this.p3.translate(v);
    }

    this.transform = function(m) {
        this.p1.post_mult(m);
        this.p2.post_mult(m);
        this.p3.post_mult(m);
    }
}

function PointLight(from, colour, attenuation) {
    this.from = from;
    this.colour = colour;
    this.attenuation = attenuation;

    // If view_vec is null, illuminate will not take account of specular illumination.
    this.illuminate = function(point, normal, material, view_vec) {
        let result = black();

        let light_vec = subtract_points(this.from, point);

        let distance = light_vec.magnitude();
        let attenuation_factor = this.attenuation_factor(distance);
        if (attenuation_factor == 0) {
            return result;
        }

        // Normalise
        light_vec.mult_scalar(1 / distance);
        let light_ray = new Ray(point, light_vec);
        if (intersects(light_ray)) {
            // In shadow.
            return result;
        }

        let diffuse_dot = dot(light_vec, normal);
        if (diffuse_dot <= 0) {
            // Facing away from light.
            return result;
        }
        let attenuated_colour = mult_colour_scalar(this.colour, attenuation_factor);
        let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot).mult(attenuated_colour);
        result.add(diffuse);

        if (view_vec) {
            let light_reflect_vec = subtract_points(normal, light_vec).mult_scalar(dot(light_vec, normal) * 2);
            let specular_dot = Math.min(1, Math.max(0, dot(mult_point_scalar(view_vec, -1), light_reflect_vec)));
            let specular = mult_colour_scalar(material.specular, Math.pow(specular_dot, material.shininess)).mult(attenuated_colour);
            result.add(specular);
        }

        return result;
    }

    this.attenuation_factor = function(d) {
        if (!this.attenuation) {
            return 1;
        }

        if (d >= this.attenuation.distance) {
            return 0;
        }

        let k = this.attenuation.distance * this.attenuation.moderation;
        let dk = d / k;
        let dm = d / this.attenuation.distance;
        return 1 / Math.pow(dk / (1 - Math.pow(dm, 2)) + 1, 2);
    }
}

function SphereLight(from, radius, colour) {
    this.from = from;
    this.radius = radius;
    this.colour = colour;
    this.samples = 16;

    // If view_vec is null, illuminate will not take account of specular illumination.
    this.illuminate = function(point, normal, material, view_vec) {
        let result = black();

        for (let i = 0; i < this.samples; ++i) {
            let light_point = this.random_point();
            let light_vec = subtract_points(light_point, point).normalise();
            let light_ray = new Ray(point, light_vec);
            if (intersects(light_ray)) {
                // In shadow.
                continue;
            }

            let diffuse_dot = dot(light_vec, normal);
            if (diffuse_dot <= 0) {
                // Facing away from light.
                continue;
            }
            let diffuse = mult_colour_scalar(material.diffuse, diffuse_dot / this.samples).mult(this.colour);
            result.add(diffuse);

            // Could compute a specular component, but seems expensive and maybe inappropriate.
        }

        return result;
    }

    // Return a random point on the surface of the sphere.
    this.random_point = function() {
        while (true) {
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
                return new Point(this.from.x + dx * this.radius,
                                 this.from.y + dy * this.radius,
                                 this.from.z + dz * this.radius);
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
