use colour::*;
use point::*;

use rand;
use {min, max, Scene, Ray, intersects, Attenuation};

pub trait Light: Sync + Send {
    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, view_vec: Option<Point>) -> Colour;
    fn from(&mut self) -> &mut Point;
}


#[derive(Debug, Clone, new)]
pub struct PointLight {
    from: Point,
    colour: Colour,
    attenuation: Option<Attenuation>,
}

impl Light for PointLight {
    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, view_vec: Option<Point>) -> Colour {
        let mut result = Colour::black();

        let mut light_vec = self.from - point;

        let distance = light_vec.magnitude();
        let attenuation_factor = self.attenuation_factor(distance);
        if attenuation_factor == 0.0 {
            return result;
        }

        // Normalise
        light_vec *= 1.0 / distance;
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
        let attenuated_colour = self.colour * attenuation_factor;
        let diffuse = material.diffuse * diffuse_dot * attenuated_colour;
        result += diffuse;

        if let Some(view_vec) = view_vec {
            let light_reflect_vec = (normal - light_vec) * dot(light_vec, normal) * 2.0;
            let specular_dot = min(1.0, max(0.0, dot(view_vec * -1.0, light_reflect_vec)));
            let specular = material.specular * specular_dot.powf(material.shininess) * attenuated_colour;
            result += specular;
        }

        result
    }

    fn from(&mut self) -> &mut Point {
        &mut self.from
    }
}

impl PointLight {
    fn attenuation_factor(&self, d: f32) -> f32 {
        match self.attenuation {
            Some(attenuation) => {
                if d >= attenuation.distance {
                    return 0.0;
                }

                let k = attenuation.distance * attenuation.moderation;
                let dk = d / k;
                let dm = d / attenuation.distance;
                let dd = dk / (1.0 - dm * dm) + 1.0;
                1.0 / (dd * dd)
            }
            None => 1.0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SphereLight{
    from: Point,
    radius: f32,
    colour: Colour,
    samples: u8,
}

impl Light for SphereLight {
    fn illuminate(&self, scene: &Scene, point: Point, normal: Point, material: Material, _view_vec: Option<Point>) -> Colour {
        let mut result = Colour::black();

        for _ in 0..self.samples {
            let light_point = self.random_point();
            let light_vec = (light_point - point).normalise();
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
            let diffuse = material.diffuse * (diffuse_dot / self.samples as f32) * self.colour;
            result += diffuse;

            // Could compute a specular component, but seems expensive and maybe inappropriate.
        }

        result
    }

    fn from(&mut self) -> &mut Point {
        &mut self.from
    }
}

impl SphereLight {
    pub fn new(from: Point, radius: f32, colour: Colour) -> SphereLight {
        SphereLight {
            from: from,
            radius: radius,
            colour: colour,
            samples: 16,
        }
    }

    // Return a random point on the surface of the sphere.
    fn random_point(&self) -> Point {
        loop {
            // Compute a random point in an axis-aligned cube at the origin, covering -1 to 1.
            let dx = rand::random::<f32>() * 2.0 - 1.0;
            let dy = rand::random::<f32>() * 2.0 - 1.0;
            let dz = rand::random::<f32>() * 2.0 - 1.0;

            // Check if our random point is in the sphere (happens apx 50% of the time).
            let sum: f32 = dx * dx + dy * dy + dz * dz;
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
