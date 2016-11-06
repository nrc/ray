use simd::f32x4;

use std::ops::{Add, Mul, AddAssign, MulAssign};

#[derive(Debug, Clone, Copy)]
pub struct Colour {
    data: f32x4,
}

impl Colour {
    pub fn new(r: f32, g: f32, b: f32) -> Colour {
        Colour {
            data: f32x4::new(r, g, b, 0.0),
        }
    }

    pub fn r(&self) -> f32 {
        self.data.extract(0)
    }

    pub fn g(&self) -> f32 {
        self.data.extract(1)
    }

    pub fn b(&self) -> f32 {
        self.data.extract(2)
    }

    pub fn red() -> Colour {
        Colour::new(0.5, 0.0, 0.0)
    }

    pub fn blue() -> Colour {
        Colour::new(0.0, 0.0, 1.0)
    }

    pub fn white() -> Colour {
        Colour::new(1.0, 1.0, 1.0)
    }

    pub fn grey() -> Colour {
        Colour::new(0.75, 0.75, 0.75)
    }

    pub fn dark_grey() -> Colour {
        Colour::new(0.25, 0.25, 0.25)
    }

    pub fn black() -> Colour {
        Colour::new(0.0, 0.0, 0.0)
    }
}

impl Add for Colour {
    type Output = Colour;

    fn add(self, rhs: Colour) -> Colour {
        Colour{ data: self.data + rhs.data }
    }
}

impl AddAssign for Colour {
    fn add_assign(&mut self, rhs: Colour) {
        self.data = self.data + rhs.data;
    }
}

impl Mul for Colour {
    type Output = Colour;

    fn mul(self, rhs: Colour) -> Colour {
        Colour { data: self.data * rhs.data }
    }
}

impl Mul<f32> for Colour {
    type Output = Colour;

    fn mul(self, rhs: f32) -> Colour {
        Colour { data: self.data * f32x4::splat(rhs) }
    }
}

impl MulAssign for Colour {
    fn mul_assign(&mut self, rhs: Colour) {
        self.data = self.data * rhs.data;
    }
}

impl MulAssign<f32> for Colour {
    fn mul_assign(&mut self, rhs: f32) {
        self.data = self.data * f32x4::splat(rhs);
    }
}

#[derive(Debug, Clone, new)]
pub struct Material {
    pub diffuse: Colour,
    pub specular: Colour,
    pub ambient: Colour,
    pub reflected: Colour,
    pub shininess: f32,
}

impl Material {
    pub fn red_plastic() -> Material {
        Material::new(Colour::red(), Colour::grey(), Colour::red(), Colour::dark_grey(), 8.0)
    }

    pub fn blue_plastic() -> Material {
        Material::new(Colour::blue(), Colour::grey(), Colour::blue(), Colour::dark_grey(), 8.0)
    }

    pub fn mirror() -> Material {
        Material::new(Colour::dark_grey(), Colour::grey(), Colour::dark_grey(), Colour::grey(), 2.0)
    }

    pub fn matte_grey() -> Material {
        Material::new(Colour::dark_grey(), Colour::black(), Colour::black(), Colour::dark_grey(), 2.0)
    }
}
