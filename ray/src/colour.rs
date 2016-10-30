use std::ops::{Add, Mul};

#[derive(Debug, Clone, Copy, new)]
pub struct Colour {
    pub r: f64,
    pub b: f64,
    pub g: f64,
}

impl Colour {
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

    pub fn add(mut self, other: Colour) -> Colour {
        self.r += other.r;
        self.g += other.g;
        self.b += other.b;

        self
    }

    pub fn mult(mut self, other: Colour) -> Colour {
        self.r *= other.r;
        self.g *= other.g;
        self.b *= other.b;

        self
    }

    pub fn mult_scalar(mut self, other: f64) -> Colour {
        self.r *= other;
        self.g *= other;
        self.b *= other;

        self
    }
}

impl Add for Colour {
    type Output = Colour;

    fn add(self, rhs: Colour) -> Colour {
        Colour::new(self.r + rhs.r,
                    self.g + rhs.g,
                    self.b + rhs.b)
    }
}

impl Mul for Colour {
    type Output = Colour;

    fn mul(self, rhs: Colour) -> Colour {
        Colour::new(self.r * rhs.r,
                    self.g * rhs.g,
                    self.b * rhs.b)
    }
}

impl Mul<f64> for Colour {
    type Output = Colour;

    fn mul(self, rhs: f64) -> Colour {
        Colour::new(self.r * rhs,
                    self.g * rhs,
                    self.b * rhs)
    }
}

#[derive(Debug, Clone, new)]
pub struct Material {
    pub diffuse: Colour,
    pub specular: Colour,
    pub ambient: Colour,
    pub reflected: Colour,
    pub shininess: f64,
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
}
