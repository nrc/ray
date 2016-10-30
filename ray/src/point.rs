use std::ops::{Mul, Sub};

pub type Matrix = [[f64; 3]; 3];

#[derive(Debug, Clone, Copy, new)]
pub struct Point {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Point {
    pub fn translate(mut self, by: Point) -> Point {
        self.x -= by.x;
        self.y -= by.y;
        self.z -= by.z;

        self
    }

    pub fn add(mut self, other: Point) -> Point {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;

        self
    }

    pub fn post_mult(mut self, matrix: &Matrix) -> Point {
        let new_x = self.x * matrix[0][0] + self.y * matrix[1][0] + self.z * matrix[2][0];
        let new_y = self.x * matrix[0][1] + self.y * matrix[1][1] + self.z * matrix[2][1];
        let new_z = self.x * matrix[0][2] + self.y * matrix[1][2] + self.z * matrix[2][2];

        self.x = new_x;
        self.y = new_y;
        self.z = new_z;

        self
    }

    pub fn mult_scalar(mut self, other: f64) -> Point {
        self.x *= other;
        self.y *= other;
        self.z *= other;

        self
    }

    pub fn normalise(mut self) -> Point {
        let magnitude = self.magnitude();
        self.x /= magnitude;
        self.y /= magnitude;
        self.z /= magnitude;

        self
    }

    pub fn magnitude(&self) -> f64 {
        (self.x.powf(2.0) + self.y.powf(2.0) + self.z.powf(2.0)).sqrt()
    }
}

impl Sub for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Point {
        Point::new(self.x - rhs.x,
                   self.y - rhs.y,
                   self.z - rhs.z)
    }
}

impl Mul<f64> for Point {
    type Output = Point;

    fn mul(self, rhs: f64) -> Point {
        Point::new(self.x * rhs,
                   self.y * rhs,
                   self.z * rhs)
    }
}

pub fn dot(a: Point, b: Point) -> f64 {
    a.x * b.x +
    a.y * b.y +
    a.z * b.z
}

pub fn cross(a: Point, b: Point) -> Point {
    Point::new(a.y * b.z - a.z * b.y,
               a.z * b.x - a.x * b.z,
               a.x * b.y - a.y * b.x)
}
