use simd::f32x4;
use std::ops::{Mul, Div, Add, Sub, AddAssign, MulAssign, SubAssign, DivAssign};

pub type Matrix = [[f32; 3]; 3];

#[derive(Debug, Clone, Copy)]
pub struct Point {
    data: f32x4,
}

impl Point {
    pub fn new(x: f32, y: f32, z: f32) -> Point {
        Point {
            data: f32x4::new(x, y, z, 0.0),
        }
    }

    pub fn post_mult(self, matrix: &Matrix) -> Point {
        let new_x = self.x() * matrix[0][0] + self.y() * matrix[1][0] + self.z() * matrix[2][0];
        let new_y = self.x() * matrix[0][1] + self.y() * matrix[1][1] + self.z() * matrix[2][1];
        let new_z = self.x() * matrix[0][2] + self.y() * matrix[1][2] + self.z() * matrix[2][2];

        Point { data: f32x4::new(new_x, new_y, new_z, 1.0) }
    }

    pub fn normalise(self) -> Point {
        Point { data: self.data / f32x4::splat(self.magnitude()) }
    }

    pub fn magnitude(self) -> f32 {
        dot(self, self).sqrt()
    }

    pub fn x(self) -> f32 {
        self.data.extract(0)
    }

    pub fn y(self) -> f32 {
        self.data.extract(1)
    }

    pub fn z(self) -> f32 {
        self.data.extract(2)
    }
}

impl Add for Point {
    type Output = Point;

    fn add(self, rhs: Point) -> Point {
        Point { data: self.data + rhs.data }
    }
}

impl Sub for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Point {
        Point { data: self.data - rhs.data }
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Point) {
        self.data = self.data + rhs.data;
    }
}

impl SubAssign for Point {
    fn sub_assign(&mut self, rhs: Point) {
        self.data = self.data - rhs.data;
    }
}

impl Mul<f32> for Point {
    type Output = Point;

    fn mul(self, rhs: f32) -> Point {
        Point { data: self.data * f32x4::splat(rhs) }
    }
}

impl MulAssign<f32> for Point {
    fn mul_assign(&mut self, rhs: f32) {
        self.data = self.data * f32x4::splat(rhs);
    }
}

impl Div<f32> for Point {
    type Output = Point;

    fn div(self, rhs: f32) -> Point {
        Point { data: self.data / f32x4::splat(rhs) }
    }
}

impl DivAssign<f32> for Point {
    fn div_assign(&mut self, rhs: f32) {
        self.data = self.data / f32x4::splat(rhs);
    }
}

pub fn dot(a: Point, b: Point) -> f32 {
    let ms = a.data * b.data;
    let mut buf = [0.0; 4];
    ms.store(&mut buf, 0);
    buf[0] + buf[1] + buf[2]
}

pub fn cross(a: Point, b: Point) -> Point {
    // TODO could use SIMD shuffles.
    Point::new(a.y() * b.z() - a.z() * b.y(),
               a.z() * b.x() - a.x() * b.z(),
               a.x() * b.y() - a.y() * b.x())
}
