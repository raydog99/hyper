use std::ops::{Add, Mul};

#[derive(Debug, Clone, PartialEq)]
pub struct Poly<T>(Vec<T>);

impl<T> Poly<T>
where
    T: Clone + Default + Add<Output = T> + Mul<Output = T> + Copy,
{
    pub fn new(coeffs: Vec<T>) -> Self {
        Poly(coeffs)
    }

    pub fn eval(&self, x: T) -> T {
        self.0
            .iter()
            .rev()
            .enumerate()
            .fold(T::default(), |acc, (i, &coeff)| acc + coeff * x.pow(i as u32))
    }

    pub fn map<F>(&self, f: F) -> Poly<T>
    where
        F: Fn(T) -> T,
    {
        Poly(self.0.iter().map(|coeff| f(coeff.clone())).collect())
    }
}

impl<T> Add for Poly<T>
where
    T: Clone + Default + Add<Output = T> + Mul<Output = T> + Copy,
{
    type Output = Poly<T>;

    fn add(self, rhs: Poly<T>) -> Poly<T> {
        let n = std::cmp::max(self.0.len(), rhs.0.len());
        let mut coeffs = vec![T::default(); n];
        for (i, coeff) in self.0.iter().enumerate() {
            coeffs[i] = coeffs[i].clone() + coeff.clone();
        }
        for (i, coeff) in rhs.0.iter().enumerate() {
            coeffs[i] = coeffs[i].clone() + coeff.clone();
        }
        Poly(coeffs)
    }
}

impl<T> Mul for Poly<T>
where
    T: Clone + Default + Add<Output = T> + Mul<Output = T> + Copy,
{
    type Output = Poly<T>;

    fn mul(self, rhs: Poly<T>) -> Poly<T> {
        let n = self.0.len() + rhs.0.len() - 1;
        let mut coeffs = vec![T::default(); n];
        for (i, coeff1) in self.0.iter().enumerate() {
            for (j, coeff2) in rhs.0.iter().enumerate() {
                coeffs[i + j] = coeffs[i + j].clone() + coeff1.clone() * coeff2.clone();
            }
        }
        Poly(coeffs)
    }
}