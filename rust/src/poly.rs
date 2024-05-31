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

use std::ops::{Add, Mul};

trait Comonoid {
    type ComonoidType;
    fn counit(&self) -> ();
    fn comult(&self) -> (Self::ComonoidType, Self::ComonoidType);
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct PolyTerm<T: Comonoid> {
    coeff: T::ComonoidType,
    exp: u32,
}

impl<T: Comonoid> PolyTerm<T> {
    fn new(coeff: T::ComonoidType, exp: u32) -> Self {
        PolyTerm { coeff, exp }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Poly<T: Comonoid>(Vec<PolyTerm<T>>);

impl<T: Comonoid> Comonoid for Poly<T> {
    type ComonoidType = Poly<T>;

    fn counit(&self) -> () {
        match self.0.first() {
            Some(term) if term.exp == 0 => term.coeff.counit(),
            None => (),
            _ => panic!("Polynomial must be constant to have a counit"),
        }
    }

    fn comult(&self) -> (Poly<T>, Poly<T>) {
        let mut xs = Vec::new();
        let mut ys = Vec::new();
        for term in &self.0 {
            if term.exp == 0 {
                let (c1, c2) = term.coeff.comult();
                xs.push(PolyTerm::new(c1, 0));
                ys.push(PolyTerm::new(c2, 0));
            } else {
                xs.push(term.clone());
                ys.push(PolyTerm::new(term.coeff.counit(), term.exp));
            }
        }
        (Poly(xs), Poly(ys))
    }
}

struct Poly<T> {
    coef: Vec<T>,
    const: T,
}

impl<T> Poly<T>
where
    T: Copy,
{
    fn corolla(&self) -> Poly<T> {
        let mut coef = vec![self.const];
        coef.extend_from_slice(&self.coef);
        Poly { coef, const: T::default() }
    }
}