#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_s_combinator() {
        let add = |x, y| x + y;
        let mul2 = |x| x * 2;
        let mul_and_add = |x| s(add, mul2, x);

        assert_eq!(mul_and_add(3), 12);
    }
}