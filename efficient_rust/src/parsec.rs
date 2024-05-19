pub fn parser_return<T: Clone>(value: T) -> Result<T, String> {
    Ok(value)
}

pub fn parser_satisfy<T, P>(predicate: P) -> impl Fn(&[T]) -> Result<T, String>
where
    T: Clone,
    P: Fn(&T) -> bool,
{
    move |input: &[T]| {
        if let Some(head) = input.get(0) {
            if predicate(head) {
                Ok(head.clone())
            } else {
                Err("Predicate not satisfied".to_string())
            }
        } else {
            Err("No input".to_string())
        }
    }
}