use std::collections::HashMap;

type Operad<'a, T> = (
    Vec<T>,                       // colors
    HashMap<(Vec<T>, T), usize>,  // operations
    Box<dyn Fn((Vec<T>, usize, Vec<usize>)) -> Option<usize>>,  // composition
    Box<dyn Fn(T) -> usize>,      // identity
);

type Seq<'a, T> = Vec<T>;

fn make_operad<'a, T: Eq + Hash + Clone>(
    colors: Vec<T>,
    operations: Vec<(Seq<T>, T, usize)>,
    composition: Box<dyn Fn((Vec<T>, usize, Vec<usize>)) -> Option<usize>>,
    identity: Box<dyn Fn(T) -> usize>,
) -> Operad<'a, T> {
    let mut ops = HashMap::new();
    for (i, (profile, color, _)) in operations.iter().enumerate() {
        ops.insert((profile.clone(), color.clone()), i);
    }
    (colors, ops, composition, identity)
}

fn find_operation<'a, T: Eq + Hash + Clone>(operad: &Operad<'a, T>, profile: Seq<T>, color: T) -> Option<usize> {
    operad.1.get(&(profile, color)).cloned()
}

fn compose<'a, T: Clone>(operad: &Operad<'a, T>, op_index: usize, operands: Vec<usize>) -> Option<usize> {
    (operad.2)((Vec::new(), op_index, operands))
}

fn identity_op<'a, T: Clone>(operad: &Operad<'a, T>, color: T) -> usize {
    (operad.3)(color)
}