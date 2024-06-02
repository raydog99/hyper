from typing import Sequence, Tuple, Callable, Optional

Operad = Tuple[
    list,                                    # colors
    dict,                                    # operations
    Callable[[Tuple[list, int, list]], Optional[int]],  # composition
    Callable[[object], int]                  # identity
]

def make_operad(
    colors: list,
    operations: list[Tuple[list, object, int]],
    composition: Callable[[Tuple[list, int, list]], Optional[int]],
    identity: Callable[[object], int]
) -> Operad:
    ops = {(profile, color): i for i, (profile, color, _) in enumerate(operations)}
    return colors, ops, composition, identity

def find_operation(operad: Operad, profile: list, color: object) -> Optional[int]:
    _, operations, _, _ = operad
    return operations.get((tuple(profile), color), None)

def compose(operad: Operad, op_index: int, operands: list[int]) -> Optional[int]:
    _, _, composition, _ = operad
    return composition(([], op_index, operands))

def identity_op(operad: Operad, color: object) -> int:
    _, _, _, identity = operad
    return identity(color)