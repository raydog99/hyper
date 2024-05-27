from __future__ import annotations
from typing import Generic, TypeVar, Union, List, Tuple

T = TypeVar('T')

# Booleans
Bool = bool

# Natural Numbers
class zero:
    pass

Nat = Union[zero, int]

# Finite Lists
FiniteList = Union[None, List[T]]

# Finite Binary Trees
class leaf:
    pass

class bnode(Generic[T]):
    def __init__(self, value: T, left: BTree[T], right: BTree[T]):
        self.value = value
        self.left = left
        self.right = right

BTree = Union[leaf, bnode[T]]

# Infinite Binary Trees
InfBTree = Union[T, Tuple[InfBTree[T], InfBTree[T]]]

# Infinite Lists
InfList = Union[T, Tuple[T, InfList[T]]]

# Non-empty Trees
NETree = Union[BTree[T], InfBTree[T]]

# Non-empty Lists
NEList = Union[FiniteList[T], InfList[T]]

# CoLists
CoList = Union[T, Tuple[T, CoList[T]]]

# Co-Non-empty Trees
CoNETree = Union[T, Tuple[CoNETree[T], CoNETree[T]]]

# Co-Non-empty Lists
CoNEList = Union[T, Tuple[T, CoNEList[T]]]