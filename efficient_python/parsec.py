from typing import Callable, Generic, TypeVar, List

T = TypeVar('T')

def parser_return(value: T) -> List[T]:
    return [value]

def parser_satisfy(predicate: Callable[[T], bool]) -> Callable[[List[T]], (T, List[T])]:
    def satisfy(input: List[T]) -> (T, List[T]):
        if not input:
            return None, input
        
        head, *tail = input
        if predicate(head):
            return head, tail
        else:
            return None, input
    return satisfy