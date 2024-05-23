from typing import Any, Callable, Generic, TypeVar

T = TypeVar('T')

class FVisitor(Generic[T]):
    def before(self, obj: Any) -> 'FVisitor[T]':
        raise NotImplementedError

    def after(self, obj: Any, fv: 'FVisitor[T]') -> 'FVisitor[T]':
        raise NotImplementedError

    def export_visitor(self) -> 'FVisitor[T]':
        raise NotImplementedError

    def continue_visit(self) -> bool:
        raise NotImplementedError

class CompReceiver(FVisitor[T]):
    pass

class IndependComp(FVisitor[T]):
    def __init__(self, vis1: FVisitor[T], vis2: FVisitor[T]):
        self.vis1 = vis1
        self.vis2 = vis2

    def before(self, obj: Any) -> 'IndependComp[T]':
        v1 = self.vis1.before(obj)
        v2 = self.vis2.before(obj)
        return IndependComp(v1, v2)

    def after(self, obj: Any, fv: 'IndependComp[T]') -> 'IndependComp[T]':
        v1 = self.vis1.after(obj, fv.vis1)
        v2 = self.vis2.after(obj, fv.vis2)
        return IndependComp(v1, v2)

class ThreadedComp(FVisitor[T]):
    def __init__(self, vis1: FVisitor[T], vis2: CompReceiver[T]):
        self.vis1 = vis1
        self.vis2 = vis2

    def before(self, obj: Any) -> 'ThreadedComp[T]':
        v1 = self.vis1.before(obj)
        v2 = self.vis2.before(obj, self.vis1.export_visitor())
        return ThreadedComp(v1, v2)

    def after(self, obj: Any, fv: 'ThreadedComp[T]') -> 'ThreadedComp[T]':
        v1 = self.vis1.after(obj, fv.vis1)
        v2 = self.vis2.after(obj, fv.vis2)
        return ThreadedComp(v1, v2)

    def export_visitor(self) -> 'FVisitor[T]':
        return self.vis2.export_visitor()

class ConditionalComp(FVisitor[T]):
    def __init__(self, vis1: FVisitor[T], vis2: FVisitor[T]):
        self.vis1 = vis1
        self.vis2 = vis2

    def before(self, obj: Any) -> 'ConditionalComp[T]':
        v1 = self.vis1.before(obj)
        if v1.continue_visit():
            v2 = self.vis2.before(obj)
            return ConditionalComp(v1, v2)
        else:
            return ConditionalComp(v1, self.vis2)

    def after(self, obj: Any, fv: 'ConditionalComp[T]') -> 'ConditionalComp[T]':
        v1 = self.vis1.after(obj, fv.vis1)
        if v1.continue_visit():
            v2 = self.vis2.after(obj, fv.vis2)
            return ConditionalComp(v1, v2)
        else:
            return ConditionalComp(v1, self.vis2)