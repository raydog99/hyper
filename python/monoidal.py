class MonoidalCategory:
    def id(self, obj):
        return obj

    def compose(self, f, g):
        return lambda x: f(g(x))

    def tensor(self, a, b):
        return (a, b)

    def tensorMor(self, f, g):
        return lambda x: (f(x[0]), g(x[1]))