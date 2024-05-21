def S(x):
    return lambda y: lambda z: x(z)(y(z))

if __name__ == "__main__":
    # Test the S combinator
    result = S(lambda x: x * 2)(3)(4)
    print(result)