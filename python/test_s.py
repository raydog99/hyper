from S_combinator import S

if __name__ == "__main__":
    result = S(lambda x: x * 2)(3)(4)
    print(result)