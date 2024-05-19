package efficient

func parserReturn[T any](value T) []T {
    return []T{value}
}

func parserSatisfy[T any](predicate func(T) bool) func([]T) (T, []T) {
    return func(input []T) (T, []T) {
        if len(input) == 0 {
            var defaultValue T
            return defaultValue, input
        }

        if predicate(input[0]) {
            return input[0], input[1:]
        } else {
            var defaultValue T
            return defaultValue, input
        }
    }
}