package efficient

type SFunc func(interface{}) func(interface{}) interface{}

func S(x interface{}) SFunc {
    return func(y interface{}) func(interface{}) interface{} {
        return func(z interface{}) interface{} {
            return x.(func(interface{}) interface{})(z).(func(interface{}) interface{})(y.(func(interface{}) interface{})(z))
        }
    }
}
