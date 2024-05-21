package main

import (
	"fmt"
	"github.com/raymonddeng99/efficient"
)

func main() {
    fmt.Println(3)
    result := S(3)(4)(func(x interface{}) interface{} {
        return x.(int) * 2
    })
    fmt.Println(3)
}
