open OUnit2
open Efficient_ocaml.S_combinator

let tests = [
  "S combinator test" >:: (fun _ ->
    let add x y = x + y in
    let mul2 x = x * 2 in
    let mul_and_add x = s add mul2 x in
    assert_equal 12 (mul_and_add 3)
  )
]

let () = run_test_tt_main ("S combinator tests" >::: tests)