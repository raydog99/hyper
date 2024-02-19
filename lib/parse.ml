open Bytes

type insertion_mode =
  | Initial
  | BeforeHtml
  | InHead
  | InBody

let rec parse_html bytes =
  let open insertion_mode in
  let open template_insertion_mode in

  let rec process_byte position insertion_mode_stack template_mode_stack =
    if position >= Bytes.length bytes then ()
    else
      let byte = Bytes.get bytes position |> Char.code in
      match byte with
      | 0x3C ->
          let next_byte = if position + 1 < Bytes.length bytes then Some (Bytes.get bytes (position + 1) |> Char.code) else None in
          process_byte (position + 1) new_insertion_mode_stack new_template_mode_stack
      | _ ->
      process_byte (position + 1) insertion_mode_stack template_mode_stack
  and parse_start_tag bytes position =
    ("sample_tag", [])
  and handle_html_element element insertion_mode_stack =
    match insertion_mode_stack with
    | InBody :: _ ->
        handle_in_body element
    | InHead :: _ ->
        handle_in_head element
    | _ -> ()
  and handle_in_body element =
    match element.tag_name with
    | "p" ->
        print_endline ("Handling paragraph in InBody mode")
    | "a" ->
        print_endline ("Handling anchor in InBody mode")
    | _ -> ()
  and handle_in_head element =
    match element.tag_name with
    | "title" ->
        print_endline ("Handling title in InHead mode")
    | "meta" ->
        print_endline ("Handling meta in InHead mode")
    | _ -> ()
  in
  process_byte 0 [InBody] [];