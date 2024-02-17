let rec prescan_byte_stream input_byte_stream =
  let rec get_attribute position =
    if position >= String.length input_byte_stream then
      None
    else
      let byte = Char.code input_byte_stream.[position] in
      match byte with
      | 0x09 | 0x0A | 0x0C | 0x0D | 0x20 | 0x2F -> get_attribute (position + 1)
      | 0x3E -> None
      | _ -> Some (byte, position + 1)
  in

  let rec process_byte position =
    if position >= String.length input_byte_stream then
      None
    else
      let byte = Char.code input_byte_stream.[position] in
      match byte with
      | 0x3C | 0x3E | 0x3F ->
          let next_byte = if position + 1 < String.length input_byte_stream then
            Some (Char.code input_byte_stream.[position + 1])
          else
            None
          in
          let rec next_byte_check next_byte =
            match next_byte with
            | Some b when b = 0x09 || b = 0x0A || b = 0x0C || b = 0x0D || b = 0x20 || b = 0x2F
              -> process_byte (position + 1)
            | Some b when b = 0x3C || b = 0x2F -> next_byte_check (Some (Char.code input_byte_stream.[position + 2]))
            | _ -> None
          in
          next_byte_check next_byte
      | _ -> None