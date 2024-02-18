let rec prescan_byte_stream input_byte_stream =
  let rec get_attribute position =
    if position >= String.length input_byte_stream then
      None
    else
      let byte = Char.code input_byte_stream.[position] in
      match byte with
      | 0x09 | 0x0A | 0x0C | 0x0D | 0x20 | 0x2F -> get_attribute (position + 1)
      | 0x3E -> None
      | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4A | 0x4B
      | 0x4C | 0x4D | 0x4E | 0x4F | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56
      | 0x57 | 0x58 | 0x59 | 0x5A ->
          Some (byte + 0x20, position + 1)
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
  in

  let rec value position =
    if position >= String.length input_byte_stream then
      None
    else
      let byte = Char.code input_byte_stream.[position] in
      match byte with
      | 0x09 | 0x0A | 0x0C | 0x0D | 0x20 -> value (position + 1)
      | 0x22 | 0x27 ->
          let quote_mark = byte in
          let rec quote_loop position =
            if position >= String.length input_byte_stream then
              None
            else
              let current_byte = Char.code input_byte_stream.[position] in
              if current_byte = quote_mark then
                Some (quote_mark, position + 1)
              else if current_byte >= 0x41 && current_byte <= 0x5A then
                quote_loop (position + 1)
              else
                value (position + 1)
          in
          quote_loop (position + 1)
      | _ ->
          Some (byte, position + 1)
  in

  let rec loop position =
    if position >= String.length input_byte_stream then
      None
    else
      let byte = Char.code input_byte_stream.[position] in
      match byte with
      | 0x3C when position + 5 < String.length input_byte_stream &&
                      Char.code input_byte_stream.[position + 1] = 0x21 &&
                      Char.code input_byte_stream.[position