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
