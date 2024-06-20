type variable = string
type pattern = string
type offset = int
type kind =
 | Value
 | Length
 | Lines
 | OffsetStart
 | OffsetEnd
 | LineStart
 | LineEnd
 | ColumnStart
 | ColumnEnd
 | FilePath
 | FileName
 | FileDirectory
 | Lowercase
 | Uppercase
 | Capitalize
 | Uncapitalize
 | UpperCamelCase
 | LowerCamelCase
 | UpperSnakeCase
 | LowerSnakeCase
 | External of string

type template_element =
 | Constant of string
 | Hole of { pattern : pattern;
             variable : variable;
             offset : offset;
             kind : kind }

let attribute_to_kind = function
 | "value" -> Value
 | "length" -> Length
 | "lines" -> Lines
 | "offset" -> OffsetStart
 | "offset.start" -> OffsetStart
 | "offset.end" -> OffsetEnd
 | "line" -> LineStart
 | "line.start" -> LineStart
 | "line.end" -> LineEnd
 | "column" -> ColumnStart
 | "column.start" -> ColumnStart
 | "column.end" -> ColumnEnd
 | "file" -> FilePath
 | "file.path" -> FilePath
 | "file.name" -> FileName
 | "file.directory" -> FileDirectory
 | "lowercase" -> Lowercase
 | "UPPERCASE" -> Uppercase
 | "Capitalize" -> Capitalize
 | "uncapitalize" -> Uncapitalize
 | "UpperCamelCase" -> UpperCamelCase
 | "lowerCamelCase" -> LowerCamelCase
 | "UPPER_SNAKE_CASE" -> UpperSnakeCase
 | "lower_snake_case" -> LowerSnakeCase
 | "lsif.hover" -> External "lsif.hover"
 | s -> failwith ("invalid attribute " ^ s)

let camel_to_snake s =
 let rec go n = function
   | [] -> []
   | c :: cs when Char.uppercase_ascii c ->
       '_' :: Char.lowercase_ascii c :: go (n + 1) cs
   | c :: cs ->
       Char.lowercase_ascii c :: go (n + 1) cs
 in
 String.concat "_" (go 0 (String.to_seq s |> List.of_seq))

let substitute_kind filepath (Hole { variable; kind }) env =
 let lookup env var =
   try Some (List.assoc var env) with Not_found -> None
 in
 let matchStart env var =
   lookup env var |> Option.map (fun value -> 0) (* Placeholder implementation *)
 in
 let matchEnd env var =
   lookup env var |> Option.map (fun value -> String.length value) (* Placeholder implementation *)
 in
 let lineStart _filepath env var =
   matchStart env var |> Option.map string_of_int
 in
 let lineEnd _filepath env var =
   matchEnd env var |> Option.map string_of_int
 in
 let columnStart _filepath env var =
   matchStart env var |> Option.map (fun offset -> string_of_int (offset + 1))
 in
 let columnEnd _filepath env var =
   matchEnd env var |> Option.map (fun offset -> string_of_int (offset + 1))
 in
 let fileName = function
   | Some path ->
       let base = Filename.basename path in
       Some base
   | None -> None
 in
 let directory = function
   | Some path ->
       let dir = Filename.dirname path in
       Some dir
   | None -> None
 in
 let offsetToLineColumn _source _offset = (1, 1) (* Placeholder implementation *) in
 let stripVar var =
   let regex = Str.regexp "^\\[(.*?)\\]$" in
   if Str.string_match regex var 0 then
     match Str.matched_group 1 var with
     | v -> Some v
     | exception Not_found -> None
   else
     None
 in
 match kind with
 | Value -> lookup env variable
 | Length -> lookup env variable |> Option.map (fun value -> string_of_int (String.length value))
 | Lines -> lookup env variable |> Option.map (fun value -> string_of_int (1 + List.length (String.split_on_char '\n' value)))
 | OffsetStart -> matchStart env variable |> Option.map string_of_int
 | OffsetEnd -> matchEnd env variable |> Option.map string_of_int
 | LineStart -> lineStart filepath env variable
 | LineEnd -> lineEnd filepath env variable
 | ColumnStart -> columnStart filepath env variable
 | ColumnEnd -> columnEnd filepath env variable
 | FilePath -> filepath
 | FileName -> fileName filepath
 | FileDirectory -> directory filepath
 | Lowercase -> lookup env variable |> Option.map String.lowercase_ascii
 | Uppercase -> lookup env variable |> Option.map String.uppercase_ascii
 | Capitalize -> lookup env variable |> Option.map (fun value -> String.capitalize_ascii value)
 | Uncapitalize -> lookup env variable |> Option.map (fun value -> String.uncapitalize_ascii value)
 | UpperCamelCase -> lookup env variable |> Option.map (fun value -> camel_to_snake value |> String.split_on_char '_' |> List.map String.capitalize_ascii |> String.concat "")
 | LowerCamelCase -> lookup env variable |> Option.map (fun value -> camel_to_snake value |> String.split_on_char '_' |> List.map String.capitalize_ascii |> String.concat "" |> String.uncapitalize_ascii)
 | UpperSnakeCase -> lookup env variable |> Option.map (fun value -> camel_to_snake value |> String.uppercase_ascii)
 | LowerSnakeCase -> lookup env variable |> Option.map camel_to_snake
 | External "lsif.hover" ->
     begin match filepath with
     | Some filepath ->
         lookup env variable
         |> Option.map (fun value ->
                let offset = matchStart env variable |> Option.get in
                let source = "" (* Placeholder implementation *) in
                let line, column = offsetToLineColumn source offset in
                Printf.sprintf "lsif.hover for (%d, %d): %s" line column value)
     | None -> None
     end
 | External _ -> failwith "Unsupported external attribute"

(* Placeholder implementation *)
let readFile _filepath = ""

(* Placeholder implementation *)
let matchRange _env _variable = (0, 0)

type environment = (variable * string) list