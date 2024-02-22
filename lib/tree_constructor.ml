type node =
  | Element of string * (string * string) list * node list
  | Text of string
  | Comment of string
  | Document of node list

type insertion_mode =
  | Initial
  | BeforeHtml
  | BeforeHead
  | InHead

type tree_context = {
  mutable document: node list;
  mutable stack: node list;
  mutable insertion_mode: insertion_mode;
}

let create_tree_context () =
  {
    document = [];
    stack = [];
    insertion_mode = Initial;
  }

let rec reconstruct_tree context tokens =
  match tokens with
  | [] -> ()
  | token :: rest ->
    match context.insertion_mode with
    | Initial -> initial_mode context token; reconstruct_tree context rest
    | BeforeHtml -> before_html_mode context token; reconstruct_tree context rest
    | BeforeHead -> before_head_mode context token; reconstruct_tree context rest
    | InHead -> in_head_mode context token; reconstruct_tree context rest

and initial_mode context token =
  match token with
  | Doctype (name, public_id, system_id, force_quirks) ->
  ()