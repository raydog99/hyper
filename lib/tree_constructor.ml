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
  | Character c when c = '\t' || c = '\n' || c = '\x0C' || c = '\r' || c = ' ' ->
    (* Ignore the token *)
    ()
  | Comment data ->
    context.document <- context.document @ [Comment data]
  | Doctype (name, public_id, system_id, force_quirks) ->
    if name <> "html" || public_id <> "" || (system_id <> "" && system_id <> "about:legacy-compat") then
      (* Parse error *)
      ();
    let doctype_node = DocumentType (name, public_id, system_id) in
    context.document <- context.document @ [doctype_node];
    if not (parser_can_change_mode context token) then
      context.insertion_mode <- BeforeHtml;
    if force_quirks then
      set_quirks_mode context doctype_node
  | _ ->
    if not (parser_can_change_mode context token) then
      set_quirks_mode context context.document;
    context.insertion_mode <- BeforeHtml;
    reconstruct_tree context [token]

and before_html_mode context token =
  ()

and parser_can_change_mode context token =
  context.parser_cannot_change_mode &&
  match token with
  | Doctype (name, _, _, _) ->
    let conditions = [
      "html";
      "-//W3O//DTD W3 HTML Strict 3.0//EN//";
      "-/W3C/DTD HTML 4.0 Transitional/EN";
      "HTML";
      "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd";
      "+//Silmaril//dtd html Pro v0r11 19970101//";
      "-//AS//DTD HTML 3.0 asWedit + extensions//";
    ] in
    List.mem name conditions
  | _ -> false

and set_quirks_mode context node =
  match node with
  | DocumentType _ -> ()
  | Comment _ -> ()
  | Document _ -> context.document <- context.document @ [Comment "<!DOCTYPE html>"]