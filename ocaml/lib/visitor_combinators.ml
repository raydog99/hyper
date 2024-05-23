module VisitorCombinators = struct

  type ('a, 'b) independent_comp = 'a * 'b

  let independent_before obj (vis1, vis2) =
    (vis1#before obj, vis2#before obj)

  let independent_after obj (vis1, vis2) =
    (vis1#after obj (fst vis1), vis2#after obj (snd vis2))

  type ('a, 'b) threaded_comp = 'a * 'b

  let threaded_before obj (vis1, vis2) =
    let v1 = vis1#before obj in
    let v2 = vis2#before obj (v1#export_visitor) in
    (v1, v2)

  let threaded_after obj (vis1, vis2) =
    let v1 = vis1#after obj (fst vis1) in
    let v2 = vis2#after obj (snd vis2) in
    (v1, v2)

  let threaded_export_visitor (vis1, vis2) =
    vis2#export_visitor

  type ('a, 'b) conditional_comp = 'a * 'b

  let conditional_before obj (vis1, vis2) =
    let v1 = vis1#before obj in
    if v1#continue_visit then
      let v2 = vis2#before obj in
      (v1, v2)
    else
      (v1, vis2)

  let conditional_after obj (vis1, vis2) =
    let v1 = vis1#after obj (fst vis1) in
    if v1#continue_visit then
      let v2 = vis2#after obj (snd vis2) in
      (v1, v2)
    else
      (v1, vis2)

end