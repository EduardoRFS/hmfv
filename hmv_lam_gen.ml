open Parsing

(* This implements an HM similar to the one described by Oleg at
   https://okmij.org/ftp/ML/generalization.html

   With one main difference, the level is indicated by a reference
   allowing O(1) generalization.

   It generalizes on lambda and implements basic value restriction *)

(* level is represented by a reference to a weight
   the weight indicate the direction to go when unifying *)
type level = int ref [@@deriving show]

let generic_level_weight = 100000000
let current_level = Stack.create ()

(* prepare stack *)
let reset_level () =
  Stack.clear current_level;
  Stack.push (ref 1) current_level

let () = reset_level ()
let current_level_weight () = !(Stack.top current_level)

(* Due to all types carrying the leven and the level being a reference
   generalizing all of the types in the level is only a matter of
   mutating it to the generic_level_weight *)
let generalize level = level := generic_level_weight

let enter_level () =
  let level_weight = current_level_weight () + 1 in
  Stack.push (ref level_weight) current_level

let leave_level () =
  let current_level = Stack.pop current_level in
  (* auto generalize when leaving *)
  generalize current_level

let current_level () = Stack.top current_level
let level_weight level = !level

(* types logic *)
type typ = { mutable desc : desc; mutable level : level }
and desc = T_arrow of typ * typ | T_var | T_link of typ [@@deriving show]

let make_typ desc level = { desc; level }
let make_arrow p r level = make_typ (T_arrow (p, r)) level
let make_var level = make_typ T_var level
let new_arrow p r = make_arrow p r (current_level ())
let new_var () = make_var (current_level ())

let rec repr typ =
  (* TODO: path compression *)
  match typ.desc with T_link typ -> repr typ | _ -> typ

let is_generic typ = level_weight typ.level = generic_level_weight

(* instantiation copies and weaken top level forall type *)
let instance typ =
  let typ = repr typ in
  let subst = ref [] in
  let levels = ref [ (typ.level, current_level ()) ] in
  let rec instance typ =
    let typ = repr typ in
    if is_generic typ then instance_uniq_typ typ else typ
  and instance_uniq_typ typ =
    match List.assq_opt typ !subst with
    | Some typ' -> typ'
    | None ->
        let typ' = instance_uniq_level typ in
        subst := (typ, typ') :: !subst;
        typ'
  and instance_uniq_level typ =
    let level = typ.level in
    match List.assq_opt level !levels with
    | Some level' -> instance_desc level' typ
    | None ->
        let level' = ref generic_level_weight in
        levels := (level, level') :: !levels;
        instance_desc level' typ
  and instance_desc level typ =
    match typ.desc with
    | T_var -> make_var level
    | T_arrow (p, r) ->
        let p = instance p in
        let r = instance r in
        make_arrow p r level
    | T_link _ -> assert false
  in

  instance typ

let rec occurs ~var typ =
  let typ = repr typ in
  if typ == var then failwith "occurs check";

  (* the weight of a composite type is the max weight of its components,
     so if it's lower than the var.level that means it doesn't include var *)
  if level_weight typ.level >= level_weight var.level then (
    typ.level <- var.level;
    occurs_desc ~var typ)

and occurs_desc ~var typ =
  match typ.desc with
  | T_var -> ()
  | T_arrow (p, r) ->
      occurs ~var p;
      occurs ~var r
  | T_link _ -> assert false

let rec unify t1 t2 =
  let t1 = repr t1 in
  let t2 = repr t2 in
  if t1 == t2 then () else unify_desc t1 t2

and unify_desc l r =
  match (l.desc, r.desc) with
  | T_var, _ -> unify_var ~var:l r
  | _, T_var -> unify_var ~var:r l
  | T_arrow (pl, rl), T_arrow (pr, rr) ->
      unify pl pr;
      unify rl rr
  | T_link _, _ | _, T_link _ -> assert false

and unify_var ~var typ =
  occurs ~var typ;
  var.desc <- T_link typ

let funmatch typ =
  let typ = repr typ in
  match typ.desc with
  | T_var ->
      let ty_par = new_var () in
      let ty_res = new_var () in
      unify typ (new_arrow ty_par ty_res);
      (ty_par, ty_res)
  | T_arrow (ty_par, ty_res) -> (ty_par, ty_res)
  | T_link _ -> assert false

let rec typeof env expr =
  match expr with
  | Pexp_var x -> List.assoc x env
  | Pexp_lambda (x, None, e) ->
      enter_level ();
      let ty_x = new_var () in
      let ty_e = typeof ((x, ty_x) :: env) e in
      let ty = new_arrow ty_x ty_e in
      leave_level ();
      ty
  | Pexp_apply (e1, e2) ->
      let ty_fun = instance (typeof env e1) in
      let ty_par, ty_res = funmatch ty_fun in
      let ty_arg = instance (typeof env e2) in
      unify ty_par ty_arg;
      ty_res
  | Pexp_let (x, e1, e2) ->
      let ty_e1 = typeof env e1 in
      typeof ((x, ty_e1) :: env) e2
  | Pexp_lambda (_, Some _typ, _) -> failwith "unsupported"

let _raw_pp_typ = pp_typ

let pp_typ () =
  let next_var = ref 0 in
  let vars = ref [] in
  let rec pp_typ fmt typ =
    let open Format in
    let typ = repr typ in
    match typ.desc with
    | T_var -> (
        if not (is_generic typ) then fprintf fmt "_";
        match List.assq_opt typ !vars with
        | Some name -> fprintf fmt "%s" name
        | None ->
            let name = Format.sprintf "x%d" !next_var in
            incr next_var;
            vars := (typ, name) :: !vars;
            fprintf fmt "%s" name)
    | T_arrow (p, r) ->
        let p = repr p in
        if p.desc <> T_var then fprintf fmt "(%a) -> %a" pp_typ p pp_typ r
        else fprintf fmt "%a -> %a" pp_typ p pp_typ r
    | T_link _ -> assert false
  in
  pp_typ

let print_typ code =
  reset_level ();
  let expr = Option.get (pexp_from_string code) in
  let ty = typeof [] expr in
  Format.printf "%a\n%!" (pp_typ ()) ty

let () = Format.printf "hmv_lam_gen\n%!"
let () = print_typ {|lambda x. x|}
let () = print_typ {|let id x = x in id|}
let () = print_typ {|let id x = x in let id_id = id id in id_id|}

let () =
  print_typ
    {|let id x = x in
      let id_id = id id in
      let _ = id_id id in
      id_id|}

let () = print_typ {|let apply f v = f v in apply|}
let () = print_typ {|let apply f v = f v in apply (lambda x. x)|}
let () = print_typ {|let sequence _ x = x in sequence (lambda x. x)|}
