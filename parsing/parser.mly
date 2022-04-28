%{ open Parsetree %}
%token <string> VAR
%token ARROW
%token LAMBDA
%token DOT
%token FORALL
%token COLON
%token LET
%token EQUAL
%token IN
%token LEFT_PARENS
%token RIGHT_PARENS
%token EOF

%start <Parsetree.expr option> pexp_opt
%start <Parsetree.typ option> ptyp_opt

%%

(* ptyp *)
let ptyp_opt :=
  | t = option(ptyp); EOF;
    { t }

let ptyp :=
  | ptyp_arrow

let ptyp_arrow :=
  | ptyp_forall
  | p = ptyp_atom; ARROW; r = ptyp_arrow;
    { Ptyp_arrow (p, r) } 

let ptyp_forall :=
  | ptyp_atom
  | FORALL; x = VAR; DOT; t = ptyp;
    { Ptyp_forall (x, t) }

let ptyp_atom :=
  | x = VAR;
    { Ptyp_var x }
  | LEFT_PARENS; t = ptyp; RIGHT_PARENS;
    { t }

(* pexp *)
let pexp_opt :=
  | t = option(pexp); EOF;
    { t }

let pexp :=
  | pexp_let

let pexp_let :=
  | pexp_lambda
  | LET; x = VAR; ps = list(VAR); EQUAL; e1 = pexp; IN; e2 = pexp;
    { let e1 = List.fold_right (fun p e1 -> Pexp_lambda (p, None, e1)) ps e1 in
      Pexp_let (x, e1, e2) }

let pexp_lambda :=
  | pexp_apply
  | LAMBDA; x = VAR; DOT; e = pexp;
    { Pexp_lambda (x, None, e) }
  | LAMBDA; x = VAR; COLON; t = ptyp; DOT; e = pexp;
    { Pexp_lambda (x, Some t, e) }

let pexp_apply :=
  | pexp_atom
  | e1 = pexp_apply; e2 = pexp_atom;
    { Pexp_apply (e1, e2) }

let pexp_atom :=
  | x = VAR;
    { Pexp_var x }
  | LEFT_PARENS; e = pexp; RIGHT_PARENS;
    { e }
