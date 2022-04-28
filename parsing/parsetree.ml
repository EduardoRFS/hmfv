type typ =
  | Ptyp_arrow of typ * typ
  | Ptyp_forall of string * typ
  | Ptyp_var of string
[@@deriving show]

type expr =
  | Pexp_var of string
  | Pexp_lambda of string * typ option * expr
  | Pexp_apply of expr * expr
  | Pexp_let of string * expr * expr
[@@deriving show]
