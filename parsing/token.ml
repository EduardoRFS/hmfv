type token =
  (* x1 *)
  | VAR of string
  (* -> *)
  | ARROW
  (* lambda *)
  | LAMBDA
  (* . *)
  | DOT
  (* forall *)
  | FORALL
  (* : *)
  | COLON
  (* let *)
  | LET
  (* = *)
  | EQUAL
  (* in *)
  | IN
  (* ( *)
  | LEFT_PARENS
  (* ) *)
  | RIGHT_PARENS
  | EOF
[@@deriving show]
