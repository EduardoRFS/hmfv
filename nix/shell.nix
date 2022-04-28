{ pkgs, hmfv }:

with pkgs; mkShell {
  inputsFrom = [ hmfv ];
  packages = [
    # Make developer life easier
    # formatters
    nixfmt
    ocamlformat
  ] ++ (with ocamlPackages; [
    # OCaml developer tooling
    ocaml
    dune_3
    ocaml-lsp
    ocamlformat-rpc
  ]);
}
