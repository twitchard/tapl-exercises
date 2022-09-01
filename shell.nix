{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; with pkgs.ocamlPackages; [
    ocaml
    dune_2
    findlib
    ocaml-lsp
    merlin
    ocamlformat
    ocamlformat-rpc-lib
    utop
    janeStreet.ppx_expect
  ];
}

# let
#   ocaml = pkgs.ocaml;
#   opam2nix = import ./opam2nix.nix {};
#   selection = opam2nix.build {
#     inherit ocaml;
#     selection = ./opam-selection.nix;
#     src = ./.;
#   };
# in selection.experiment
