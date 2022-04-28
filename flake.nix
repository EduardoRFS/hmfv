{
  description = "Nix Flake";

  inputs = {
    nixpkgs.url = "github:anmonteiro/nix-overlays";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:

      let
        pkgs = nixpkgs.legacyPackages."${system}";
        hmfv = pkgs.callPackage ./nix { doCheck = true; };
      in
      rec {
        packages = { inherit hmfv; };
        devShell = import ./nix/shell.nix { inherit pkgs hmfv; };
      });
}
