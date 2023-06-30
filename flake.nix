{
  description = "A simple library to cache a single IO action with timeout";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      rec
      {
        # This becomes the list of package and devShell names.
        supportedCompilers = [
          "ghc88"
          "ghc810"
          "ghc90"
          "ghc92"
          "ghc94"
          "ghc96"
        ];

        packages = builtins.listToAttrs (
          builtins.map
            (compiler: {
              name = compiler;
              value = pkgs.haskell.packages.${compiler}.callPackage ./cached-io.nix { };
            })
            supportedCompilers
        );

        devShells' = (builtins.mapAttrs
          (compiler: v: v.env.overrideAttrs (oldAttrs: {
            buildInputs = oldAttrs.buildInputs
              ++ [ pkgs.nixpkgs-fmt ]
              ++ (with pkgs.haskell.packages.${compiler}; [
              cabal-fmt
              cabal-install
              doctest
              haskell-ci
              haskell-language-server
              hlint
            ]);
          }))
          packages
        );

        devShells = devShells' // { default = devShells'.ghc92; };
      });
}
