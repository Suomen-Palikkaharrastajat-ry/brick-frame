let
  ci =
    { pkgs, ... }:
    let
      hpkgs = pkgs.haskell.packages.ghc96;
      npmTools = pkgs.callPackage ./pkgs/npm-tools.nix { };
      technicPackage = hpkgs.callCabal2nix "technic" ./. { };
      generatorCommand = pkgs.writeShellScriptBin "generator-nix" ''
        exec ${technicPackage}/bin/generator "$@"
      '';
    in
    {
      languages.elm.enable = true;

      languages.haskell.enable = true;
      languages.haskell.package = pkgs.haskell.packages.ghc96.ghc;

      env.NODE_PATH = "${npmTools}/lib/node_modules";
      env.GENERATOR_NIX = "${generatorCommand}/bin/generator-nix";

      packages = [
        generatorCommand
        npmTools
        pkgs.cabal-install
        pkgs.nodejs_22
        hpkgs.hlint
        hpkgs.fourmolu
      ];

      enterShell = ''
        ln -sfn "${npmTools}/lib/node_modules" node_modules
        ln -sfn "${npmTools}/lib/node_modules" elm-app/node_modules
      '';
    };

  shell =
    { pkgs, ... }:
    let
      npmTools = pkgs.callPackage ./pkgs/npm-tools.nix { };
    in
    {
      packages = [
        pkgs.entr
        pkgs.git
        pkgs.nodejs_22
        pkgs.treefmt
        pkgs.elmPackages.elm-review
        pkgs.elmPackages.elm-json
        pkgs.haskell.packages.ghc96.hlint
        pkgs.haskell.packages.ghc96.fourmolu
        npmTools
      ];

      languages.haskell.enable = true;
      languages.haskell.package = pkgs.haskell.packages.ghc96.ghc;
      languages.elm.enable = true;

      dotenv.enable = true;

      env.NODE_PATH = "${npmTools}/lib/node_modules";

      enterShell = ''
        ln -sfn "${npmTools}/lib/node_modules" node_modules
        ln -sfn "${npmTools}/lib/node_modules" elm-app/node_modules

        echo ""
        echo "── technic dev environment ──────────────────────────"
        echo "  GHC:    $(ghc --version)"
        echo "  Cabal:  $(cabal --version | head -1)"
        echo "  Elm:    $(elm --version)"
        echo "  Node:   $(node --version)"
        echo "  Vite:   $(vite --version)"
        echo ""
        echo "  make dev      — generate data + start Vite dev server"
        echo "  make build    — generate data + production build"
        echo "  make dist-ci  — build CI/deploy output in build/"
        echo ""
      '';
    };
in
{
  profiles.shell.module = {
    imports = [ shell ];
  };

  profiles.ci.module = {
    imports = [ ci ];
  };
}
