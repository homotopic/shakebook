{ haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/tarball/40d0528f6d970bd71b181c706a5f9b057ae8a735") {}
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
  compiler-nix-name = "ghc883";
}

