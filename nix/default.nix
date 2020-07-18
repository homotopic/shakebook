# default.nix
let
  sources = import ./sources.nix;
  haskellNix = import sources.haskell {};
  nixpkgs = import haskellNix.sources.nixpkgs-2003 haskellNix.nixpkgsArgs;

  pkgSet = nixpkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    modules = [];
  };
in
  pkgSet.config.hsPkgs
