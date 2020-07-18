let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "shakebook"; version = "0.11.0.0"; };
      license = "MIT";
      copyright = "2020 Daniel Firth";
      maintainer = "dan.firth@homotopic.tech";
      author = "Daniel Firth";
      homepage = "";
      url = "";
      synopsis = "Shake-based technical documentation generator; HTML & PDF";
      description = "Shakebook is a documentation generator aimed at covering all the bases for mathematical, technical and scientific diagrams and typesetting. Shakebook provides combinators for taking markdown files and combining them into documents, but allowing the user to control how. Shakebook provides general combinators for templating single pages, cofree comonads for representing tables of contents, and zipper comonads for representing pagers.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."aeson-better-errors" or (buildDepError "aeson-better-errors"))
          (hsPkgs."aeson-with" or (buildDepError "aeson-with"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary-instances" or (buildDepError "binary-instances"))
          (hsPkgs."comonad" or (buildDepError "comonad"))
          (hsPkgs."comonad-extras" or (buildDepError "comonad-extras"))
          (hsPkgs."composite-aeson" or (buildDepError "composite-aeson"))
          (hsPkgs."composite-base" or (buildDepError "composite-base"))
          (hsPkgs."doctemplates" or (buildDepError "doctemplates"))
          (hsPkgs."feed" or (buildDepError "feed"))
          (hsPkgs."free" or (buildDepError "free"))
          (hsPkgs."hashable-time" or (buildDepError "hashable-time"))
          (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
          (hsPkgs."ixset-typed" or (buildDepError "ixset-typed"))
          (hsPkgs."ixset-typed-conversions" or (buildDepError "ixset-typed-conversions"))
          (hsPkgs."lens" or (buildDepError "lens"))
          (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
          (hsPkgs."lucid" or (buildDepError "lucid"))
          (hsPkgs."lucid-cdn" or (buildDepError "lucid-cdn"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."mustache" or (buildDepError "mustache"))
          (hsPkgs."pandoc" or (buildDepError "pandoc"))
          (hsPkgs."pandoc-types" or (buildDepError "pandoc-types"))
          (hsPkgs."path" or (buildDepError "path"))
          (hsPkgs."path-extensions" or (buildDepError "path-extensions"))
          (hsPkgs."rio" or (buildDepError "rio"))
          (hsPkgs."shake-plus" or (buildDepError "shake-plus"))
          (hsPkgs."sitemap-gen" or (buildDepError "sitemap-gen"))
          (hsPkgs."slick" or (buildDepError "slick"))
          (hsPkgs."split" or (buildDepError "split"))
          (hsPkgs."text-time" or (buildDepError "text-time"))
          (hsPkgs."vinyl" or (buildDepError "vinyl"))
          (hsPkgs."within" or (buildDepError "within"))
          (hsPkgs."zipper-extra" or (buildDepError "zipper-extra"))
          ];
        buildable = true;
        };
      tests = {
        "shakebook-test" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-better-errors" or (buildDepError "aeson-better-errors"))
            (hsPkgs."aeson-with" or (buildDepError "aeson-with"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary-instances" or (buildDepError "binary-instances"))
            (hsPkgs."comonad" or (buildDepError "comonad"))
            (hsPkgs."comonad-extras" or (buildDepError "comonad-extras"))
            (hsPkgs."composite-aeson" or (buildDepError "composite-aeson"))
            (hsPkgs."composite-base" or (buildDepError "composite-base"))
            (hsPkgs."doctemplates" or (buildDepError "doctemplates"))
            (hsPkgs."feed" or (buildDepError "feed"))
            (hsPkgs."free" or (buildDepError "free"))
            (hsPkgs."hashable-time" or (buildDepError "hashable-time"))
            (hsPkgs."http-conduit" or (buildDepError "http-conduit"))
            (hsPkgs."ixset-typed" or (buildDepError "ixset-typed"))
            (hsPkgs."ixset-typed-conversions" or (buildDepError "ixset-typed-conversions"))
            (hsPkgs."lens" or (buildDepError "lens"))
            (hsPkgs."lens-aeson" or (buildDepError "lens-aeson"))
            (hsPkgs."lucid" or (buildDepError "lucid"))
            (hsPkgs."lucid-cdn" or (buildDepError "lucid-cdn"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."mustache" or (buildDepError "mustache"))
            (hsPkgs."pandoc" or (buildDepError "pandoc"))
            (hsPkgs."pandoc-types" or (buildDepError "pandoc-types"))
            (hsPkgs."path" or (buildDepError "path"))
            (hsPkgs."path-extensions" or (buildDepError "path-extensions"))
            (hsPkgs."rio" or (buildDepError "rio"))
            (hsPkgs."shake-plus" or (buildDepError "shake-plus"))
            (hsPkgs."shakebook" or (buildDepError "shakebook"))
            (hsPkgs."sitemap-gen" or (buildDepError "sitemap-gen"))
            (hsPkgs."slick" or (buildDepError "slick"))
            (hsPkgs."split" or (buildDepError "split"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (buildDepError "tasty-golden"))
            (hsPkgs."text-time" or (buildDepError "text-time"))
            (hsPkgs."vinyl" or (buildDepError "vinyl"))
            (hsPkgs."within" or (buildDepError "within"))
            (hsPkgs."zipper-extra" or (buildDepError "zipper-extra"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }) // {
    cabal-generator = "hpack";
    }