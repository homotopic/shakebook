{
  extras = hackage:
    {
      packages = {
        "commonmark" = (((hackage.commonmark)."0.1.0.2").revisions).default;
        "commonmark-extensions" = (((hackage.commonmark-extensions)."0.2.0.1").revisions).default;
        "commonmark-pandoc" = (((hackage.commonmark-pandoc)."0.2.0.0").revisions).default;
        "compdoc" = (((hackage.compdoc)."0.1.0.0").revisions).default;
        "composite-aeson-cofree-list" = (((hackage.composite-aeson-cofree-list)."0.1.0.0").revisions).default;
        "composite-aeson-throw" = (((hackage.composite-aeson-throw)."0.1.0.0").revisions).default;
        "composite-aeson-writeonly" = (((hackage.composite-aeson-writeonly)."0.1.0.0").revisions).default;
        "pandoc" = (((hackage.pandoc)."2.10.1").revisions).default;
        "pandoc-throw" = (((hackage.pandoc-throw)."0.1.0.0").revisions).default;
        "path-utils" = (((hackage.path-utils)."0.1.0.0").revisions).default;
        "shake-plus-extended" = (((hackage.shake-plus-extended)."0.4.1.0").revisions).default;
        "slick" = (((hackage.slick)."1.0.1.1").revisions).default;
        shakebook = ./shakebook.nix;
        };
      };
  resolver = "nightly-2020-09-19";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    {
      packages = {
        "$locals" = { package = { ghcOptions = "-fwrite-ide-info"; }; };
        };
      }
    ];
  }