{
  extras = hackage:
    {
      packages = {
        "aeson-with" = (((hackage.aeson-with)."0.1.1.1").revisions).default;
        "binary-instances" = (((hackage.binary-instances)."1.0.0.1").revisions).default;
        "comonad-extras" = (((hackage.comonad-extras)."4.0.1").revisions).default;
        "ixset-typed-conversions" = (((hackage.ixset-typed-conversions)."0.1.0.0").revisions).default;
        "lucid-cdn" = (((hackage.lucid-cdn)."0.1.1.0").revisions).default;
        "path-extensions" = (((hackage.path-extensions)."0.1.1.0").revisions).default;
        "path-like" = (((hackage.path-like)."0.2.0.1").revisions).default;
        "shake-plus" = (((hackage.shake-plus)."0.2.0.0").revisions).default;
        "slick" = (((hackage.slick)."1.0.1.1").revisions).default;
        "text-time" = (((hackage.text-time)."0.2.0").revisions).default;
        "vinyl" = (((hackage.vinyl)."0.13.0").revisions).default;
        "within" = (((hackage.within)."0.2.0.0").revisions).default;
        "zipper-extra" = (((hackage.zipper-extra)."0.1.3.0").revisions).default;
        shakebook = ./shakebook.nix;
        composite-aeson = ./composite-aeson.nix;
        composite-base = ./composite-base.nix;
        };
      };
  resolver = "lts-16.5";
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