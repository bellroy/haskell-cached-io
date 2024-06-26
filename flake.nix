{
  description = "A simple library to cache a single IO action with timeout";

  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
  };

  outputs = inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      cabalPackages = [
        {
          name = "cached-io";
          path = ./cached-io.nix;
        }
      ];
      supportedCompilers = [
        "ghc810"
        "ghc90"
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        "ghc910"
      ];
      defaultCompiler = "ghc96";
    };
}
