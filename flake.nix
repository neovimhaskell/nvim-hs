{
  description = "nvim-hs";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        haskellPackages = pkgs.haskellPackages;
        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        # Parameter buildTools is used to share most of the boilerplate code
        # between the package and the devShell.
        project = { buildTools ? [ ] }:
          let addBuildTools = (t.flip hl.addBuildTools) buildTools;
          in
          haskellPackages.developPackage {
            root = nixpkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            name = "nvim-hs";
            returnShellEnv = !(buildTools == [ ]);

            modifier = package: t.pipe package [
              addBuildTools
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };
      in
      {
        packages.pkg = project { };
        defaultPackage = self.packages.${system}.pkg;
        devShell = with haskellPackages; project {
          buildTools = [
            cabal-fmt
            cabal-install
            fourmolu
            haskell-language-server
            hlint
          ];
        };
      });
}
