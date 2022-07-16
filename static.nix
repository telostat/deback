{ compiler ? "ghc922"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get deback:
  deback = haskell.callCabal2nixWithOptions "deback" ./. "--no-haddock" { };

  ## Override stuff:
  staticDeback = pkgs.haskell.lib.compose.overrideCabal
    (old: {
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--ghc-option=-optl=-pthread"
        "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
        "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
        "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
      ];
    })
    deback;
in
staticDeback
