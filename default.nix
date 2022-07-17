{ compiler ? "ghc922"
, doStatic ? false
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Import rrclone:
  rrclone = import sources.rrclone;

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Define a function that makes deback statically compiled:
  makeDebackStatic = drv: pkgs.haskell.lib.compose.overrideCabal
    (_: {
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
    drv;

  ## Define a function that makes deback installable in Nix environment with all its dependencies:
  makeDebackInstallable = drv: drv.overrideAttrs (oldAttrs: rec {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
      pkgs.makeWrapper
    ];

    postFixup = (oldAttrs.postFixup or "") + ''
      wrapProgram $out/bin/deback --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.rclone pkgs.smartmontools ]}
    '';
  });

  ## Get raw deback:
  deback = haskell.callCabal2nixWithOptions "deback" ./. "--no-haddock" { };
in
if doStatic then makeDebackStatic deback else makeDebackInstallable deback
