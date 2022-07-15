{ compiler ? "ghc922"
, ...
}:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs { };

  ## Import rrclone:
  rrclone = import sources.rrclone;

  ## Get the haskell set:
  haskell = pkgs.haskell.packages.${compiler};

  ## Get deback:
  deback = haskell.callPackage (import ./deback.nix) { };

  ## Get deback Haskell dependencies:
  debackDeps = pkgs.haskell.lib.compose.getHaskellBuildInputs deback;

  ## Get our GHC for development:
  ghc = haskell.ghcWithPackages (_: debackDeps);
in
pkgs.mkShell {
  buildInputs = [
    ## Fancy stuff:
    pkgs.figlet
    pkgs.lolcat

    ## Haskell stuff:
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskell-language-server
    ghc

    ## Runtime dependencies:
    pkgs.cryptsetup
    pkgs.rclone
    pkgs.smartmontools
    rrclone
  ];

  shellHook = ''
    figlet -w 999 "DEBACK DEV SHELL" | lolcat -S 42
  '';
}
