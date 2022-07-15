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

  ## Get our GHC for development:
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
    aeson
    ansi-terminal
    base
    bytestring
    optparse-applicative
    table-layout
    text
    typed-process
  ]);
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
