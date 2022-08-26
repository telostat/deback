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
  deback = haskell.callCabal2nixWithOptions "deback" ./. "--no-haddock" { };

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

    ## Release stuff:
    pkgs.busybox
    pkgs.gh
    pkgs.git
    pkgs.git-chglog
    pkgs.upx

    ## Haskell stuff:
    ghc
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.haskell-language-server
    pkgs.haskellPackages.apply-refact
    pkgs.hlint

    ## Runtime dependencies:
    pkgs.autorestic
    pkgs.cryptsetup
    pkgs.rclone
    pkgs.smartmontools
    rrclone
  ];

  shellHook = ''
    figlet -w 999 "DEBACK DEV SHELL" | lolcat -S 42
  '';
}
