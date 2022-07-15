{ compiler ? "ghc922"
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

  ## Get deback:
  deback = haskell.callPackage (import ./deback.nix) { };

  ## Get installable deback:
  installableDeback = deback.overrideAttrs (oldAttrs: rec {
    nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [
      pkgs.makeWrapper
    ];

    postFixup = (oldAttrs.postFixup or "") + ''
      wrapProgram $out/bin/deback --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.rclone pkgs.smartmontools ]}
    '';
  });
in
installableDeback
