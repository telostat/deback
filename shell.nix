{ ... }:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Pinned nixpkgs:
  pkgs = import sources.nixpkgs {};

  ## Import ansi print helper:
  ansi = import sources.ansi {};

  ## Import deback:
  deback = import ./default.nix {};
in
pkgs.mkShell {
  buildInputs = [
    ansi
    deback
  ];

  shellHook = ''
    ansi --black --bg-cyan "########################################"
    ansi --black --bg-cyan "## Welcome to Continous Backups Shell ##"
    ansi --black --bg-cyan "########################################"
  '';
}
