{ ... }:

let
  ## Import sources:
  sources = import ./nix/sources.nix;

  ## Import nixpkgs:
  pkgs = import sources.nixpkgs {};

  ## Import ansi print helper:
  ansi = import sources.ansi {};

  ## Import rrclone:
  rrclone = import sources.rrclone;

  ## Package name:
  name = "deback";

  ## Package version:
  version = (builtins.readFile ./version.txt);
in
pkgs.stdenv.mkDerivation {
  pname = name;
  version = version;

  src = ./.;

  buildInputs = [
    pkgs.bash
    pkgs.jq
    pkgs.rclone
    pkgs.smartmontools

    ansi
    rrclone
  ];

  nativeBuildInputs = [
    pkgs.makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    ## deback-disk-init
    cp ./scripts/disk-init.sh $out/bin/${name}-disk-init
    chmod +x $out/bin/${name}-disk-init
    wrapProgram $out/bin/${name}-disk-init --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.cryptsetup ansi ]}

    ## deback-disk-list
    cp ./scripts/disk-list.sh $out/bin/${name}-disk-list
    chmod +x $out/bin/${name}-disk-list
    wrapProgram $out/bin/${name}-disk-list --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.smartmontools pkgs.jq ansi ]}

    ## deback-disk-mount
    cp ./scripts/disk-mount.sh $out/bin/${name}-disk-mount
    chmod +x $out/bin/${name}-disk-mount
    wrapProgram $out/bin/${name}-disk-mount --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.cryptsetup ansi ]}

    ## deback-disk-umount
    cp ./scripts/disk-unmount.sh $out/bin/${name}-disk-unmount
    chmod +x $out/bin/${name}-disk-unmount
    wrapProgram $out/bin/${name}-disk-unmount --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.cryptsetup ansi ]}

    ## deback-disk-umount
    cp ./scripts/disk-unmount.sh $out/bin/${name}-disk-unmount
    chmod +x $out/bin/${name}-disk-unmount
    wrapProgram $out/bin/${name}-disk-unmount --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.cryptsetup ansi ]}

    ## deback-sync
    ln -s ${rrclone}/bin/rrclone $out/bin/${name}-sync
    wrapProgram $out/bin/${name}-sync --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.rclone ]}
  '';
}
