# deback

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/deback)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/deback)
![GitHub](https://img.shields.io/github/license/telostat/deback)

> **Note:** This software is under development and of prototype quality at the
> moment. Expect significant breaking changes without notification until we
> reach the first minor version. Until then, we will keep bumping the patch
> version.

Backups are hard, but we need to do it right for business continuity
and sanity.

`deback` is an opinionated file synchronisation and backup tool using various
cool tools under the hood.

## Tools Used

`deback` is using following tools:

- [Nix Package Manager](https://nixos.org/): To package the `deback` suite.
- [smartmontools](https://www.smartmontools.org/): To control and monitor
  storage systems.
- [Cryptsetup and LUKS](https://gitlab.com/cryptsetup/cryptsetup): For
  open-source disk encryption.
- [Rclone](https://rclone.org/): For syncing a remote to a local or remote
  directory.
- [rrclone](https://github.com/telostat/rrclone): Rclone convenience wrapper for
  running multiple tasks defined in a YAML/JSON file.
- [Restic](https://restic.net/): For incremental backups with encryption
- [Autorestic](https://github.com/cupcakearmy/autorestic/): For automating
  Restic process.

In addition to above, following can be used as helper tools:

- `cron` for scheduling backups.
- [autossh](https://www.harding.motd.ca/autossh/): To keep SSH tunnels alive.

## Installation

Assuming that you are on Nix:

```sh
nix-env -i -f https://github.com/telostat/deback/archive/<branch-or-tag>.tar.gz
```

## Usage

See the *help* provided along with the `deback` CLI tool:

```console
$ deback --help
deback - Some File Synchronisation and Backup Tool v0.0.4

Usage: deback [--version] COMMAND
  See available commands.

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  list-disks               Lists disks. WARNING: Note that this command may
                           output unreliable information. USB enclosures and
                           hubs may shadow real serial numbers, or indicate
                           non-unique serial numbers for listed devices.
                           Therefore, do not use this command in automated
                           scripts. Also, running this command without super
                           user privileges may result in errors or incomplete
                           results.
  init-disk                Encrypts and formats a disk. Note that you need to
                           have super user privileges to run this command.
  mount-disk               Mounts a disk. Note that you need to have super user
                           privileges to run this command.
  unmount-disk             Unmounts a disk. Note that you need to have super
                           user privileges to run this command.
  sync                     Runs the sync process. Note that you most probably do
                           not want to be super user when running this command.
  backup-check             Checks the backup configuration. Note that you most
                           probably do not want to be super user when running
                           this command.
  backup-info              Displays backup plan. Note that you most probably do
                           not want to be super user when running this command.
  backup-run               Runs the backup process. Note that you most probably
                           do not want to be super user when running this
                           command.
  backup-snapshots         Shows backup snapshots. Note that you most probably
                           do not want to be super user when running this
                           command.
  sync-and-backup          Runs synchronisation and backup processes
                           respectively. Note that you most probably do not want
                           to be super user when running this command.
```

Most notably:

- `deback sync` consumes a configuration file (`config.sync.yaml`) and runs
`rrclone` on this configuration file.
- `deback backup-run` consumes a configuration file (`config.backup.yaml`) and runs
`autorestic` on this configuration file.
- `deback sync-and-backup` consumes two configuration files (`config.sync.yaml`
and `config.backup.yaml`), and first synchronizes files, and then, runs the
backup process.

## Testing

There are two configuration files in this repository:

1. [config.sync.test.yaml](./config.sync.test.yaml) for running:

    ```sh
    deback sync --config ./config.sync.test.yaml
    ```

2. [config.backup.test.yaml](./config.backup.test.yaml) for running:

    ```sh
    deback backup-run --config ./config.backup.test.yaml
    ```

These configuration files assume that we are syncing and backing up local test
files as in:

```sh
rm -Rf /tmp/deback/testing

mkdir -p /tmp/deback/testing/{sync,backup}
mkdir -p /tmp/deback/testing/sync/sources/{source1,source2}
mkdir -p /tmp/deback/testing/sync/targets/{target1,target2}
mkdir -p /tmp/deback/testing/backup/{backup1,backup2}

for _i in $(seq 1 3); do
  echo "My number is ${_i}" > "/tmp/deback/testing/sync/sources/source1/source1_file${_i}.txt"
  echo "My number is ${_i}" > "/tmp/deback/testing/sync/sources/source2/source2_file${_i}.txt"
done
```

The directory structure should look like this:

```console
$ tree /tmp/deback/testing/
/tmp/deback/testing/
├── backup
│   ├── backup1
│   └── backup2
└── sync
    ├── sources
    │   ├── source1
    │   │   ├── source1_file1.txt
    │   │   ├── source1_file2.txt
    │   │   └── source1_file3.txt
    │   └── source2
    │       ├── source2_file1.txt
    │       ├── source2_file2.txt
    │       └── source2_file3.txt
    └── targets
        ├── target1
        └── target2

10 directories, 6 files
```

Now, we can run the sync:

```console
$ deback sync --config config.sync.test.yaml
RRCLONE>> [2022-08-26 02:56:11] Running task: Sync source1 to target1
2022/08/26 10:56:11 NOTICE: Config file "/home/vst/.config/rclone/rclone.conf" not found - using defaults
2022/08/26 10:56:11 INFO  : source1_file2.txt: Copied (new)
2022/08/26 10:56:11 INFO  : source1_file1.txt: Copied (new)
2022/08/26 10:56:11 INFO  : source1_file3.txt: Copied (new)
2022/08/26 10:56:11 NOTICE:
Transferred:             45 B / 45 B, 100%, 0 B/s, ETA -
Transferred:            3 / 3, 100%
Elapsed time:         0.0s

RRCLONE>> [2022-08-26 02:56:11] Tasks finished successfully in 0 second(s).
RRCLONE>> [2022-08-26 02:56:11] Running task: Sync source2 to target2
2022/08/26 10:56:11 NOTICE: Config file "/home/vst/.config/rclone/rclone.conf" not found - using defaults
2022/08/26 10:56:11 INFO  : source2_file1.txt: Copied (new)
2022/08/26 10:56:11 INFO  : source2_file2.txt: Copied (new)
2022/08/26 10:56:11 INFO  : source2_file3.txt: Copied (new)
2022/08/26 10:56:11 NOTICE:
Transferred:             45 B / 45 B, 100%, 0 B/s, ETA -
Transferred:            3 / 3, 100%
Elapsed time:         0.0s

RRCLONE>> [2022-08-26 02:56:11] Tasks finished successfully in 0 second(s).
```

And the new directory structure should look like this:

```console
$ tree /tmp/deback/testing/
/tmp/deback/testing/
├── backup
│   ├── backup1
│   └── backup2
└── sync
    ├── sources
    │   ├── source1
    │   │   ├── source1_file1.txt
    │   │   ├── source1_file2.txt
    │   │   └── source1_file3.txt
    │   └── source2
    │       ├── source2_file1.txt
    │       ├── source2_file2.txt
    │       └── source2_file3.txt
    └── targets
        ├── target1
        │   ├── source1_file1.txt
        │   ├── source1_file2.txt
        │   └── source1_file3.txt
        └── target2
            ├── source2_file1.txt
            ├── source2_file2.txt
            └── source2_file3.txt

10 directories, 12 files
```

Now, we can check the backup configuration file:

```console
$ deback backup-check --config config.backup.test.yaml
[... TRUNCATED ...]
Everything is fine.
```

..., show backup plan:

```console
$ deback backup-info --config config.backup.test.yaml
[... LOTS OF OUTPUT ...]
```

..., run backups:

```console
$ deback backup-run --config config.backup.test.yaml
[... LOTS OF OUTPUT ...]
```

..., and finally, check backup snapshots:

```console
$ deback backup-snapshots --config config.backup.test.yaml
[... LOTS OF OUTPUT ...]
```

You can test the "first synchronize and then backup" process:

```console
$ deback sync-and-backup --config-sync config.sync.test.yaml --config-backup config.backup.test.yaml
[... LOTS OF OUTPUT ...]
```

## Notes

### Rclone + SFTP + Superuser Access

We are making use of SFTP backend when using Rclone. This creates a
problem when Rclone encounters a file or directory on the source which
lacks required read permissions for the user we are SFTPing.

If this user has `sudo` privileges, we can instruct Rclone to use a
custom SFTP command via `server_command` configuration parameter in
Rclone configuration for the remote, as in:

```sh
[remote]
type = sftp
host = some-hostname-or-ip-address
user = ubuntu
server_command = sudo /usr/lib/openssh/sftp-server -R
```

Note that the `server_command` may be different depending on the
remote operating system distribution. Also note that, we are passing
`-R` to ensure we are not accidentally modifying or deleting anything
on the source.

### Rclone + SFTP - SSH Agent

If there is no ssh-agent available during the invocation of `rclone`
(`rrclone`), then you can add `key_file` to the backend variables:

```yaml
tasks:
  - name: "Mirror Some Remote Server"
    source:
      backend:
        type: "sftp"
        vars:
          host: "127.127.127.127"
          user: "user"
          port: "22"
          key_file: "/home/user/.ssh/id_ed25519"
```

### File Metadata

Note that Rclone does not maintain ownership and permissions. This is
a desired feature for Rclone. We may wish it was not the case, but it
also help us with weird ownership and permission issues.

### SSH, keep alive

When we need SSH tunnel:

```sh
autossh -M 20000 -f -N -L2222:protected-machine:90 public-machine
```

## Development

This program is developed in Haskell on Nix(OS). All Haskell dependencies are
pinned to the `nixpkgs` snapshot specified inside this repository (see
[./nix](./nix)).

Development takes place in a Nix shell ([./shell.nix](./shell.nix)):

```sh
nix-shell
```

Static builds can be obtained via:

```sh
nix-build --arg doStatic true
file result/bin/deback
```

In this case, all runtime dependencies ([rclone](https://rclone.org/),
[rrclone](https://github.com/telostat/rrclone),
[cryptsetup](https://gitlab.com/cryptsetup/cryptsetup),
[smartmontools](https://www.smartmontools.org/) etc.) must be installed on the
host and be on the `PATH` when running `deback`.

If you are on Nix, install the application (all its runtime dependencies will be
automatically installed) via:

```sh
nix-env -f default.nix -i
```

...or:

```sh
nix-env -f https://github.com/telostat/deback/archive/main.tar.gz -i
```

## Releasing

Below is the release process. Run these under the `nix-shell`, but `nix-shell
--pure` won't work for now due to missing `nix-build` command (We will attend it
later).

```sh
git checkout develop
git pull
git checkout main
git pull
git merge --no-ff develop
bash release.sh -n <NEXT-TAG>
git checkout develop
git rebase main
git push
```
