# deback

Backups are hard, but we need to do it right for business continuity
and sanity.

`deback` is an opinionated backup and archive tool using various cool tools
under the hood.

## Tools Used

- [Rclone](https://rclone.org/): For syncing a remote to a local or remote
  directory.
- [Restic](https://restic.net/): For incremental backups with encryption
- [rrclone](https://github.com/telostat/rrclone): Rclone convenience wrapper for
  running multiple tasks defined in a YAML/JSON file.
- `cron` for scheduling backups.
- [autossh](https://www.harding.motd.ca/autossh/): To keep SSH tunnels alive.

## The Idea

1. Generate Rclone configuration entries for remotes.
2. Define Rclone sync commands and Restic repositories.
3. Run Rclone sync on a regular basis.
4. Also run Restic immediately after Rclone sync is performed.

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

Static builds can be obtained using [./static.nix](./static.nix):

```sh
nix-build static.nix
file result/bin/deback
```

In this case, all runtime dependencies ([rclone](https://rclone.org/),
[rrclone](https://github.com/telostat/rrclone),
[cryptsetup](https://gitlab.com/cryptsetup/cryptsetup),
[smartmontools](https://www.smartmontools.org/) etc.) must be installed on the
host and be on the `PATH` when running `deback`.

If you are on Nix, use [./default.nix](./default.nix) to install the application
(all its runtime dependencies will be automatically installed) via:

```sh
nix-env -f default.nix -i
```

...or:

```sh
nix-env -f https://github.com/telostat/deback/archive/main.tar.gz -i
```
