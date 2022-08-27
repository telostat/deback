<a name="unreleased"></a>
## [Unreleased]


<a name="v0.0.4"></a>
## [v0.0.4] - 2022-08-27
### Chore
- **dev:** update release script and document release process
- **dev:** update release process

### Docs
- update README

### Feat
- add sync-and-backup command
- add backup commands

### Refactor
- add restic to deback exec path
- revisit CLI option descriptions
- update program header

### Pull Requests
- Merge pull request [#10](https://github.com/telostat/deback/issues/10) from telostat/develop
- Merge pull request [#9](https://github.com/telostat/deback/issues/9) from telostat/vst/issue-8
- Merge pull request [#7](https://github.com/telostat/deback/issues/7) from telostat/vst/update-release-process


<a name="v0.0.3"></a>
## [v0.0.3] - 2022-07-18
### Chore
- gitignore result/
- update package metadata, add LICENSE
- **build:** improve release artifact
- **build:** parameterize default.nix for static builds
- **build:** use callCabal2nix* instead of deback.nix
- **docs:** update README
- **release:** v0.0.3

### Fix
- add rrclone to PATH on Nix installation

### Pull Requests
- Merge pull request [#6](https://github.com/telostat/deback/issues/6) from telostat/get-ready-for-release
- Merge pull request [#5](https://github.com/telostat/deback/issues/5) from telostat/vst/fix-runtime
- Merge pull request [#4](https://github.com/telostat/deback/issues/4) from telostat/vst/revisit-nix-setup


<a name="v0.0.2"></a>
## [v0.0.2] - 2022-07-15
### Chore
- implement custom release process
- **dev:** make Nix shell use deback.nix
- **docs:** update development notes in README
- **release:** v0.0.2

### Feat
- improve CLI information, refactor Deback.Cli module
- finish refactoring the application in Haskell

### Refactor
- apply hlint suggestions
- add sync command to Haskell application (for rrclone)
- finish translating scripts to Haskell
- start translating scripts to Haskell

### Pull Requests
- Merge pull request [#3](https://github.com/telostat/deback/issues/3) from telostat/vst/haskellize


<a name="v0.0.1"></a>
## v0.0.1 - 2022-07-14
### Chore
- release 0.0.1
- integrate release-please
- read version from version.txt file
- **main:** release 0.0.1

### Feat
- initial commit

### Pull Requests
- Merge pull request [#2](https://github.com/telostat/deback/issues/2) from telostat/release-please--branches--main--components--release-please-action


[Unreleased]: https://github.com/telostat/deback/compare/v0.0.4...HEAD
[v0.0.4]: https://github.com/telostat/deback/compare/v0.0.3...v0.0.4
[v0.0.3]: https://github.com/telostat/deback/compare/v0.0.2...v0.0.3
[v0.0.2]: https://github.com/telostat/deback/compare/v0.0.1...v0.0.2
