<a name="unreleased"></a>
## [Unreleased]


<a name="v0.0.3"></a>
## [v0.0.3] - 2022-07-18
### Chore
- update package metadata, add LICENSE
- gitignore result/
- **build:** improve release artifact
- **build:** parameterize default.nix for static builds
- **build:** use callCabal2nix* instead of deback.nix
- **docs:** update README

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


[Unreleased]: https://github.com/telostat/deback/compare/v0.0.3...HEAD
[v0.0.3]: https://github.com/telostat/deback/compare/v0.0.2...v0.0.3
[v0.0.2]: https://github.com/telostat/deback/compare/v0.0.1...v0.0.2
