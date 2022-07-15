#!/usr/bin/env bash

_error() {
    echo "$@" 1>&2
}

_usage() {
    _error "Usage: $0 -n <VERSION>"
    exit 1
}

_VERSION_NEXT=""

while getopts ":n:" o; do
    case "${o}" in
    n)
        _VERSION_NEXT="${OPTARG}"
        ;;
    *)
        _usage
        ;;
    esac
done
shift $((OPTIND - 1))


if [ -z "${_VERSION_NEXT}" ]; then
    _usage
fi

## Update version:
sed -i -E "s/^version:([ ]+).*/version:\\1${_VERSION_NEXT}/g" deback.cabal

## Regenerate deback.nix:
cabal2nix --no-haddock . > deback.nix

## Update CHANGELOG.md:
git-chglog -o CHANGELOG.md --next-tag "v${_VERSION_NEXT}"

## Add files:
git add deback.cabal deback.nix CHANGELOG.md

## Commit:
git commit -m "chore(release): v${_VERSION_NEXT}"

## Tag:
git tag -a -m "Release v${_VERSION_NEXT}" "v${_VERSION_NEXT}"

## Push:
git push --follow-tags origin main

## Build application:
nix-build static.nix

## Release
gh release create "v${_VERSION_NEXT}" --generate-notes
gh release upload "v${_VERSION_NEXT}" result/bin/deback
