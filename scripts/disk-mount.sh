#!/usr/bin/env bash

_error() {
    echo "$@" 1>&2
}

_usage() {
    _error "Usage: $0 -d <device> -n <name> [-m <mount-point>]"
    exit 1
}

_device=""
_name=""
_mount=""

while getopts ":d:n:m:" o; do
    case "${o}" in
    d)
        _device="${OPTARG}"
        ;;
    n)
        _name="${OPTARG}"
        ;;
    m)
        _mount="${OPTARG}"
        ;;
    *)
        _usage
        ;;
    esac
done
shift $((OPTIND - 1))

if [ -z "${_device}" ] || [ -z "${_name}" ]; then
    _usage
fi

if [ -z "${_mount}" ]; then
    _mount="/mnt/${_name}"
fi

if [ ! -d "${_mount}" ]; then
    _error "${_mount} is not an existing directory."
    exit 1
fi

## Print info and get confirmation:
ansi --red --bold "Device: ${_device}"
ansi --red --bold "Name  : ${_name}"
ansi --red --bold "Mount : ${_mount}"
echo ""
ansi --cyan --bold --no-newline "To mount, type \"yes\" and hit [Enter] to proceed: "
read -r _SURE
if [ "${_SURE}" = "yes" ]; then
    echo "Proceeding..."
else
    echo "Aborting!"
    exit 0
fi

## Proceed...

ansi --cyan "LUKS opening..."
cryptsetup luksOpen "${_device}" "${_name}"

ansi --cyan "Mounting..."
mount "/dev/mapper/${_name}" "${_mount}"
