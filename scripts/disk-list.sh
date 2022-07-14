#!/usr/bin/env bash

ansi --white --bg-red --bold " WARNING: Note that this command may output unreliable information.  " >&2
ansi --white --bg-red --bold " USB enclosures and hubs may shadow real serial numbers, or indicate " >&2
ansi --white --bg-red --bold " non-unique serial numbers for listed devices. Therefore, do not use " >&2
ansi --white --bg-red --bold " this command in automated scripts.                                  " >&2

smartctl --scan --json | jq -r ".devices[]|.name" | while read -r _disk; do
    smartctl --json -i "${_disk}" | jq -c "{disk: \"${_disk}\", model: .model_name, serial: .serial_number}"
done
