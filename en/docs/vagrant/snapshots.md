---
title: "Snapshots"
slug: "snapshots"
draft: false
images: []
weight: 9924
type: docs
toc: true
---

## Take a snapshot
    vagrant snapshot save mysnapshot

## Bash script to delete all snapshots
    #!/bin/bash

    NO_SNAPSHOTS="No snapshots have been taken yet"
    SNAPSHOT_OUTPUT=$(vagrant snapshot list | grep "${NO_SNAPSHOTS}")

    if [ -z "${SNAPSHOT_OUTPUT}" ]; then
        echo "Found some snapshots, going to remove them"
        for SNAPSHOT in $(vagrant snapshot list); do
            vagrant snapshot delete "${SNAPSHOT}"
        done
    else
        echo "No snapshots found"
    fi


## Restore a snapshot
    vagrant snapshot restore mysnapshot

## List snapshots
    vagrant snapshot list

## Restore a snapshot without provisioning the box
    vagrant snapshot restore --no-provision mysnapshot

## Remove a snapshot
    vagrant snapshot delete mysnapshot

