#!/usr/bin/env bash

set -e

echo "This will permanently delete all numbered run directories:"
echo
ls -d [0-9][0-9] 2>/dev/null || {
    echo "No numbered directories found. Nothing to delete."
    exit 0
}

echo

for d in [0-9][0-9]; do
    if [ -d "$d" ]; then
        echo "Deleting $d"
        rm -rf "$d"
    fi
done

echo "Cleanup complete."
