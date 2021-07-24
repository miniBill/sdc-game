#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i bash

make -j

inotifywait -m -r -e close_write,move src-gba public Makefile --format "%e %w" | while read event file; do 
    echo ""
    echo ""
    echo ""
    echo "File $file changed: $event"
    make -j
done
