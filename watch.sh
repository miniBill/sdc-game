#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i bash

make out/image.gba

inotifywait -m -e close_write *.* Makefile --format "%e %w" | while read event file; do 
    echo ""
    echo ""
    echo ""
    echo "File $file changed: $event"
    make out/image.gba
done
