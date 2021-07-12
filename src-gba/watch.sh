#! /usr/bin/env nix-shell
#! nix-shell ../shell.nix -i bash

make out/$1.gba

inotifywait -m -e close_write *.* Makefile --format "%e %w" | while read event file; do 
    echo ""
    echo ""
    echo ""
    echo "File $file changed: $event"
    make out/$1.gba
done
