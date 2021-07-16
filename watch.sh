#! /usr/bin/env nix-shell
#! nix-shell shell.nix -i bash

make out/game.gba

inotifywait -m -r -e close_write,move src-gba static Makefile --format "%e %w" | while read event file; do 
    echo ""
    echo ""
    echo ""
    echo "File $file changed: $event"
    make out/game.gba
done
 