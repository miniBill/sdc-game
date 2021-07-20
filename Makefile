CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -I out -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall -Werror

IMAGES_HEADERS := $(patsubst art/%.png,out/art/%.h,$(wildcard art/*.png))
IMAGES_OBJECTS := $(patsubst art/%.png,out/art/%.o,$(wildcard art/*.png))

LIB_HEADERS := $(wildcard src-gba/lib/*.h)
LIB_OBJECTS := $(patsubst src-gba/lib/%.c,out/%.o,$(wildcard src-gba/lib/*.c))

PUBLIC_IMAGES := $(patsubst art/%.png,public/art/%.png,$(wildcard art/*.png))

.PHONY: all
all: out/game.gba public/art/list.json

COMMA := ,

public/art/list.json: Makefile $(PUBLIC_IMAGES)
	python3 -c 'import glob, os, json; os.chdir("public/art"); print(json.dumps(glob.glob("*.png")))' > $@

out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@

out/game.elf: out/crt0.o out/game.o out/logic.o $(LIB_OBJECTS) $(IMAGES_OBJECTS)
	$(CC) -o $@ $^ -Tsrc-gba/script.ld -nostartfiles -lm

out/game.o: $(LIB_HEADERS) $(IMAGES_HEADERS)
out/logic.o: $(LIB_HEADERS) $(IMAGES_HEADERS)

out/%.o: src-gba/%.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

out/%.o: src-gba/%.s
	mkdir -p out
	$(AS) -o $@ $<

out/%.o: src-gba/lib/%.c src-gba/lib/%.h
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

.PRECIOUS: out/%.c out/%.h
out/art/%.c out/art/%.h: out/art/%.ppm out/dump_ppm
	mkdir -p out/art
	./out/dump_ppm $* < $< 

.PRECIOUS: out/font.ppm
out/art/font.ppm: art/font.png
	mkdir -p out/art
	convert $^ -compress none $@

.PRECIOUS: out/%.ppm
out/art/%.ppm: art/%.png Makefile
	mkdir -p out/art
	convert $< -resize 240x160 -dither FloydSteinberg -colors 125 -compress none $@

public/art/%.png: out/art/%.ppm
	mkdir -p public/art
	convert $< $@

out/dump_ppm: src-gba/dump_ppm.c
	mkdir -p out
	gcc -o $@ $^

.PHONY: clean
clean:
	rm -rf out public/art elm-stuff
