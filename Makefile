CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -I out -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall -Werror

IMAGES_HEADERS := $(patsubst static/art/%.png,out/%.h,$(wildcard static/art/*.png))
IMAGES_OBJECTS := $(patsubst static/art/%.png,out/%.o,$(wildcard static/art/*.png))

LIB_HEADERS := $(wildcard src-gba/lib/*.h)
LIB_OBJECTS := $(patsubst src-gba/lib/%.c,out/%.o,$(wildcard src-gba/lib/*.c))

.PHONY: all
all: out/game.gba

out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@

out/game.elf: out/crt0.o out/game.o out/logic.o $(LIB_OBJECTS) $(IMAGES_OBJECTS)
	$(CC) -o $@ $^ -Tsrc-gba/script.ld -nostartfiles -lm

out/game.o: $(LIB_HEADERS) $(IMAGES_HEADERS)

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
out/%.c out/%.h: out/%.ppm out/dump_ppm
	./out/dump_ppm $* < $< 

.PRECIOUS: out/%.ppm
out/%.ppm: static/art/%.png
	convert $^ -compress none $@

out/dump_ppm: src-gba/dump_ppm.c
	mkdir -p out
	gcc -o $@ $^

.PHONY: clean
clean:
	rm -rf out
