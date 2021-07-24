CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -I out -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall -Werror

ART_FILES := $(filter-out art/font.png, $(wildcard art/*.png))

IMAGES_HEADERS := $(patsubst art/%.png,out/art/%.h,$(ART_FILES))
IMAGES_OBJECTS := $(patsubst art/%.png,out/art/%.o,$(ART_FILES))

LIB_HEADERS := $(wildcard src-gba/lib/*.h)
LIB_OBJECTS := $(patsubst src-gba/lib/%.c,out/%.o,$(wildcard src-gba/lib/*.c))

SMOL_IMAGES := $(patsubst art/%.png,out/art/%.png,$(ART_FILES))

OBJS := out/crt0.o out/game.o out/logic.o out/font.o $(LIB_OBJECTS) $(IMAGES_OBJECTS)


.PHONY: all
all: out/game.gba $(SMOL_IMAGES)


out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@


out/game.elf: $(OBJS)
	$(CC) -o $@ $^ -Tsrc-gba/script.ld -nostartfiles -lm


out/%.o: src-gba/%.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<


out/%.o: src-gba/%.s
	mkdir -p out
	$(AS) -o $@ $<


out/%.o: src-gba/lib/%.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<


out/font.c out/font.h: out/font.ppm out/dump_font
	./out/dump_font < $<


.PRECIOUS: out/art/%.c out/art/%.h
out/art/%.c out/art/%.h: out/art/%.ppm out/dump_ppm
	mkdir -p out/art
	./out/dump_ppm $* < $< 


.PRECIOUS: out/font.ppm
out/font.ppm: art/font.png
	mkdir -p out/art
	convert $^ -compress none $@


.PRECIOUS: out/art/%.ppm
out/art/%.ppm: art/%.png
	mkdir -p out/art
	convert $< -resize 240x160 -dither FloydSteinberg -colors 125 -compress none $@


.PRECIOUS: out/art/%.png
out/art/%.png: out/art/%.ppm
	convert $^ $@


.PRECIOUS: out/%
out/%: src-gba/%.c
	mkdir -p out
	gcc -o $@ $^


.PHONY: clean
clean:
	rm -rf out elm-stuff

# Automatic dependencies
.PRECIOUS: %.d
%.d: %.c Makefile
	$(CC) -MM -MT"$@ $(@:.d=.o)" -MF$@ $(CFLAGS) $<

out/%.d: src-gba/%.c Makefile
	$(CC) -MM -MT"$@ $(@:.d=.o)" -MF$@ $(CFLAGS) $<

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
-include $(OBJS:.o=.d)
endif
endif
