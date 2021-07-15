CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall -Werror

IMAGES_HEADERS := $(patsubst ../art/%.png,out/%.h,$(wildcard ../art/*.png))
IMAGES_OBJECTS := $(patsubst ../art/%.png,out/%.o,$(wildcard ../art/*.png))

LIB_HEADERS := $(wildcard lib/*.h)
LIB_OBJECTS := $(patsubst lib/%.c,out/%.o,$(wildcard lib/*.c))

.PHONY: all
all: out/image.gba

out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@

out/image.elf: out/crt0.o out/image.o $(LIB_OBJECTS) $(IMAGES_OBJECTS)
	$(CC) -o $@ $^ -Tscript.ld -nostartfiles -lm

out/image.o: $(LIB_HEADERS) $(IMAGES_HEADERS)

out/%.o: %.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

out/%.o: %.s
	mkdir -p out
	$(AS) -o $@ $<

out/%.o: lib/%.c lib/%.h
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

.PRECIOUS: out/%.c out/%.h
out/%.c out/%.h: out/%.ppm out/dump_ppm
	./out/dump_ppm $* < $< 

.PRECIOUS: out/%.ppm
out/%.ppm: ../art/%.png
	convert $^ -compress none $@

out/dump_ppm: dump_ppm.c
	mkdir -p out
	gcc -o $@ $^

.PHONY: clean
clean:
	rm -rf out
