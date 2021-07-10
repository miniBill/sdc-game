CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall -Werror

.PHONY: all
all: out/example.gba out/main.gba out/image.gba

out/image.o: out/orla.h
out/image.elf: out/crt0.o out/utils.o out/sys.o out/image.o out/orla.o

out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@

out/%.elf: out/crt0.o out/utils.o out/sys.o out/%.o
	$(CC) -o out/$*.elf $^ -Tscript.ld -nostartfiles -lm

out/%.o: %.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

out/%.ppm: art/%.png
	convert $^ -compress none $@

.PRECIOUS: out/%.c out/%.h
out/%.c out/%.h: out/%.ppm out/dump_ppm
	./out/dump_ppm $* < $<

out/dump_ppm: dump_ppm.c
	mkdir -p out
	gcc -o $@ $^

out/%.o: %.s
	mkdir -p out
	$(AS) -o $@ $<

.PHONY: clean
clean:
	rm -rf out
