obj := out/crt0.o out/example.o
bin := out/example.gba

CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

CFLAGS := -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi -std=c11 -pedantic -Wall

.PHONY: all
all: out/example.gba out/main.gba

out/%.gba: out/%.elf
	$(OBJCOPY) -O binary out/$*.elf $@

out/%.elf: out/crt0.o out/%.o
	$(CC) -o out/$*.elf $^ -Tscript.ld -nostartfiles -lm

out/%.o: %.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $^

out/%.o: %.s
	mkdir -p out
	$(AS) -o $@ $<

.PHONY: clean
clean:
	rm -rf out
