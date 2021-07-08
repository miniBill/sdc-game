obj := out/crt0.o out/example.o
bin := out/example.gba

CC := arm-none-eabi-gcc
AS := arm-none-eabi-as
OBJCOPY := arm-none-eabi-objcopy

opt := -O3 -fomit-frame-pointer -marm -mcpu=arm7tdmi

CFLAGS := $(opt) -std=c89 -pedantic -Wall

$(bin): $(obj)
	$(CC) -o out/out.elf $(obj) -Tscript.ld -nostartfiles -lm
	$(OBJCOPY) -O binary out/out.elf $(bin)
	$(RM) out/out.elf

-include $(obj:.o=.d)

%.d: %.c
	@rm -f $@; $(CC) -MM $(CFLAGS) $< > $@

out/%.o: %.c
	mkdir -p out
	$(CC) -c $(CFLAGS) -o $@ $<

out/%.o: %.s
	mkdir -p out
	$(AS) -o $@ $<

.PHONY: clean
clean:
	rm -rf out
