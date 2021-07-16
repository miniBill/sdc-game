#pragma once

#include <stdint.h>

enum Align { ALIGN_BEGIN, ALIGN_MIDDLE, ALIGN_END };

int count_lines(const char *text);
int measure_text_width(const char *text);

void print_text(volatile uint16_t *buffer, const char *text, int x, int y,
                enum Align halign, enum Align valign);

void setup_font_palette();
