#pragma once

#include <stdint.h>

int measure_text(const char *text);

void print_text(volatile uint16_t *buffer, const char *text, int x, int y);

void print_text_centered(volatile uint16_t *buffer, const char *text, int x,
                         int y);

void setup_font_palette();
