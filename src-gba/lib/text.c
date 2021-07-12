#include "text.h"
#include "../out/font.h"
#include "graphics.h"
#include <stdint.h>
#include <string.h>

#define font_string_length 94

const char *font_string = " !\"#$%&'()*+,-./"
                          "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
                          "abcdefghijklmnopqrstuvwxyz{|}~";

int fontl_cached[font_string_length] = {0};
int fontr_cached[font_string_length] = {0};

uint8_t font_color_index[3] = {0};

uint8_t find_fontl_fontr(int index, int *fontl, int *fontr) {
  int font_index = 0;
  while (*fontr < font_indexed_width) {
    (*fontr)++;
    if (font_indexed[*fontr] == 1) {
      font_index++;
      if (font_index == index)
        *fontl = *fontr;
      if (font_index == index + 1)
        return 1;
    }
  }
  return 0;
}

void fill_fontl_fontr_cache() {
  for (int i = 0; i < font_string_length; i++)
    find_fontl_fontr(i, &fontl_cached[i], &fontr_cached[i]);
}

int find_font_index(char curr) {
  int index = -1;
  while (++index < strlen(font_string))
    if (font_string[index] == curr)
      return index;
  return -1;
}

int print_char(volatile uint16_t *buffer, char curr, int x, int y) {
  int index = find_font_index(curr);
  if (index < 0)
    return 0;

  int fontl = fontl_cached[index];
  int fontr = fontr_cached[index];

  for (int fy = 1; fy < font_indexed_height; fy++) {
    for (int fx = fontl; fx < fontr; fx++) {
      uint8_t pixel = font_indexed[fy * font_indexed_width + fx];
      uint8_t indexed_color = font_color_index[pixel];
      put_pixel(buffer, y + fy - 1, x + fx - fontl, indexed_color);
    }
  }

  return fontr - fontl;
}

int measure_text(const char *text) {
  if (!fontr_cached[0])
    fill_fontl_fontr_cache();

  int result = 0;
  for (int i = 0; i < strlen(text); i++) {
    char curr = text[i];
    int index = find_font_index(curr);
    if (index < 0)
      continue;

    int fontl = fontl_cached[index];
    int fontr = fontr_cached[index];
    result += fontr - fontl;
  }
  return result;
}

void print_text(volatile uint16_t *buffer, const char *text, int x, int y) {
  if (!fontr_cached[0])
    fill_fontl_fontr_cache();

  for (int i = 0; i < strlen(text); i++) {
    char curr = text[i];
    int curr_width = print_char(buffer, curr, x, y);
    x += curr_width;
  }
}

void print_text_centered(volatile uint16_t *buffer, const char *text, int x,
                         int y) {
  int w = measure_text(text);
  print_text(buffer, text, x - w / 2, y);
}

void setup_font_palette() {
  for (int i = 0; i < font_palette_size; i++)
    font_color_index[i] = add_color_16(font_palette[i]);
}
