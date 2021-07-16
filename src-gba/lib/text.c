#include "text.h"
#include "../out/font.h"
#include "graphics.h"
#include "utils.h"
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

int count_lines(const char *text) {
  int lines = text[0] == 0 ? 0 : 1;
  for (int i = 0; i < strlen(text); i++)
    if (text[i] == '\n')
      lines++;
  return lines;
}

int measure_first_line_width(const char *text) {
  if (!fontr_cached[0])
    fill_fontl_fontr_cache();

  int len = strlen(text);
  int current_row_width = 1;
  for (int i = 0; i < len; i++) {
    char curr = text[i];
    if (curr == '\n')
      return current_row_width;

    int index = find_font_index(curr);
    if (index < 0)
      continue;

    int fontl = fontl_cached[index];
    int fontr = fontr_cached[index];
    current_row_width += fontr - fontl;
  }
  return current_row_width;
}

void print_text(volatile uint16_t *buffer, const char *text, int x, int y,
                enum Align halign, enum Align valign) {
  if (!fontr_cached[0])
    fill_fontl_fontr_cache();

  int ox = x;
  if (halign != ALIGN_BEGIN || y != ALIGN_BEGIN) {
    int lines = count_lines(text);
    int height = (font_indexed_height - 1) * lines;
    switch (valign) {
    case ALIGN_BEGIN:
      break;
    case ALIGN_MIDDLE:
      y -= height / 2;
      break;
    case ALIGN_END:
      y -= height;
      break;
    }
  }

  bool start_of_line = true;

  for (int i = 0;; i++) {
    char curr = text[i];
    if (!curr)
      break;

    if (curr == '\n') {
      start_of_line = true;
      y += font_indexed_height - 1;
      x = ox;
      continue;
    }

    if (start_of_line) {
      start_of_line = false;

      int width = measure_first_line_width(text + i);
      switch (halign) {
      case ALIGN_BEGIN:
        break;
      case ALIGN_MIDDLE:
        x -= width / 2;
        break;
      case ALIGN_END:
        x -= width;
        break;
      }

      print_char(buffer, ' ', x, y);
      x++;
    }

    int curr_width = print_char(buffer, curr, x, y);
    x += curr_width;
  }
}

void setup_font_palette() {
  for (int i = 0; i < font_palette_size; i++)
    font_color_index[i] = add_color_16(font_palette[i]);
}
