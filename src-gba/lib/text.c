#include "text.h"
#include "font.h"
#include "graphics.h"
#include "utils.h"
#include <stdint.h>
#include <string.h>

uint8_t font_color_index[3] = {0};

bool is_printable(char curr) { return curr >= ' ' && curr <= '~'; }

int print_char(volatile uint16_t *buffer, char curr, int x, int y) {
  if (!is_printable(curr))
    return 0;

  int index = curr - ' ';

  int char_width = font_width[index];

  for (int fy = 0; fy < font_height; fy++) {
    for (int fx = 0; fx < char_width; fx++) {
      uint8_t pixel = font_indexed[index][fy * char_width + fx];
      uint8_t indexed_color = font_color_index[pixel];
      put_pixel(buffer, y + fy, x + fx, indexed_color);
    }
  }

  return char_width;
}

int count_lines(const char *text) {
  int lines = text[0] == 0 ? 0 : 1;
  for (int i = 0; i < strlen(text); i++)
    if (text[i] == '\n')
      lines++;
  return lines;
}

int measure_first_line_width(const char *text) {
  int len = strlen(text);
  int current_row_width = 1;
  for (int i = 0; i < len; i++) {
    char curr = text[i];
    if (curr == '\n')
      return current_row_width;

    if (!is_printable(curr))
      continue;

    int index = curr - ' ';

    current_row_width += font_width[index] + 1;
  }
  return current_row_width;
}

void print_text(volatile uint16_t *buffer, const char *text, int x, int y,
                enum Align halign, enum Align valign) {
  int ox = x;
  if (valign != ALIGN_BEGIN) {
    int lines = count_lines(text);
    int height = font_height * lines;
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
      y += font_height;
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

    x += print_char(buffer, curr, x, y);
    x += print_char(buffer, ' ', x, y);
  }
}

void setup_font_palette() {
  for (int i = 0; i < font_palette_size; i++)
    font_color_index[i] = add_color_16(font_palette[i]);
}
