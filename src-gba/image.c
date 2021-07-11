#include "out/font.h"
#include "out/orla1.h"
#include "out/orla2.h"
#include "out/orla3.h"
#include "out/orla4.h"
#include "out/orla5.h"
#include "out/orla6.h"
#include "out/orla7.h"
#include "utils.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define font_string_length 94

const char *font_string = " !\"#$%&'()*+,-./"
                          "0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
                          "abcdefghijklmnopqrstuvwxyz{|}~";

uint8_t font_color_index[3] = {0};
int fontl_cached[font_string_length] = {0};
int fontr_cached[font_string_length] = {0};

int find_font_index(char curr) {
  int index = -1;
  while (++index < strlen(font_string))
    if (font_string[index] == curr)
      return index;
  return -1;
}

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

uint8_t find_color(const uint16_t *palette, int palette_size, uint16_t col) {
  uint8_t cr = col & 0x1f;
  uint8_t cg = (col >> 5) & 0x1f;
  uint8_t cb = (col >> 10) & 0x1f;
  int best = 0;
  int best_dist = cr + cg + cb;
  for (int i = 1; i < palette_size; i++) {
    uint8_t r = palette[i] & 0x1f;
    uint8_t g = (palette[i] >> 5) & 0x1f;
    uint8_t b = (palette[i] >> 10) & 0x1f;
    int dist = iabs(r - cr) + iabs(g - cg) + iabs(b - cb);
    if (dist < best_dist)
      best = i;
  }
  return best;
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

volatile uint8_t *trash = font_color_index;

void delay(int milliseconds) {
  for (int i = 0; i < 500 * milliseconds; i++)
    trash[0] = 0;
}

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* the buffer we start with */
  volatile uint16_t *buffer = back_buffer;

  wait_vblank();
  buffer = flip_buffers(buffer);

  for (int i = 0; i < font_string_length; i++)
    find_fontl_fontr(i, &fontl_cached[i], &fontr_cached[i]);

  /* loop forever */
  for (int i = 0;; i++) {
    char *sentence;
    const uint16_t *palette;
    const uint8_t *image;
    int palette_size;
    switch (i % 7) {
    case 0:
      sentence = "Hello, this is a test!";
      palette = orla1_palette;
      image = orla1_indexed;
      palette_size = orla1_palette_size;
      break;
    case 1:
      sentence = "I'm testing font rendering";
      palette = orla2_palette;
      image = orla2_indexed;
      palette_size = orla2_palette_size;
      break;
    case 2:
      sentence = "It's apparently working";
      palette = orla3_palette;
      image = orla3_indexed;
      palette_size = orla3_palette_size;
      break;
    case 3:
      sentence = "NOICE!";
      palette = orla4_palette;
      image = orla4_indexed;
      palette_size = orla4_palette_size;
      break;
    case 4:
      sentence = "";
      palette = orla5_palette;
      image = orla5_indexed;
      palette_size = orla5_palette_size;
      break;
    case 5:
      sentence = "";
      palette = orla6_palette;
      image = orla6_indexed;
      palette_size = orla6_palette_size;
      break;
    case 6:
      sentence = "";
      palette = orla7_palette;
      image = orla7_indexed;
      palette_size = orla7_palette_size;
      break;
    default:
      sentence = "";
      palette = orla7_palette;
      image = orla7_indexed;
      palette_size = orla7_palette_size;
      break;
    }

    for (int i = 0; i < font_palette_size; i++)
      font_color_index[i] = find_color(palette, palette_size, font_palette[i]);
    if (font_color_index[1] == font_color_index[2])
      font_color_index[2]++;

    wait_vblank();
    buffer = flip_buffers(buffer);

    reset_palette();
    add_color(0, 0, 0);
    for (int i = 1; i < palette_size; i++)
      add_color_16(palette[i]);
    draw_fullscreen_image(buffer, image);

    if (i < 7)
      print_text_centered(buffer, sentence, WIDTH / 2,
                          HEIGHT - 20 - font_indexed_height);

    wait_vblank();
    buffer = flip_buffers(buffer);

    delay(2000);
  }
}
