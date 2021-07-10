#include "out/orla.h"
#include "utils.h"
#include <stdint.h>
#include <stdlib.h>

void print_text(volatile uint16_t *buffer, const char *text, int x, int y) {}

float hue(uint16_t color) {
  uint8_t r = color & 0x1F;
  uint8_t g = (color >> 5) & 0x1F;
  uint8_t b = (color >> 10) & 0x1F;
  uint8_t max = u8max(r, u8max(g, b));
  uint8_t min = u8min(r, u8min(g, b));

  float hue;
  if (r == max)
    hue = (float)(g - b) / (float)(max - min);
  else if (g == max)
    hue = 2.0 + (float)(b - r) / (float)(max - min);
  else if (b == max)
    hue = 4.0 + (float)(r - g) / (float)(max - min);
  else
    hue = 0;

  if (hue < 0)
    hue += 6;

  if (max == min) {
    return -min;
  }

  return (int)(hue * 1000) + 0.2126 * r + 0.7152 * g + 0.0722 * b;
}

void sort_palette(const uint16_t *palette, uint8_t *sorted, int size) {
  // Yes, it's a stupid sort. No, I do not care [yet].
  float *hues = malloc(size * sizeof(float));
  uint8_t *indexes = malloc(size);
  for (int i = 0; i < size; i++) {
    hues[i] = hue(palette[i]);
    indexes[i] = i;
  }

  for (int i = 1; i < size; i++) {
    float h = hues[i];
    int ts = indexes[i];
    int j = i - 1;
    for (; j >= 0 && hues[j] > h; j--) {
      hues[j + 1] = hues[j];
      indexes[j + 1] = indexes[j];
    }
    hues[j + 1] = h;
    indexes[j + 1] = ts;
  }

  for (int i = 0; i < size; i++)
    sorted[i] = indexes[i];

  free(hues);
  free(indexes);
}

void draw_palette(volatile uint16_t *buffer, const uint16_t *palette,
                  int palette_size) {
  uint8_t *sorted_palette = malloc(palette_size);
  sort_palette(palette, sorted_palette, palette_size);

  for (int x = 0; x < imin(WIDTH, palette_size); x++)
    for (int y = 0; y < HEIGHT; y++)
      put_pixel(buffer, y, x, sorted_palette[x]);

  free(sorted_palette);
}

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* add black to the palette */
  uint8_t black = 0;

  add_color(0, 0, 0);
  for (int i = 1; i < orla_palette_size; i++)
    add_color_16(orla_palette[i]);

  /* the buffer we start with */
  volatile uint16_t *buffer = back_buffer;

  clear_screen(front_buffer, black);
  clear_screen(back_buffer, black);

  // draw_fullscreen_image(buffer, orla_indexed);

  // /* wait for vblank before switching buffers */
  // wait_vblank();

  // /* swap the buffers */
  // buffer = flip_buffers(buffer);

  // draw_fullscreen_image(buffer, orla_indexed);

  // print_text(buffer, "Hello! This is a test.", 1, 1);

  // /* wait for vblank before switching buffers */
  // wait_vblank();

  // /* swap the buffers */
  // buffer = flip_buffers(buffer);

  /* loop forever */
  while (1) {
    clear_screen(buffer, black);
    draw_palette(buffer, orla_palette, orla_palette_size);
    wait_vblank();
    buffer = flip_buffers(buffer);
  }
}
