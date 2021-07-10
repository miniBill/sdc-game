/*
 * square2.c
 * this program attempts to draw a square in a loop
 */

#include "utils.h"
#include <stdint.h>
#include <stdlib.h>

/* clear the screen to black */
void draw_pretty(volatile unsigned short *buffer) {
  unsigned short row, col;
  /* set each pixel black */
  for (row = 0; row < HEIGHT; row++) {
    for (col = 0; col < WIDTH; col++) {
      put_pixel(buffer, row, col,
                ((col * 16 / WIDTH << 4) + (row * 16 / HEIGHT)) % 256);
    }
  }
}

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* add black to the palette */
  uint8_t black = 0;

  for (int g = 0; g < (1 << 4); g++)
    for (int b = 0; b < (1 << 4); b++)
      add_color(0, g << 1, b << 1);

  // add_color(0, 0, 0);
  // for (int i = 1; i < 256; i++)
  //   add_color_16(orla_palette[i]);

  /* the buffer we start with */
  volatile uint16_t *buffer = back_buffer;

  clear_screen(front_buffer, black);
  clear_screen(back_buffer, black);

  uint8_t *dump = malloc(WIDTH * HEIGHT);
  for (int i = 0; i < WIDTH * HEIGHT; i++)
    dump[i] = i;

  /* loop forever */
  while (1) {
    // draw_fullscreen_image(buffer, dump);
    draw_pretty(buffer);

    /* wait for vblank before switching buffers */
    wait_vblank();

    /* swap the buffers */
    buffer = flip_buffers(buffer);
  }
}
