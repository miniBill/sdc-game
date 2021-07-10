#include "out/orla.h"
#include "utils.h"
#include <stdint.h>
#include <stdlib.h>

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

  /* loop forever */
  while (1) {
    draw_fullscreen_image(buffer, orla_indexed);

    /* wait for vblank before switching buffers */
    wait_vblank();

    /* swap the buffers */
    buffer = flip_buffers(buffer);
  }
}
