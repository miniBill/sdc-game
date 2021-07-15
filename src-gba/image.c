#include "lib/graphics.h"
#include "lib/text.h"
#include "lib/utils.h"
#include "out/font.h"
#include "out/orla1.h"
#include "out/orla2.h"
#include "out/orla3.h"
#include "out/orla4.h"
#include "out/orla5.h"
#include "out/orla6.h"
#include "out/orla7.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* the buffer we start with */
  volatile uint16_t *buffer = back_buffer;

  wait_vblank();
  buffer = flip_buffers(buffer);

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
      sentence = "AV test";
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

    wait_vblank();
    buffer = flip_buffers(buffer);

    reset_palette();
    add_color(0, 0, 0);
    for (int i = 1; i < palette_size; i++)
      add_color_16(palette[i]);
    draw_fullscreen_image(buffer, image);

    setup_font_palette();
    print_text_centered(buffer, sentence, WIDTH / 2,
                        HEIGHT - font_indexed_height + 1);

    wait_vblank();
    buffer = flip_buffers(buffer);

    delay(2000);
  }
}
