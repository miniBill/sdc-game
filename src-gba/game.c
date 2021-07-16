#include "font.h"
#include "lib/graphics.h"
#include "lib/text.h"
#include "lib/utils.h"
#include "logic.h"
#include "orla1.h"
#include "orla2.h"
#include "orla3.h"
#include "orla4.h"
#include "orla5.h"
#include "orla6.h"
#include "orla7.h"
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

  int current_scene = 0;
  int choice = -1;
  scene scene = main_scene;

  /* loop forever */
  while (1) {
    char *text = scene.text;
    const image *image = scene.image;

    wait_vblank();
    buffer = flip_buffers(buffer);

    if (image)
      draw_fullscreen_image(buffer, *image);
    else
      clear_screen(buffer, 0);

    setup_font_palette();
    print_text_centered(buffer, text, WIDTH / 2,
                        HEIGHT - font_indexed_height + 1);

    wait_vblank();
    buffer = flip_buffers(buffer);

    choice = -1;
    while (choice < 0) {
      if (button_pressed(Button_Right) || button_pressed(Button_A)) {
        choice = 0;
      } else if (button_pressed(Button_Left) || button_pressed(Button_B)) {
        choice = 1;
      }
    }
    scene = step(&current_scene, choice);
  }
}
