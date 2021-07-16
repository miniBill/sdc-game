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
                        current_scene < 0 ? HEIGHT / 2 : 0);

    char *left_label = 0;
    char *right_label = 0;
    switch (scene.choices_count) {
    case 1:
      right_label = concat("A/B: ", strlen(scene.choices_labels[0])
                                        ? scene.choices_labels[0]
                                        : "Next");
      break;
    case 2:
      left_label = concat("B: ", scene.choices_labels[0]);
      right_label = concat("A: ", scene.choices_labels[1]);
      break;
    }

    if (left_label) {
      print_text(buffer, left_label, 1, HEIGHT - font_indexed_height + 1);
      free(left_label);
    }
    if (right_label) {
      print_text_right(buffer, right_label, WIDTH,
                       HEIGHT - font_indexed_height + 1);
      free(right_label);
    }

    wait_vblank();
    buffer = flip_buffers(buffer);

    choice = -1;
    if (current_scene < 0) {
      while (current_scene < 0) {
        uint16_t btn = buttons_pressed();
        if ((btn & Button_Start) == 0) {
          current_scene = 0;
          scene = main_scene;
        }
      }
    } else {
      while (choice < 0) {
        uint16_t btn = buttons_pressed();
        if ((btn & Button_Left) == 0 || (btn & Button_B) == 0) {
          choice = 0;
        } else if ((btn & Button_Right) == 0 || (btn & Button_A) == 0) {
          choice = 1;
        }
      }
      scene = step(&current_scene, choice);
    }
  }
}
