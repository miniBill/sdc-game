#include "font.h"
#include "lib/graphics.h"
#include "lib/text.h"
#include "lib/utils.h"
#include "logic.h"
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* the buffer we start with */
  volatile uint16_t *buffer = back_buffer;

  int current_scene = 0;
  scene scene = main_scene;

  // Prevent double-press by keeping track of the latest read
  uint16_t last_buttons = 0;

  /* loop forever */
  while (1) {
    char *text = scene.text;
    const image *image = scene.image;

    if (image)
      draw_fullscreen_image(buffer, *image);
    else {
      reset_palette(buffer);
      clear_screen(buffer, 0);
    }

    setup_font_palette();
    if (current_scene < 0)
      print_text(buffer, text, WIDTH / 2, HEIGHT / 2, ALIGN_MIDDLE,
                 ALIGN_MIDDLE);
    else
      print_text(buffer, text, WIDTH / 2, 0, ALIGN_MIDDLE, ALIGN_BEGIN);

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
      print_text(buffer, left_label, 0, HEIGHT, ALIGN_BEGIN, ALIGN_END);
      free(left_label);
    }
    if (right_label) {
      print_text(buffer, right_label, WIDTH, HEIGHT, ALIGN_END, ALIGN_END);
      free(right_label);
    }

    wait_vblank();
    buffer = flip_buffers(buffer);

    while (1) {
      uint16_t btn = buttons_pressed();
      if (btn == last_buttons)
        continue;
      last_buttons = btn;
      if ((btn & Button_Start) == 0) {
        current_scene = 0;
        scene = main_scene;
        break;
      }
      if (current_scene >= 0) {
        if ((btn & Button_Left) == 0 || (btn & Button_B) == 0) {
          scene = step(&current_scene, 0);
          break;
        } else if ((btn & Button_Right) == 0 || (btn & Button_A) == 0) {
          scene = step(&current_scene, 1);
          break;
        }
      }
    }
  }
}
