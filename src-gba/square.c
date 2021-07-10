/*
 * square2.c
 * this program attempts to draw a square in a loop
 */

#include "stdint.h"
#include "utils.h"

/* this bit indicates whether to display the front or the back buffer
 * this allows us to refer to bit 4 of the display_control register */
#define SHOW_BACK 0x10;

/* the display control pointer points to the gba graphics register */
volatile uint16_t *display_control = (volatile uint16_t *)0x4000000;

/* the address of the color palette used in graphics mode 4 */
volatile uint16_t *palette = (volatile uint16_t *)0x5000000;

/* pointers to the front and back buffers - the front buffer is the start
 * of the screen array and the back buffer is a pointer to the second half */
volatile uint16_t *front_buffer = (volatile uint16_t *)0x6000000;
volatile uint16_t *back_buffer = (volatile uint16_t *)0x600A000;
/* the button register holds the bits which indicate whether each button has
 * been pressed - this has got to be volatile as well
 */
volatile uint16_t *buttons = (volatile uint16_t *)0x04000130;

/* the bit positions indicate each button - the first bit is for A, second for
 * B, and so on, each constant below can be ANDED into the register to get the
 * status of any one button */
#define BUTTON_A (1 << 0)
#define BUTTON_B (1 << 1)
#define BUTTON_SELECT (1 << 2)
#define BUTTON_START (1 << 3)
#define BUTTON_RIGHT (1 << 4)
#define BUTTON_LEFT (1 << 5)
#define BUTTON_UP (1 << 6)
#define BUTTON_DOWN (1 << 7)
#define BUTTON_R (1 << 8)
#define BUTTON_L (1 << 9)

/* this function checks whether a particular button has been pressed */
uint8_t button_pressed(uint16_t button) {
  /* and the button register with the button constant we want */
  uint16_t pressed = *buttons & button;

  /* if this value is zero, then it's pressed */
  return pressed == 0;
}

/* a colored square */
struct square {
  unsigned short x, y, size;
  unsigned char color;
};

/* draw a square onto the screen */
void draw_square(volatile unsigned short *buffer, struct square *s) {
  unsigned short row, col;
  /* for each row of the square */
  for (row = imax(s->y - 6, 0); row < imin(HEIGHT, s->y + s->size + 6); row++) {
    /* loop through each column of the square */
    for (col = imax(s->x - 6, 0); col < imin(WIDTH, s->x + s->size + 6);
         col++) {
      /* set the screen location to this color */
      put_pixel(buffer, row, col, 0);
    }
  }

  /* for each row of the square */
  for (row = s->y; row < (s->y + s->size); row++) {
    /* loop through each column of the square */
    for (col = s->x; col < (s->x + s->size); col++) {
      /* set the screen location to this color */
      put_pixel(buffer, row, col, s->color);
    }
  }
}

/* clear the screen to black */
void clear_screen(volatile unsigned short *buffer, unsigned short color) {
  unsigned short row, col;
  /* set each pixel black */
  for (row = 0; row < HEIGHT; row++) {
    for (col = 0; col < WIDTH; col++) {
      put_pixel(buffer, row, col, color);
    }
  }
}

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

/* this function takes a video buffer and returns to you the other one */
volatile unsigned short *flip_buffers(volatile unsigned short *buffer) {
  /* flip back buffer bit and return the other buffer */
  *display_control ^= SHOW_BACK;
  return (volatile unsigned short *)((int)buffer ^ 0xA000);
}

/* handle the buttons which are pressed down */
void handle_buttons(struct square *s) {
  /* move the square with the arrow keys */
  if (button_pressed(BUTTON_DOWN)) {
    s->y = imin(HEIGHT - s->size, s->y + 3);
  }
  if (button_pressed(BUTTON_UP)) {
    s->y = imax(0, s->y - 3);
  }
  if (button_pressed(BUTTON_RIGHT)) {
    s->x = imin(WIDTH - s->size, s->x + 3);
  }
  if (button_pressed(BUTTON_LEFT)) {
    s->x = imax(0, s->x - 3);
  }
}

/* the main function */
int main() {
  /* we set the mode to mode 4 with bg2 on */
  *display_control = MODE4 | BG2;

  /* add black to the palette */
  unsigned char black = 0;

  for (int g = 0; g < (1 << 4); g++)
    for (int b = 0; b < (1 << 4); b++)
      add_color(0, g << 1, b << 1);

  /* make a green square */
  // struct square s = {0, 0, 15, 1};

  /* the buffer we start with */
  volatile unsigned short *buffer = back_buffer;

  clear_screen(front_buffer, black);
  clear_screen(back_buffer, black);

  /* loop forever */
  while (1) {
    /* clear the screen */

    // handle_buttons(&s);

    /* draw the square */
    // draw_square(buffer, &s);

    draw_pretty(buffer);

    /* wait for vblank before switching buffers */
    wait_vblank();

    /* swap the buffers */
    buffer = flip_buffers(buffer);
  }
}
