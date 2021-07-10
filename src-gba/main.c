/*
 * hello.c
 * this program is a simple GBA example
 * it simply creates a colored screen and waits
 */

#include "utils.h"
#include <stdlib.h>

/* the width and height of the screen */
#define WIDTH 240
#define HEIGHT 160

/* these identifiers define different bit positions of the display control */
#define MODE3 0x0003
#define BG2 0x0400

/* the screen is simply a pointer into memory at a specific address this
 * pointer points to 16-bit colors of which there are 240x160 */
volatile unsigned short *screen = (volatile unsigned short *)0x6000000;

/* compute a 16-bit integer color based on the three components */
unsigned short make_color(unsigned char r, unsigned char g, unsigned char b) {
  unsigned short color = (b & 0x1f) << 10;
  color |= (g & 0x1f) << 5;
  color |= (r & 0x1f);
  return color;
}

/* place a pixel of a given color on the screen */
void put_pixel_direct(int row, int col, unsigned short color) {
  /* set the screen location to this color */
  screen[row * WIDTH + col] = color;
}

int main() {
  char *test = malloc(100);
  test[10] = 12;
  if (test[10] != 12)
    while (1) {
    }
  free(test);

  /* we set the mode to mode 3 with background 2 on */
  *display_control = MODE3 | BG2;

  /* loop through each row of the screen */
  for (int row = 0; row < HEIGHT; row++) {

    /* make a color in the range of black to bright blue based on the row */
    unsigned short color = make_color(0, 0, row % 32);

    /* loop through each column of the screen */
    for (int col = 0; col < WIDTH; col++) {
      put_pixel_direct(row, col, color);
    }
  }

  /* we now loop forever displaying the image */
  while (1) {
    /* do nothing */
  }
}
