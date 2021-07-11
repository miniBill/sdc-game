#include "utils.h"

/* the scanline counter is a memory cell which is updated to indicate how
 * much of the screen has been drawn */
volatile uint16_t *scanline_counter = (volatile uint16_t *)0x4000006;

/* pointers to the front and back buffers - the front buffer is the start
 * of the screen array and the back buffer is a pointer to the second half */
volatile uint16_t *front_buffer = (volatile uint16_t *)0x6000000;
volatile uint16_t *back_buffer = (volatile uint16_t *)0x600A000;

/* the display control pointer points to the gba graphics register */
volatile uint16_t *display_control = (volatile uint16_t *)0x4000000;

/* wait for the screen to be fully drawn so we can do something during vblank */
void wait_vblank() {
  /* wait until all 160 lines have been updated */
  while (*scanline_counter < 160) {
  }
}

int imin(int l, int r) { return l < r ? l : r; }
int imax(int l, int r) { return l > r ? l : r; }

uint8_t u8min(uint8_t l, uint8_t r) { return l < r ? l : r; }
uint8_t u8max(uint8_t l, uint8_t r) { return l > r ? l : r; }

int iabs(int i) { return i < 0 ? -i : i; }

/* put a pixel on the screen in mode 4 */
void put_pixel(volatile uint16_t *buffer, int row, int col, uint8_t color) {
  /* find the offset which is the regular offset divided by two */
  uint16_t offset = (row * WIDTH + col) >> 1;

  /* read the existing pixel which is there */
  uint16_t pixel = buffer[offset];

  /* if it's an odd column */
  if (col & 1) {
    /* put it in the left half of the short */
    buffer[offset] = (color << 8) | (pixel & 0x00ff);
  } else {
    /* it's even, put it in the left half */
    buffer[offset] = (pixel & 0xff00) | color;
  }
}

/* the address of the color palette used in graphics mode 4 */
volatile uint16_t *palette = (volatile uint16_t *)0x5000000;

/*
 * function which adds a color to the palette and returns the
 * index to it
 */
uint8_t add_color(uint8_t r, uint8_t g, uint8_t b) {
  uint16_t color = b << 10;
  color += g << 5;
  color += r;

  return add_color_16(color);
}

int next_palette_index = 0;

void reset_palette() { next_palette_index = 0; }

/*
 * function which adds a color to the palette and returns the
 * index to it
 */
uint8_t add_color_16(uint16_t color) {
  /* add the color to the palette */
  palette[next_palette_index] = color;

  /* increment the index */
  next_palette_index++;

  /* return index of color just added */
  return next_palette_index - 1;
}

/* this function takes a video buffer and returns to you the other one */
volatile uint16_t *flip_buffers(volatile uint16_t *buffer) {
  /* flip back buffer bit and return the other buffer */
  *display_control ^= SHOW_BACK;
  return (volatile uint16_t *)((int)buffer ^ 0xA000);
}

/* clear the screen to black */
void clear_screen(volatile uint16_t *buffer, uint8_t color) {
  uint16_t row, col;
  /* set each pixel black */
  for (row = 0; row < HEIGHT; row++) {
    for (col = 0; col < WIDTH; col++) {
      put_pixel(buffer, row, col, color);
    }
  }
}

void draw_fullscreen_image(volatile uint16_t *buffer, const uint8_t *image) {
  for (int i = 0; i < HEIGHT * WIDTH; i += 2)
    buffer[i / 2] = (image[i + 1] << 8) | image[i];
}
