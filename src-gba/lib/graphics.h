#pragma once

#include <stdbool.h>
#include <stdint.h>

typedef struct Image {
  const uint8_t *indexed;
  const uint16_t *palette;
  int palette_size;
} image;

/* pointers to the front and back buffers - the front buffer is the start
 * of the screen array and the back buffer is a pointer to the second half
 */
volatile uint16_t *front_buffer;
volatile uint16_t *back_buffer;

/* the display control pointer points to the gba graphics register */
volatile uint16_t *display_control;

/* the width and height of the screen */
#define WIDTH 240
#define HEIGHT 160

/* these identifiers define different bit positions of the display control */
#define MODE4 0x0004
#define BG2 0x0400

/* this bit indicates whether to display the front or the back buffer
 * this allows us to refer to bit 4 of the display_control register */
#define SHOW_BACK 0x10;

void wait_vblank();

void reset_palette(volatile uint16_t *buffer);
uint8_t add_color(uint8_t r, uint8_t g, uint8_t b);
uint8_t add_color_16(uint16_t color);

void put_pixel(volatile uint16_t *buffer, int row, int col, uint8_t color);

volatile uint16_t *flip_buffers(volatile uint16_t *buffer);

void clear_screen(volatile uint16_t *buffer, uint8_t color);

// Resets the palette and draws an image
void draw_fullscreen_image(volatile uint16_t *buffer, image image);
