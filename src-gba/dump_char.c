#include "font.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s 'X'\n", argv[0]);
    return 1;
  }

  int index = atoi(argv[1]) - ' ';

  printf("P3\n");
  printf("%d %d\n", font_width[index], font_height);
  printf("255\n");
  for (int y = 0; y < font_height; y++)
    for (int x = 0; x < font_width[index]; x++) {
      uint8_t pixel = font_indexed[index][y * font_width[index] + x];
      uint16_t color = font_palette[pixel];
      uint8_t r = color & 0x1f;
      uint8_t g = (color >> 5) & 0x1f;
      uint8_t b = color >> 10;
      printf("%d %d %d ", r << 3, g << 3, b << 3);
    }
  printf("\n");
}