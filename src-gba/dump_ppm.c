#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint16_t palette[256] = {0};

uint8_t palette_inverse[256 * 256 * 256] = {0};

int main(int argc, char *argv[]) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s image_name < input.ppm > output.c\n");
    return 1;
  }

  char *image_name = argv[1];

  char p, three;
  scanf("%c%c", &p, &three);

  if (p != 'P' || three != '3') {
    fprintf(stderr, "Input image must be in 'P3' Netpbm format\n");
    return 2;
  }

  int width, height;
  scanf("%d %d", &width, &height);

  int bit_depth;
  scanf("%d", &bit_depth);

  if (bit_depth != 255) {
    fprintf(stderr, "Input image must have bit depth 255, found %d\n",
            bit_depth);
    return 3;
  }

  int next_free_palette = 1;

  uint8_t *indexed_image = malloc(width * height);

  for (int y = 0, i = 0; y < height; y++)
    for (int x = 0; x < width; x++, i++) {
      uint32_t r, g, b;
      scanf("%d %d %d", &r, &g, &b);

      uint16_t color = (r >> 3) << 10 | (g >> 3) << 5 | (b >> 3);
      if (palette_inverse[color] == 0) {
        if (next_free_palette > 255) {
          fprintf(stderr, "Ran out of palette at %f%%\n",
                  (100.0 * (y * width + x)) / (1.0 * width * height));
          return 4;
        }
        palette_inverse[color] = next_free_palette;
        palette[next_free_palette++] = color;
      }
      indexed_image[i] = palette_inverse[color];
    }

  printf("#include <stdint.h>\n");
  printf("\n");
  printf("uint16_t %s_palette[] = {\n", image_name);
  for (int y = 0; y < 8; y++) {
    printf("  ");
    for (int x = 0; x < 8; x++)
      printf(x < 7 ? "0x%04x, " : "0x%04x,", palette[y * 8 + x]);
    printf("\n");
  }
  printf("};\n");

  printf("\n");

  printf("uint8_t %s_indexed[] = {\n", image_name);
  for (int y = 0; y < height; y++) {
    printf("  ");
    for (int x = 0; x < width; x++)
      printf(x < width - 1 ? "0x%02x, " : "0x%02x,",
             indexed_image[y * width + x]);
    printf("\n");
  }
  printf("};\n");
}
