#include "dump_utils.h"
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

uint16_t palette[256] = {0};

uint8_t palette_inverse[256 * 256 * 256] = {0};

int dump_char(FILE *output_h, FILE *output_c, char curr, uint8_t *indexed_image,
              int width, int height, int fontl) {
  int fontr = fontl + 1;
  while (indexed_image[fontr] == 0)
    fontr++;

  int char_width = fontr - fontl - 1;
  int size = char_width * (height - 1);

  fprintf(output_c, "\n");
  fprintf(output_c, "const uint8_t font_%d_width = %d;\n", curr, char_width);
  fprintf(output_c, "\n");
  fprintf(output_c,
          "const uint8_t font_%d_indexed[%d] __attribute__((aligned(4))) = {",
          curr, size);
  for (int i = 0; i < size; i++) {
    if (i % 24 == 0)
      fprintf(output_c, "\n  ");
    fprintf(
        output_c, (i % 24) < 23 ? "0x%02x, " : "0x%02x,",
        indexed_image[width * (1 + i / char_width) + fontl + i % char_width]);
  }
  fprintf(output_c, "};\n");

  return fontr;
}

int main(int argc, char *argv[]) {
  int width, height;
  if (!read_ppm_header(&width, &height)) {
    return 2;
  }

  int next_free_palette = 1;

  uint8_t *indexed_image = malloc(width * height);

  for (int y = 0, i = 0; y < height; y++)
    for (int x = 0; x < width; x++, i++) {
      int r, g, b;
      scanf("%d %d %d", &r, &g, &b);

      uint16_t color = (b >> 3) << 10 | (g >> 3) << 5 | (r >> 3);
      if (color > 0 && palette_inverse[color] == 0) {
        if (next_free_palette > 3) {
          fprintf(stderr, "Ran out of palette at %f%%\n",
                  (100.0 * (y * width + x)) / (1.0 * width * height));
          return 4;
        }
        palette_inverse[color] = next_free_palette;
        palette[next_free_palette++] = color;
      }
      indexed_image[i] = palette_inverse[color];
    }

  int font_size = '~' - ' ' + 1;

  FILE *output_h = fopen("out/font.h", "w");

  fprintf(output_h, "#include <stdint.h>\n");
  fprintf(output_h, "\n");
  fprintf(output_h, "extern const int font_palette_size;\n");
  fprintf(output_h, "extern const uint16_t font_palette[%d];\n",
          next_free_palette);
  fprintf(output_h, "\n");
  fprintf(output_h, "extern const int font_height;\n");
  fprintf(output_h, "extern const uint8_t font_width[%d];\n", font_size);
  fprintf(output_h, "extern const uint8_t * font_indexed[%d];\n", font_size);

  FILE *output_c = fopen("out/font.c", "w");

  fprintf(output_c, "#include \"font.h\"\n");
  fprintf(output_c, "#include <stdint.h>\n");
  fprintf(output_c, "\n");
  fprintf(output_c, "const int font_palette_size = %d;\n", next_free_palette);

  fprintf(output_c,
          "\nconst uint16_t font_palette[%d] __attribute__((aligned(4))) = {",
          next_free_palette);
  for (int i = 0; i < next_free_palette; i++) {
    if (i % 8 == 0)
      fprintf(output_c, "\n  ");
    fprintf(output_c, (i % 8) < 7 ? "0x%04x, " : "0x%04x,", palette[i]);
  }
  fprintf(output_c, "\n};\n");

  int fontl = 0;
  for (char curr = ' '; curr <= '~'; curr++)
    fontl = dump_char(output_h, output_c, curr, indexed_image, width, height,
                      fontl);

  fprintf(output_c, "\nconst int font_height = %d;\n", height - 1);

  fprintf(output_c, "\nconst uint8_t font_width[%d] = {", font_size);
  for (char curr = ' '; curr <= '~'; curr++) {
    if ((curr - ' ') % 8 == 0)
      fprintf(output_c, "\n  ");
    fprintf(output_c,
            ((curr - ' ') % 8) < 7 ? "font_%d_width, " : "font_%d_width,",
            curr);
  }
  fprintf(output_c, "\n};\n");

  fprintf(output_c, "\nconst uint8_t * font_indexed[%d] = {", font_size);
  for (char curr = ' '; curr <= '~'; curr++) {
    if ((curr - ' ') % 8 == 0)
      fprintf(output_c, "\n  ");
    fprintf(output_c,
            ((curr - ' ') % 8) < 7 ? "font_%d_indexed, " : "font_%d_indexed,",
            curr);
  }
  fprintf(output_c, "\n};\n");

  fclose(output_c);
  fclose(output_h);

  return 0;
}
