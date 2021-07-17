#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

      uint16_t color = (b >> 3) << 10 | (g >> 3) << 5 | (r >> 3);
      if (color > 0 && palette_inverse[color] == 0) {
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

  // Print header
  char *output_h_name = calloc(
      strlen("out/art/") + strlen(image_name) + strlen(".h") + 1, sizeof(char));
  sprintf(output_h_name, "out/art/%s.h", image_name);

  FILE *output_h = fopen(output_h_name, "w");

  fprintf(output_h, "#include <stdint.h>\n");
  fprintf(output_h, "#include \"../src-gba/lib/graphics.h\"\n");
  fprintf(output_h, "\n");
  fprintf(output_h, "extern const image %s_image;\n", image_name);
  fprintf(output_h, "\n");
  fprintf(output_h, "extern const int %s_palette_size;\n", image_name);
  fprintf(output_h, "extern const uint16_t %s_palette[%d];\n", image_name,
          next_free_palette);

  fprintf(output_h, "\n");

  fprintf(output_h, "extern const int %s_indexed_width;\n", image_name);
  fprintf(output_h, "extern const int %s_indexed_height;\n", image_name);
  fprintf(output_h, "extern const uint8_t %s_indexed[%d];\n", image_name,
          width * height);

  fclose(output_h);

  // Print code
  char *output_c_name = calloc(
      strlen("out/art/") + strlen(image_name) + strlen(".c") + 1, sizeof(char));
  sprintf(output_c_name, "out/art/%s.c", image_name);

  FILE *output_c = fopen(output_c_name, "w");

  fprintf(output_c, "#include \"%s.h\"\n", image_name);
  fprintf(output_c, "#include <stdint.h>\n");
  fprintf(output_c, "\n");
  fprintf(output_h, "const image %s_image = { %s_indexed, %s_palette, %d };\n",
          image_name, image_name, image_name, next_free_palette);
  fprintf(output_c, "\n");
  fprintf(output_c, "const int %s_palette_size = %d;\n", image_name,
          next_free_palette);
  fprintf(output_c, "\n");
  fprintf(output_c,
          "const uint16_t %s_palette[%d] __attribute__((aligned(4))) = {",
          image_name, next_free_palette);
  for (int i = 0; i < next_free_palette; i++) {
    if (i % 8 == 0)
      fprintf(output_c, "\n  ");
    fprintf(output_c, (i % 8) < 7 ? "0x%04x, " : "0x%04x,", palette[i]);
  }
  fprintf(output_c, "\n};\n");

  fprintf(output_c, "\n");

  fprintf(output_c, "const int %s_indexed_width = %d;\n", image_name, width);
  fprintf(output_c, "const int %s_indexed_height = %d;\n", image_name, height);
  fprintf(output_c, "\n");
  fprintf(output_c,
          "const uint8_t %s_indexed[%d] __attribute__((aligned(4))) = {",
          image_name, width * height);
  for (int i = 0; i < width * height; i++) {
    if (i % 24 == 0)
      fprintf(output_c, "\n  ");
    fprintf(output_c, (i % 24) < 23 ? "0x%02x, " : "0x%02x,", indexed_image[i]);
  }
  fprintf(output_c, "};\n");

  fclose(output_c);
}
