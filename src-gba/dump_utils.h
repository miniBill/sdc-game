#pragma once

#include <stdio.h>

int read_ppm_header(int *width, int *height) {
  char p, three;
  scanf("%c%c", &p, &three);

  if (p != 'P' || three != '3') {
    fprintf(stderr, "Input image must be in 'P3' Netpbm format\n");
    return 0;
  }

  int bit_depth;
  scanf("%d %d %d", width, height, &bit_depth);

  if (bit_depth != 255) {
    fprintf(stderr, "Input image must have bit depth 255, found %d\n",
            bit_depth);
    return 0;
  }

  return 1;
}
