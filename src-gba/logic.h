#pragma once

#include "lib/graphics.h"

typedef struct Scene {
  char *text;
  const image *image;
  int choices_count;
  const char *const *choices_labels;
} scene;

extern const scene main_scene;

scene step(int *current_scene, int choice);
