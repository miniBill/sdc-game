#include "utils.h"
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

int imin(int l, int r) { return l < r ? l : r; }
int imax(int l, int r) { return l > r ? l : r; }

uint8_t u8min(uint8_t l, uint8_t r) { return l < r ? l : r; }
uint8_t u8max(uint8_t l, uint8_t r) { return l > r ? l : r; }

int iabs(int i) { return i < 0 ? -i : i; }

char *concat(const char *left, const char *right) {
  int left_len = strlen(left);
  int len = left_len + strlen(right) + 1;
  char *result = malloc(len);
  strcpy(result, left);
  strcpy(result + left_len, right);
  result[len] = 0;
  return result;
}

// I/O
volatile uint16_t *buttons = (volatile uint16_t *)0x04000130;

uint16_t buttons_pressed() { return *buttons; }

/* this function checks whether a particular button has been pressed */
bool button_pressed(enum Buttons button) {
  /* and the button register with the button constant we want */
  uint16_t pressed = *buttons & button;

  /* if this value is zero, then it's pressed */
  return pressed == 0;
}

void delay(int milliseconds) {
  uint8_t trash;
  volatile uint8_t *trashp = &trash;
  for (int i = 0; i < 500 * milliseconds; i++)
    *trashp = 0;
}
