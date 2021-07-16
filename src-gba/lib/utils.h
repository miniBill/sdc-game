#pragma once

#include <stdbool.h>
#include <stdint.h>

/////////////
// Generic //
/////////////
int imin(int l, int r);
int imax(int l, int r);

uint8_t u8min(uint8_t l, uint8_t r);
uint8_t u8max(uint8_t l, uint8_t r);

int iabs(int i);

char *concat(const char *left, const char *right);

/////////
// I/O //
/////////

/* the bit positions indicate each button - the first bit is for A, second for
 * B, and so on, each constant below can be ANDED into the register to get the
 * status of any one button */
enum Buttons {
  Button_A = 0x001,
  Button_B = 0x002,
  Button_Select = 0x004,
  Button_Start = 0x008,
  Button_Right = 0x010,
  Button_Left = 0x020,
  Button_Up = 0x040,
  Button_Down = 0x080,
  Button_R = 0x100,
  Button_L = 0x200
};

uint16_t buttons_pressed();

bool button_pressed(enum Buttons button);

void delay(int milliseconds);
