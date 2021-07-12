// This file contains functions that are needed by newlib.

extern int _end;

void *_sbrk(int incr) {
  static unsigned char *heap = 0;
  unsigned char *prev_heap;

  if (heap == 0) {
    heap = (unsigned char *)&_end;
  }
  prev_heap = heap;

  heap += incr;

  return prev_heap;
}
