#include <stdio.h>

int is_composite(int n) {
  for (int i = 2; i < n; i++) {
    if (n % i == 0) {
      return 1;
    }
  }
  return 0;
}

int main() {
  int a = 1;
  int b = 0;
  int c = 0;
  int f = 0;
  int g = 0;
  int h = 0;

  b = 84;
  c = b;

  if (a != 0) {
    b *= 100;
    b -= -100000;
    c = b;
    c -= -17000;
  }

  while (1) {
    f = !is_composite(b);

    if (f == 0) {
      h -= -1;
    }

    g = b - c;
    if (g == 0) {
      printf("h: %d\n", h);
      return 0;
    }
    b -= -17;
  }
}
