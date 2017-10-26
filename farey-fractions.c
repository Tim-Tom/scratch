#include <stdio.h>

#define N 500
#define SIZE (N*N)

#define LESS 0
#define GREATER 1


typedef struct fract {
  int num;
  int den;
} frac;

frac farey[SIZE];

int build_farey() {
  int a, b, c, d, k, i, an, bn;
  a = 0;
  b = 1;
  c = 1;
  d = N;
  i = 0;
  while (c < N) {
    k = ((N + b) / d);
    an = k*c - a;
    bn = k*d - b;
    a = c;
    b = d;
    c = an;
    d = bn;
    farey[i].num = a;
    farey[i++].den = b;
  }
  return i - 1;
}

int myCompare(const frac a, const frac b) {
  frac c;
  c.num = a.num - b.num;
  c.den = a.den - b.den;
  if (c.den < 0) {
    printf("???\n");
    c.num = -c.num;
    c.den = -c.den;
  }
  if (c.num < 0) {
    return LESS;
  } else if (c.num == 0) {
    return c.den == 0 ? GREATER : LESS;
  } else if (c.num == 1 && c.den == 1) {
    return GREATER;
  } else if (c.den < b.den) {
    return !myCompare(b, c);
  } else {
    return myCompare(c, b);
  }
}

#define FA farey[i]
#define FB farey[j]

int main(const int argc, const char* const argv[]) {
  int i, j, res;
  int len = build_farey();
  printf("%d\n", len);
  for (i = 0; i < len; ++i) {
    for (j = i + 1; j < len; ++j) {
      if (FA.den < FB.den) {
        res = !myCompare(FB, FA);
      } else {
        res = myCompare(FA, FB);
      }
      if (res != LESS) {
        printf("%3d / %3d <> %3d / %3d: %d", FA.num, FA.den, FB.num, FB.den, res);
      }
    }
  }
  return 0;
}
