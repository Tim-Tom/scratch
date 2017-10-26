#include <stdio.h>

typedef int (*t1)(int, int);

typedef int (*t2)(int, t1[], int);

typedef int (*t3)(int, int (int, int));


typedef int (*(*(*t4)(int, int))(int, int))(int, int);
// typedef int (*(*t4)(int, int))(int, int))(int, int (*(int, int)));



int add(int x, int y) {
  return x + y;
}

int dbl(int x, int (*y)(int, int)) {
  return y(x, x);
}

int addN(int base, t1 fs[], int num) {
  int result = fs[0](base, base);
  for (int i = 1; i < num; ++i) {
    result = fs[i](result, base);
  }
  return result;
}

int main(const int argc, const char* const argv[]) {
  t1 adds[3] = { add, add, add};
  t2 pf = addN;
  t3 dbl2 = dbl;
  printf("%d\n", dbl2(5, add));
  printf("%d\n", pf(5,adds, 3));
  return 0;
}
