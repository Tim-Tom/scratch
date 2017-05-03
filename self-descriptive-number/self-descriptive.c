#include <stdio.h>

int num[10] = {0};
int remain[10] = {0};

int invalid = 0;

void pick(int i) {
  int valid;
  int j, n;
  if (i == 9) {
    n = -remain[i];
    if (n < 0 || n > 9) {
      return;
    }
    --remain[n];
    remain[i] += n;
    for (j = 0, valid = 1; j < 10; ++j) {
      if (remain[j] != 0) {
        valid = 0;
        break;
      }
    }
    if (valid) {
      num[i] = n;
      for (j = 0; j < 10; ++j) {
        printf("%d ", num[j]);
      }
      printf("\n");
    } else {
      ++invalid;
    }
    ++remain[n];
    remain[i] -= n;
  } else {
    for (n = 0; n < 10; ++n) {
      num[i] = n;
      --remain[n];
      remain[i] += n;
      if (remain[i] >= 0 && (n > i || remain[n] >= 0)) {
        pick(i + 1);
      }
      /* if (remain[i] >= 0) { */
      /*   pick(i + 1); */
      /* } */
      ++remain[n];
      remain[i] -= n;
    }
  }
}

int main() {
  int i;
  pick(0);
  printf("Evaluated %d invalid candidates\n", invalid);
  return 0;
}
