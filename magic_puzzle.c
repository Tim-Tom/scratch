#include <stdio.h>

#define TRUE 1
#define FALSE 0

#define GOAL 21
#define WIDTH 3
#define WM (WIDTH-1)
#define WP (WIDTH+1)
#define SIZE (WIDTH*WIDTH)

// typedef bool (*()) predicate;
typedef int bool;
typedef bool (*predicate)();

const static int choices[SIZE] = {3,4,5,6,7,8,9,10,11};
static bool picked[SIZE];
static int a[SIZE];

static predicate additionalConstraints[SIZE];

static int choiceSearch(int c) {
  // Since our list is expected to be so small, we actually just do a linear search.
  for(int i = 0; i < SIZE; ++i) {
    if (choices[i] == c) return i;
  }
  return -1;
}

static bool reverseCrossInvalid() {
  int i, sum;
  for(i = WM, sum = 0; i <= WIDTH*WM; i += WM) {
    sum += a[i];
  }
  return sum != GOAL;
}

static bool bottomRightInvalid() {
  int i, sum;
  // Check bottom horizontal
  for(i = SIZE-WIDTH, sum = 0; i < SIZE; ++i) {
    sum += a[i];
  }
  if (sum != GOAL) {
    return TRUE;
  }
  // Check forward cross
  for(i = 0, sum = 0; i < SIZE; i += WP) {
    sum += a[i];
  }
  return sum != GOAL;
}

static void pickNext(int);

static void pickInternal(int i) {
  int c;
  predicate ac = additionalConstraints[i];
  for(c = 0; c < SIZE; ++c) {
    a[i] = choices[c];
    if (picked[c] || (ac && ac())) {
      continue;
    }
    picked[c] = TRUE;
    pickNext(i);
    picked[c] = FALSE;
  }
}

static void pickRight(int i) {
  int n, c;
  for (c = i - i%WIDTH, n = 0; c < i; ++c) {
    n = n + a[c];
  }
  n = GOAL - n;
  c = choiceSearch(n);
  a[i] = n;
  if (c == -1 || picked[c] || (additionalConstraints[i] && additionalConstraints[i]())) {
    return;
  }
  picked[c] = TRUE;
  pickNext(i);
  picked[c] = FALSE;
}

static void pickBottom(int i) {
  int n, c;
  for (c = i % WIDTH, n = 0; c < i; c += WIDTH) {
    n = n + a[c];
  }
  n = GOAL - n;
  c = choiceSearch(n);
  a[i] = n;
  if (c == -1 || picked[c] || (additionalConstraints[i] && additionalConstraints[i]())) {
    return;
  }
  picked[c] = TRUE;
  pickNext(i);
  picked[c] = FALSE;
}

static void solution() {
  static int count = 0;
  int i, c;
  char sep;
  printf("--- Solution %d ---\n", ++count);
  for(i = 0, c = 1; i < SIZE; ++i, ++c) {
    if (c == WIDTH) {
      sep = '\n';
      c = 0;
    } else {
      sep = ' ';
    }
    printf("%2d%c", a[i], sep);
  }
}

static void pickNext(int i) {
  int next;
  if (i == SIZE - 1) {
    return solution();
  }
  if (i >= WIDTH * WM) {
    next = i - WM;
  } else if (i >= WIDTH * (WIDTH - 2)) {
    return pickBottom(i + WIDTH);
  } else {
    next = i + 1;
  }
  if (next % WIDTH == WM) {
    pickRight(next);
  } else {
    pickInternal(next);
  }
}


int main(int argc, const char* argv[]) {
  additionalConstraints[SIZE - 2*WIDTH + 1] = reverseCrossInvalid;
  additionalConstraints[SIZE - 1] = bottomRightInvalid;
  pickInternal(0);
}
