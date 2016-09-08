#include <stdio.h>

#define TRUE 1
#define FALSE 0

#define GOAL 34
#define WIDTH 4
#define WM (WIDTH-1)
#define WP (WIDTH+1)
#define SIZE (WIDTH*WIDTH)

typedef int bool;
typedef bool (*predicate)();

static void pickNext(int);

const static int choices[SIZE] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
static bool picked[SIZE];
static int a[SIZE];

static predicate additionalConstraints[SIZE];

// Find thge index of the item in our choice list. Returns -1 on failure.
static int choiceSearch(int c) {
  // Since our list is expected to be so small, we actually just do a linear search.
  for(int i = 0; i < SIZE; ++i) {
    if (choices[i] == c) return i;
  }
  return -1;
}

// Returns whether the cross from the top right to the bottom left is invalid.
static bool reverseCrossInvalid() {
  int i, sum;
  for(i = WM, sum = 0; i <= WIDTH*WM; i += WM) {
    sum += a[i];
  }
  return sum != GOAL;
}

// Returns whether the bottom row is invalid and the cros sfro mthe top left to the bottom
// right is invalid.
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

// Square that's not along the bottom or right edges, so we have to pick from the
// remaining choices.
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

// Square on the right edge, sum the elements horizontally, subtract from the goal, and
// check if it's a valid selection.
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

// Square on the bottom edge, sum the elements vertically, subtract from the goal, and
// check if it's a valid selection.
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

// Prints out a solution
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

// We go top to bottom, left to right except that on the second to last row we finish off
// the bottom before progressing right since it doesn't require any choices to fill out
// that box.
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
