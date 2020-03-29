#include <stdio.h>

#define TRUE 1
#define FALSE 0

#define WIDTH 4

#define WM (WIDTH-1)
#define WP (WIDTH+1)
#define SIZE (WIDTH*WIDTH)

#define NUM_PERMUTES 8

typedef int bool;

#if WIDTH == 3

#define GOAL 21

// choices array must be sorted.
const static int choices[SIZE] = {3,4,5,6,7,8,9,10,11};


const static int permutations[NUM_PERMUTES][SIZE] = {
  { 0, 1, 2, 3, 4, 5, 6, 7, 8 },
  { 6, 7, 8, 3, 4, 5, 0, 1, 2 },
  { 2, 1, 0, 5, 4, 3, 8, 7, 6 },
  { 8, 7, 6, 5, 4, 3, 2, 1, 0 },
  { 0, 3, 6, 1, 4, 7, 2, 5, 8 },
  { 6, 3, 0, 7, 4, 1, 8, 5, 2 },
  { 2, 5, 8, 1, 4, 7, 0, 3, 6 },
  { 8, 5, 2, 7, 4, 1, 6, 3, 0 }
};


#elif WIDTH == 4

#define GOAL 34

const static int choices[SIZE] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};

const static int permutations[NUM_PERMUTES][SIZE] = {
  {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 },
  { 12, 13, 14, 15,  8,  9, 10, 11,  4,  5,  6,  7,  0,  1,  2,  3 },
  {  3,  2,  1,  0,  7,  6,  5,  4, 11, 10,  9,  8, 15, 14, 13, 12 },
  { 15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0 },
  {  0,  4,  8, 12,  1,  5,  9, 13,  2,  6, 10, 14,  3,  7, 11, 15 },
  { 12,  8,  4,  0, 13,  9,  5,  1, 14, 10,  6,  2, 15, 11,  7,  3 },
  {  3,  7, 11, 15,  2,  6, 10, 14,  1,  5,  9, 13,  0,  4,  8, 12 },
  { 15, 11,  7,  3, 14, 10,  6,  2, 13,  9,  5,  1, 12,  8,  4,  0 }
};

#endif

#define TL 0
#define TR (WIDTH - 1)
#define BL ((WIDTH - 1)*WIDTH)
#define BR (WIDTH*WIDTH - 1)

typedef void (*perform)(int);

typedef struct Action {
  int pos;
  perform action;
  int indexes[WIDTH];
} Action;

typedef struct Chosen {
  int v;
  int i;
} Chosen;

static bool picked[SIZE];
static Chosen a[SIZE];

static Action actions[SIZE + 4];

#ifdef TRACE

static int depth;

static void pad() {
  for (int i = 0; i < depth; ++i) {
    putchar(' ');
  }
}

static void printChoice(int pos, int n) {
  pad();
  printf("Putting %d at %d\n", n, pos);
}

#endif

static int choiceSearch(int c) {
  // Since our list is expected to be so small, we actually just do a linear search.
  for(int i = 0; i < SIZE; ++i) {
    if (choices[i] == c) return i;
  }
  return -1;
}

static void choose(int ai) {
  int i;
  const Action* ap = &actions[ai];
  for (i = 0; i < SIZE; ++i) {
    if (!picked[i]) {
      a[ap->pos].v = choices[i];
      a[ap->pos].i = i;
      picked[i] = TRUE;
      #ifdef TRACE
      printChoice(ap->pos, choices[i]);
      ++depth;
      #endif
      actions[ai + 1].action(ai + 1);
      #ifdef TRACE
      --depth;
      #endif
      picked[i] = FALSE;
    }
  }
}

static void chooseTR(int ai) {
  int i;
  const Action* ap = &actions[ai];
  for (i = a[TL].i + 1; i < SIZE; ++i) {
    if (!picked[i]) {
      a[ap->pos].v = choices[i];
      a[ap->pos].i = i;
      picked[i] = TRUE;
      #ifdef TRACE
      printChoice(ap->pos, choices[i]);
      ++depth;
      #endif
      actions[ai + 1].action(ai + 1);
      #ifdef TRACE
      --depth;
      #endif
      picked[i] = FALSE;
    }
  }
}

static void chooseBL(int ai) {
  int i;
  const Action* ap = &actions[ai];
  for (i = a[TR].i + 1; i < SIZE; ++i) {
    if (!picked[i]) {
      a[ap->pos].v = choices[i];
      a[ap->pos].i = i;
      picked[i] = TRUE;
      #ifdef TRACE
      printChoice(ap->pos, choices[i]);
      ++depth;
      #endif
      actions[ai + 1].action(ai + 1);
      #ifdef TRACE
      --depth;
      #endif
      picked[i] = FALSE;
    }
  }
}

static void checkBR(int ai) {
  if (a[BL].i < a[BR].i || a[TL].i < a[BR].i) {
    actions[ai + 1].action(ai + 1);
  }
  #ifdef TRACE
  else {
    pad();
    printf("Failed checkBR Validation\n");
  }
  #endif
}

static void decide(int ai) {
  int i, n;
  const Action* ap = &actions[ai];
  for(i = 0, n = GOAL; i < WIDTH - 1; ++i) {
    n -= a[ap->indexes[i]].v;
  }
  i = choiceSearch(n);
  if (i == -1 || picked[i]) {
    #ifdef TRACE
    pad();
    printf("Could not put %d at %i\n", n, ap->pos);
    #endif
    return;
  }
  a[ap->pos].i = i;
  a[ap->pos].v = n;
  picked[i] = TRUE;
  #ifdef TRACE
  printChoice(ap->pos, n);
  #endif
  actions[ai + 1].action(ai + 1);
  picked[i] = FALSE;
}

static void validate(int ai) {
  int i, n;
  const Action* ap = &actions[ai];
  for(i = 0, n = GOAL; i < WIDTH; ++i) {
    n -= a[ap->indexes[i]].v;
  }
  if (n == 0) {
    actions[ai + 1].action(ai + 1);
  }
  #ifdef TRACE
  else {
    pad();
    printf("Failed check validation\n");
  }
  #endif
}

static void printSolution(int ai, const int* idx) {
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
    printf("%2d%c", a[idx[i]].v, sep);
  }
}

// Prints out a solution
static void solution(int ai) {
  for (int i = 0; i < NUM_PERMUTES; ++i) {
    printSolution(ai, permutations[i]);
  }
}

int main(int argc, const char* argv[]) {
  int i = -1;
#if WIDTH == 3
  actions[++i].pos = 0; actions[i].action = &choose;
  actions[++i].pos = 2; actions[i].action = &chooseTR;
  actions[++i].pos = 1; actions[i].action = &decide; actions[i].indexes[0] = 0; actions[i].indexes[1] = 2;
  actions[++i].pos = 6; actions[i].action = &chooseBL;
  actions[++i].pos = 3; actions[i].action = &decide; actions[i].indexes[0] = 0; actions[i].indexes[1] = 6;
  actions[++i].pos = 4; actions[i].action = &decide; actions[i].indexes[0] = 2; actions[i].indexes[1] = 6;
  actions[++i].pos = 5; actions[i].action = &decide; actions[i].indexes[0] = 3; actions[i].indexes[1] = 4;
  actions[++i].pos = 7; actions[i].action = &decide; actions[i].indexes[0] = 1; actions[i].indexes[1] = 4;
  actions[++i].pos = 8; actions[i].action = &decide; actions[i].indexes[0] = 6; actions[i].indexes[1] = 7;
  actions[++i].pos = 0; actions[i].action = &validate; actions[i].indexes[0] = 0; actions[i].indexes[1] = 4; actions[i].indexes[2] = 8;
  actions[++i].pos = 0; actions[i].action = &validate; actions[i].indexes[0] = 2; actions[i].indexes[1] = 5; actions[i].indexes[2] = 8;
  actions[++i].pos = 0; actions[i].action = &checkBR;
  actions[++i].pos = 0; actions[i].action = &solution;
#elif WIDTH == 4
  // todo: Not updated to new order
  actions[++i].pos =  0; actions[i].action = &choose;
  actions[++i].pos =  3; actions[i].action = &chooseTR;
  actions[++i].pos =  1; actions[i].action = &choose;
  actions[++i].pos =  2; actions[i].action = &decide;   actions[i].indexes[0] =  0; actions[i].indexes[1] =  1; actions[i].indexes[2] =  3;
  actions[++i].pos = 12; actions[i].action = &chooseBL;
  actions[++i].pos =  6; actions[i].action = &choose;
  actions[++i].pos =  9; actions[i].action = &decide;   actions[i].indexes[0] =  3; actions[i].indexes[1] =  6; actions[i].indexes[2] = 12;
  actions[++i].pos = 10; actions[i].action = &choose;
  actions[++i].pos = 14; actions[i].action = &decide;   actions[i].indexes[0] =  2; actions[i].indexes[1] =  6; actions[i].indexes[2] = 10;
  actions[++i].pos =  5; actions[i].action = &choose;
  actions[++i].pos = 15; actions[i].action = &decide;   actions[i].indexes[0] =  0; actions[i].indexes[1] =  5; actions[i].indexes[2] = 10;
  actions[++i].pos =  0; actions[i].action = &checkBR;
  actions[++i].pos = 13; actions[i].action = &decide;   actions[i].indexes[0] =  1; actions[i].indexes[1] =  5; actions[i].indexes[2] =  9;
  actions[++i].pos =  0; actions[i].action = &validate; actions[i].indexes[0] = 12; actions[i].indexes[1] = 13; actions[i].indexes[2] = 14; actions[i].indexes[3] = 15;
  actions[++i].pos =  4; actions[i].action = &choose;
  actions[++i].pos =  7; actions[i].action = &decide;   actions[i].indexes[0] =  4; actions[i].indexes[1] =  5; actions[i].indexes[2] =  6;
  actions[++i].pos =  8; actions[i].action = &decide;   actions[i].indexes[0] =  0; actions[i].indexes[1] =  4; actions[i].indexes[2] = 12;
  actions[++i].pos = 11; actions[i].action = &decide;   actions[i].indexes[0] =  8; actions[i].indexes[1] =  9; actions[i].indexes[2] = 10;
  actions[++i].pos =  0; actions[i].action = &validate; actions[i].indexes[0] =  3; actions[i].indexes[1] =  7; actions[i].indexes[2] = 11; actions[i].indexes[3] = 15;
  actions[++i].pos =  0; actions[i].action = &solution;
#endif
  actions[0].action(0);
  return 0;
}
