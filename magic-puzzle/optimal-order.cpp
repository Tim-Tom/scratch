#include <stdio.h>
#include <stdbool.h>
#include <malloc.h>
#include <string>
#include <map>
#include <list>
#include <vector>
#include <queue>

#define WIDTH 5
#define SIZE (WIDTH*WIDTH)

#define WP (WIDTH + 1)
#define WM (WIDTH - 1)

#define POOL_SIZE 1024

typedef struct Board {
  int board[SIZE];
  int remaining[SIZE];
  int remaining_count;
  int cost;
  int step;
  int estimate;
} Board;

typedef struct Validation {
  int position;
  int requirements[WIDTH - 2];
} Validation;

Validation validations[SIZE][SIZE];


class Board_Cost {
public:
  bool operator()(const Board* a, const Board* b) const {
    return a->estimate < b->estimate;
  }
};

typedef std::map<std::string, Board*> BoardMap;
BoardMap memory;
std::list<Board*> free_boards;
std::priority_queue<Board*, std::vector<Board*>, Board_Cost> board_list;

void pick(Board* b, int p, int cost) {
  int i, j, vi;
  b->board[p] = cost;
  b->cost += cost * cost;
  b->remaining[p] = false;
  b->remaining_count -= 1;
  for (vi = 0; vi < SIZE && validations[p][vi].position != -1; ++vi) {
    bool valid = true;
    Validation* v = &validations[p][vi];
    if (b->board[v->position] != 0) continue;
    // printf("got here: %d %d %d\n", p, vi, v->position);
    for (j = 0; j < WIDTH - 2; ++j) {
      // printf("\t%d: %d\n", v->requirements[j], b->board[v->requirements[j]]);
      if (!b->board[v->requirements[j]]) {
        valid = false;
        break;
      }
    }
    if (valid) {
      pick(b, v->position, cost);
    }
  }
}

Board* getBoard() {
  int i;
  Board* new_boards;
  Board* result;
  if (free_boards.size() == 0) {
    new_boards = new Board[POOL_SIZE];
    for(i = 0; i < POOL_SIZE; ++i) {
      free_boards.push_back(new_boards + i);
    }
  }
  result = free_boards.front();
  free_boards.pop_front();
  return result;
}

void returnBoard(Board* b) {
  free_boards.push_front(b);
}

std::string getHash(Board* b) {
  int i;
  std::string result(SIZE, '\0');
  for(i = 0; i < SIZE; ++i) {
    result[i] = '0' + (b->board[i] != 0);
  }
  return result;
}

int best_limit = SIZE*SIZE*SIZE;

int c,m;

void solve() {
  int i;
  Board* b;
  while (!board_list.empty()) {
    b = board_list.top();
    board_list.pop();
    if (b->estimate >= best_limit) {
      continue;
    }
    ++c;
    // printf("%d: %d\n", b->estimate, b->remaining_count);
    if (b->remaining_count == 0 && b->cost < best_limit) {
      best_limit = b->cost;
    }
    for (i = 0; i < SIZE; ++i) {
      if (b->board[i] != 0) continue;
      Board* b2 = getBoard();
      BoardMap::iterator it;
      std::string key;
      *b2 = *b;
      b2->step = b->step + 1;
      pick(b2, i, b2->step);
      key = getHash(b2);
      it = memory.find(key);
      b2->estimate = b2->cost + b2->remaining_count * (b2->step + 1) * (b2->step + 1);
      if (b2->estimate >= best_limit) {
        returnBoard(b2);
        continue;
      }
      if (it == memory.end()) {
        ++m;
        memory.insert(std::pair<std::string, Board*>(key, b2));
        board_list.push(b2);
      } else {
        Board* old = it->second;
        if (old->cost > b2->cost) {
          it->second = b2;
          // returnBoard(old);
          board_list.push(b2);
        } else {
          returnBoard(b2);
        }
      }
    }
  }
  printf("%d %d\n", c, m);
}

void print_board(Board* b) {
  int i;
  char after;
  for (i = 0; i < SIZE; ++i) {
    after = (i % WIDTH) == WM ? '\n' : ' ';
    printf("%2d%c", b->board[i], after);
  }
}

int main(const int argc, const char * const argv[]) {
  int i, j, k, vi, ri;
  Board root;
  BoardMap::iterator it;
  std::string finalHash(SIZE, '1');
  Validation* v;
  for (i = 0; i < SIZE; ++i) {
    root.board[i] = 0;
    root.remaining[i] = true;
    for (j = 0; j < SIZE; ++j) {
      validations[i][j].position = -1;
    }
  }
  for (i = 0; i < SIZE; ++i) {
    // Horizontal
    vi = 0;
    for (j = (i - i % WIDTH); j < (i - i % WIDTH) + WIDTH; ++j) {
      if (i == j) continue;
      v = &validations[i][vi++];
      v->position = j;
      for(k = (i - i % WIDTH), ri = 0; k < (i - i % WIDTH) + WIDTH; ++k) {
        if (k == i || k == j) continue;
        v->requirements[ri++] = k;
      }
    }
    // Vertical
    for (j = i % WIDTH; j < SIZE; j += WIDTH) {
      if (i == j) continue;
      v = &validations[i][vi++];
      v->position = j;
      for(k = i % WIDTH, ri = 0; k < SIZE; k += WIDTH) {
        if (k == i || k == j) continue;
        v->requirements[ri++] = k;
      }
    }
    // Cross
    if (i % WIDTH == i / WIDTH) {
      for(j = 0; j < SIZE; j += WP) {
        if (i == j) continue;
        v = &validations[i][vi++];
        v->position = j;
        for(k = 0, ri = 0; k < SIZE; k += WP) {
          if (k == i || k == j) continue;
          v->requirements[ri++] = k;
        }
      }
    }
    // Reverse Cross
    if (i % WIDTH + i / WIDTH == WIDTH - 1) {
      for(j = WIDTH - 1; j < SIZE - 1; j += WM) {
        if (i == j) continue;
        v = &validations[i][vi++];
        v->position = j;
        for(k = WIDTH - 1, ri = 0; k < SIZE - 1; k += WM) {
          if (k == i || k == j) continue;
          v->requirements[ri++] = k;
        }
      }
    }
  }
  root.cost = 0;
  root.step = 0;
  root.remaining_count = SIZE;
  root.estimate = SIZE;
  board_list.push(&root);
  solve();
  printf("%d\n", (int)memory.size());
  
  it = memory.find(finalHash);
  if (it == memory.end()) {
    printf("Not found :(");
  } else {
    print_board(it->second);
  }
  // print_board(memory[]);
}
