#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define NUM_SIMULATIONS 1000000
#define MAX_GENERATIONS 10

int simulate() {
  int generation = 0;
  int count = 2;
  while(generation++ < MAX_GENERATIONS && count) {
    int new_count = 0;
    for (int i = 0; i < count; ++i) {
      if (rand() % 4) {
        new_count += 2;
      }
    }
    count = new_count;
  }
  // printf("generation %d | count %d\n", generation, count);
  return count == 0;
}

int main(int argc, const char* const argv[]) {
  int count = 0;
  srand(time(NULL));
  for (int sim = 0; sim < NUM_SIMULATIONS; ++sim) {
    if (simulate()) {
      ++count;
    }
  }
  double death_prob =  ((double)count / (double)NUM_SIMULATIONS);
  printf("Death: %.4f\n", 100.0*death_prob);
  return 0;
}
