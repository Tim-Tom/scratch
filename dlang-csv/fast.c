#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COUNT_SIZE 2048
#define BUFFER_SIZE 2048

int counts[COUNT_SIZE];

int main(int argc, const char* const argv[]) {
  const char* const filename = argv[1];
  const int ki = atoi(argv[2]);
  const int vi = atoi(argv[3]);
  const int mi = vi > ki ? vi : ki;
  char buffer[BUFFER_SIZE];
  FILE* inf = fopen(filename, "r");
  int i, year, count;
  char *field;
  int max, maxKey;
  while(fgets(buffer, BUFFER_SIZE, inf)) {
    for(i = 0, field = strtok(buffer, "\t\n"); i <= mi; i++, field = strtok(NULL, "\t\n")) {
      if (i == ki) {
        year = atoi(field);
      }
      if (i == vi) {
        count = atoi(field);
      }
    }
    if (year >= COUNT_SIZE) {
      fprintf(stderr, "Ooops, buffer isn't big enough: %d", year);
    }
    counts[year] += count;
  }
  max = -1;
  for(i = 0; i < COUNT_SIZE; ++i) {
    if (counts[i] > max) {
      maxKey = i;
      max = counts[i];
    }
  }
  printf("max_key: %d sum: %d\n", maxKey, max);
  return 0;
}
