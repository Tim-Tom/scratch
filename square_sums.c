#include <stdio.h>
#include <stdlib.h>

#define NUM_FOURTHS 1001
static long fourths[NUM_FOURTHS];

int cmp_long(const long* const a, const long* const b) {
   long res = *a - *b;
   return (res < 0) ? -1 : ((res > 0) ? 1 : 0);
}

int main() {
   long a, b, x, ys;
   long* yp;
   for(a = 0; a < NUM_FOURTHS; ++a) {
      fourths[a] = a*a*a*a;
   }
   for(a = 1; a < NUM_FOURTHS - 2; ++a) {
      for(b = a + 2; b < NUM_FOURTHS; ++b) {
         for (x = a + 1; x < b; ++x) {
            ys = fourths[a] + fourths[b] - fourths[x];
            yp = (long*)bsearch(&ys, fourths, NUM_FOURTHS, sizeof(long), cmp_long);
            if (NULL != yp) {
               printf("%d**4 + %d**4 = %d**4 + %d**4\n", a, b, x, yp-fourths);
               return 1;
            }
         }
      }
   }
   printf("Done Searching\n");
   return 0;
}
