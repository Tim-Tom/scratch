#include "stdio.h"
#include "stdlib.h"

int main(int argc, char** ARGV) {
   char buffer[1024];
   while(1) {
      if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
         printf("%s", buffer);
      } else {
         printf("eof\n");
      }
      sleep(1);
   }
   return 0;
}
