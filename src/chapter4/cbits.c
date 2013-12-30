/* cbits
$ gcc -fPIC -shared cbits.c -o cbits.so
$ clang -fPIC -shared cbits.c -o cbits.so
*/

#include "stdio.h"

// putchard - putchar that takes a double and returns 0.
double putchard(double X) {
  putchar((char)X);
  fflush(stdout);
  return 0;
}
