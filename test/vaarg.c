
// TODO: To support this we need to support some builtin functions first

#include <stdarg.h>

int foo(int count, ...) {
    va_list ap;
    int j;
    double sum = 0;

    va_start(ap, count); /* Requires the last fixed parameter (to get the address) */
    for (j = 0; j < count; j++) {
        sum += va_arg(ap, double); /* Increments ap to the next argument. */
    }
    va_end(ap);

    return sum / count;
}

int main(void) {
  return 0;
}
