#include "popcount.h"
int popcount(unsigned int w) {
    return __builtin_popcount(w);
}
