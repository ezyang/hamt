#include "popcount.h"
inline int popcount(unsigned int w) {
    return __builtin_popcount(w);
}
