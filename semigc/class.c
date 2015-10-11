// Class routines
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010
#include "class.h"

// Return a new instance of a class
struct C*
new_C(int value)
{
    struct C __attribute__((gcroot)) *ptr;
    ptr = (struct C*)gc_alloc(sizeof(struct C));

    ptr->type = 0x0001;
    ptr->value = value;
    return ptr;
}
