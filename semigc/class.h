// Class layout
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010

#ifndef CLASS_H
#define CLASS_H

#include "types.h"
#include "alloc.h"

struct C
{
    // Type tag
    ushort      type;

    // Object attributes
    int         value;
    struct C    *left_child, *right_child;
};

struct C*   new_C();

#endif
