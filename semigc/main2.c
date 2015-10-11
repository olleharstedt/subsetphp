// Main program 2
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010

#include <stdlib.h>
#include <stdio.h>
#include "types.h"
#include "alloc.h"
#include "class.h"

int main()
{

  for (int i = 0; i < 1000; i++) {
    // Must manually add to gcroot
    struct C *ptr;
    ptr = (struct C*) llvm_gc_allocate(sizeof(struct C));

    ptr->type = 0x0001;
    ptr->value = 1;
  }

  return 0;
}
