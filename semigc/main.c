// Main program
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010

#include <stdlib.h>
#include <stdio.h>
#include "class.h"

// Add a left and right child to an object
void
add_children(struct C* c)
{
    // Need to use a GC root to point to c, to make sure it stays live, as
    // actions below could trigger a collection. If this collection causes
    // roots currently pointing (indirectly) to c to get collected we would
    // allow c to get collected as well.
    struct C __attribute__((gcroot)) *ccopy = c;

    // Just for fun we create some garbage here
    struct C __attribute__((gcroot)) *garbage = new_C(0);

    // Note that we use the variable copy that is marked gcroot here, not c itself.
    // This is needed as otherwise the value of the root can't be updated during
    // collection (the copying collector needs to alter the stack maps, as it moves
    // objects around)
    printf("* add_children()\n");
    printf("Before: c = 0x%08x, value = %d\n", (unsigned int)ccopy, ccopy->value);
    ccopy->left_child = new_C(2*ccopy->value);
    ccopy->right_child = new_C(2*ccopy->value + 1);
    printf("After : c = 0x%08x, value = %d\n", (unsigned int)ccopy, ccopy->value);

    // XXX NULL-ing gcroots is probably only necessary when there is a possibility
    // that a root is still around and a collection is forced before the end of
    // this function.
    ccopy = NULL;
    garbage = NULL;
}

void
print_tree(struct C* c, int indent)
{
    // In principle we should make a copy of c and mark the
    // copy as a GC root, but as we don't do any allocation here
    // (and we're not multithreading so a collection can't be triggered)
    // we can get away with it.

    for (int i = 0; i < indent; i++)
        printf(" ");

    if (!c)
        printf("NULL\n");
    else
    {
        printf("[0x%08x] %d\n", (unsigned int)c, c->value);
        print_tree(c->left_child, indent+2);
        print_tree(c->right_child, indent+2);
    }
}

struct C*
create_tree()
{
    struct C __attribute__((gcroot)) *r;
    struct C __attribute__((gcroot)) *s1;
    struct C __attribute__((gcroot)) *s2;

    r = new_C(1);
    s1 = new_C(2);
    s2 = new_C(3);
    add_children(s1);
    add_children(s2);
    r->left_child = s1;
    r->right_child = s2;

    return r;
}

int main()
{
    // Create a small heap
    gc_init(128);

    struct C __attribute__((gcroot)) *r;
    struct C __attribute__((gcroot)) *s;

    // Create a tree structure of objects

    printf("* Creating tree structure\n");

    r = create_tree();

    print_tree(r, 0);

    // Create garbage by sharing one of the subtree's nodes,
    // leaving the original r->left_child->right_child unreferenced

    printf("* Sharing a tree node\n");

    r->left_child->right_child = r->right_child->left_child;

    print_tree(r, 0);

    // We duplicate an object reference here, meaning that there will be
    // two roots pointing to the same object. When the first of these is
    // processed during collection the object will get copied to the other
    // semi-space and a forwarding pointer left in place of the original object.
    // At that point the second root will therefore point to the forwarder,
    // not the original object, something we want to make sure the
    // collector handles correctly.
    s = r->left_child;

    // Force a collection

    printf("* Running collection\n");
    gc_collect();

    print_tree(r, 0);

    // Now create more garbage by stepping down into one of the subtrees
    // with the root reference

    r = r->left_child;

    printf("* Creating garbage, tree is now\n");
    print_tree(r, 0);

    // Again add a second reference to one of the objects
    s = r;

    // Force a collection

    printf("* Running collection\n");
    gc_collect();

    print_tree(r, 0);

    // We're done, clean up

    printf("* Shutting down\n");

    gc_shutdown();

    return 0;
}
