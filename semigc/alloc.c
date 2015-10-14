// Semi-space garbage collection routines, using Cheney scan
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "types.h"
#include "alloc.h"
#include "llvm_gc_support.h"
#include "class.h"

static byte *f_heap, *f_limit;
static byte *t_heap, *t_limit;
static byte *t_alloc;

struct forwarder
{
    ushort  type;       // Should be 0xffff
    void*   ptr;        // Points to an object in the to-space
};

void llvm_gc_initialize(unsigned int heapsz)
{
    printf("Initializing semi-space heap (%d bytes per space)\n", heapsz);

    f_heap = (byte*)malloc(heapsz);
    memset(f_heap, 0, heapsz);
    f_limit = f_heap + heapsz - 1;

    t_heap = (byte*)malloc(heapsz);
    memset(t_heap, 0, heapsz);
    t_limit = t_heap + heapsz - 1;
    t_alloc = t_heap;
}

void
llvm_gc_shutdown()
{
    printf("+-------------------------------\n");
    printf("| gc_shutdown()\n");
    printf("+-------------------------------\n");
    printf("| Heap still in use: %ld out of %ld bytes\n", t_alloc-t_heap, t_limit-t_heap+1);
    printf("+--------------------------\n");

    // Free both spaces
    free(f_heap);
    free(t_heap);
}

int object_in_from_space(struct C* obj)
{
    return ((byte*)obj >= f_heap && (byte*)obj < f_limit);
}

// Copy the given object (which is located in the
// from-space) to the next empty spot in the to-space.
// Leave a forwarding pointer in the from-space.
struct C* copy_object(struct C* obj)
{
    assert(object_in_from_space(obj) && "Object to copy not located in the from-space!");
    assert(obj->type != 0xffff && "Attempting to copy a forwarding pointer!");
    assert(t_alloc+sizeof(struct C) <= t_limit && "Not enough room to create object copy!");

    struct C* newloc = (struct C*)t_alloc;

    // Copy the object
    memcpy(newloc, obj, sizeof(struct C));
    t_alloc += sizeof(struct C);

    // Leave a forwarding pointer in the from-space, overwriting
    // the space occupied by the object just copied
    struct forwarder* forward = (struct forwarder*)obj;
    forward->type = 0xffff;
    forward->ptr = newloc;

    // XXX non-strict aliasing check
    assert(obj->type = 0xffff);

    // Return the new object location
    return newloc;
}

// Note: we use hard-coded knowledge of the object layout here (e.g. left_child, right_child).
// In a general collection routine the object layout would probably be fetched
// from some kind of metadata structure, based on the type of object being moved.
void llvm_gc_collect()
{
    printf("+-------------------------------\n");
    printf("| gc_collect()\n");
    printf("+-------------------------------\n");
    printf("| Heap use before collection: %ld out of %ld bytes\n", t_alloc-t_heap, t_limit-t_heap+1);

    // Swap 'from' and 'to' spaces

    byte *tmp;

    tmp = t_heap;
    t_heap = f_heap;
    f_heap = tmp;

    tmp = t_limit;
    t_limit = f_limit;
    f_limit = tmp;

    t_alloc = t_heap;

    // The new 'to' space is initially empty. Make sure it is cleared, so
    // newly allocated objects will have all their pointer fields set to null.

    memset(t_heap, 0, t_limit-t_heap+1);

    // Traverse roots and copy live objects from the
    // 'from' space to the 'to' space.
    //
    // Use a Cheney scan, as it is nicely iterative

    printf("| Processing stack map\n");

    int32_t             i, num_roots;
    struct C            *root, *newroot;
    struct StackEntry   *entry = llvm_gc_root_chain;

    while (entry)
    {
        num_roots = entry->Map->NumRoots;
        printf("| [0x%08x] %d root(s)\n", (unsigned int)entry, num_roots);

        for (i = 0; i < num_roots; i++)
        {
            root = (struct C*)entry->Roots[i];
            printf("| ... [%d] 0x%08x", i, (unsigned int)root);

            if (root)
            {
                assert(object_in_from_space(root));

                // It is possible for multiple roots to point to the same object.
                // When the first of those roots is processed in this loop and the
                // object pointed to has been moved the remaining roots will point
                // to a forwarder.
                if (root->type == 0xffff)
                {
                    // "object" is a forwarding pointer
                    printf(" ... forwarder to 0x%08x found, updating root", (unsigned int) ((struct forwarder*)root)->ptr);
                    // Update the root pointer
                    entry->Roots[i] = ((struct forwarder*)root)->ptr;
                }
                else
                {
                    // Copy object from the from-space to the to-space
                    newroot = copy_object(root);
                    printf(" ... object copied to 0x%08x", (unsigned int)newroot);

                    assert(root->type == 0xffff && "Copied object not replaced by forwarding pointer!");

                    // Update the root pointer
                    entry->Roots[i] = (struct C*)newroot;
                }
            }

            printf("\n");
        }

        entry = entry->Next;
    }

    // Now scan over the to-space, to make sure all objects referenced
    // from the objects copied above also end up in the to-space.
    // Any objects left in the from-space will be garbage.

    byte            *scanptr;
    struct C        *object, *child;

    printf("| Scanning objects in to-space\n");

    scanptr = t_heap;
    while (scanptr < t_alloc)
    {
        object = (struct C*) scanptr;
        printf("| [0x%08x]\n", (unsigned int)object);

        /*
        if (object->left_child)
        {
            child = object->left_child;
            assert(object_in_from_space(child));
            if (child->type == 0xffff)
            {
                // Update pointer in object, using forwarding pointer
                object->left_child = ((struct forwarder*)child)->ptr;
                printf("| ... left child references forwarder to 0x%08x, updating pointer\n", (unsigned int) ((struct forwarder*)root)->ptr);
            }
            else
            {
                // Child object referenced is still in from-space, copy
                // it and update the pointer we have
                printf("| ... copying left child 0x%08x", (unsigned int)(object->left_child));
                object->left_child = copy_object(child);
                printf(" to 0x%08x\n", (unsigned int)(object->left_child));
            }
        }
        else
            printf("| ... left child NULL\n");

        if (object->right_child)
        {
            child = object->right_child;
            assert(object_in_from_space(child));
            if (child->type == 0xffff)
            {
                // Update pointer in object, using forwarding pointer
                object->right_child = ((struct forwarder*)child)->ptr;
                printf("| ... right child references forwarder to 0x%08x, updating pointer\n", (unsigned int) ((struct forwarder*)root)->ptr);
            }
            else
            {
                // Child object referenced is still in from-space, copy
                // it and update the pointer we have
                printf("| ... copying right child 0x%08x", (unsigned int)(object->right_child));
                object->right_child = copy_object(child);
                printf(" to 0x%08x\n", (unsigned int)(object->right_child));
            }
        }
        else
            printf("| ... right child NULL\n");
          */

        // Move to next object in to-space
        scanptr += sizeof(struct C);
    }

    printf("| Heap use after collection: %ld out of %ld bytes\n", t_alloc-t_heap, t_limit-t_heap+1);

    // Mark the bytes of the current 'from' heap, so we can catch invalid
    // access through object pointers that weren't updated.

    memset(f_heap, 0xff, f_limit-f_heap+1);

    printf("+--------------------------\n");
}

void* llvm_gc_allocate(unsigned int sz)
{
    //printf("gc_alloc(%d)", sz);

    byte *res;

    if (t_alloc + sz > t_limit)
    {
        // Need to collect
        printf(" - not enough free heap space, forcing a collection ...\n");
        llvm_gc_collect();

        if (t_alloc + sz > t_limit)
        {
            printf("Fatal: not enough heap space after collection. Available space is %ld bytes, need %d bytes\n", t_limit-t_alloc+1, sz);
            exit(-1);
        }

        res = t_alloc;
        //printf("... new object at 0x%08x of size %d\n", (unsigned int)res, sz);
    }
    else
    {
        // Enough space available, simply increment
        res = t_alloc;
        t_alloc += sz;
        //printf(" - new object at 0x%08x, heap size now %ld bytes\n", (unsigned int)res, t_alloc-t_heap);
    }

    return res;
}
