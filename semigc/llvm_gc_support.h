// These datastructures represent the shadow stack as maintained by LLVM.
// Based on LLVM docs and probably some code using the LLVM shadow stack,
// but I can't really remember which code anymore.
//
// Paul E.C. Melis (paul@floorball-flamingos.nl), March 31st, 2010

#ifndef LLVM_GC_SUPPORT_H
#define LLVM_GC_SUPPORT_H

//typedef unsigned int int32_t;

// Note w.r.t the Roots[] array:
// For roots [0, NumMeta), the metadata pointer is in the FrameMap.
// For roots [NumMeta, NumRoots), the metadata pointer is NULL.
struct FrameMap
{
    int32_t NumRoots;               //< Number of roots in stack frame.
    int32_t NumMeta;                //< Number of metadata entries. May be < NumRoots.
    const void *Meta[0];            //< Metadata for each root.
};

struct StackEntry
{
    struct StackEntry *Next;        //< Link to next stack entry (the caller's).
    const struct FrameMap *Map;     //< Pointer to constant FrameMap.
    void *Roots[0];                 //< Stack roots (in-place array).
};

// The root of the current stack chain
struct StackEntry *llvm_gc_root_chain;

#endif
