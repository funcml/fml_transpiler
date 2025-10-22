#include <HsFFI.h>
#include <stddef.h>

// Declare the Haskell foreign exported symbols
extern HsPtr hs_malloc(HsInt);
extern HsPtr compile(HsPtr);

// Force export names for wasm
__attribute__((export_name("hs_malloc")))
HsPtr
wasm_hs_malloc(HsInt x)
{
    return hs_malloc(x);
}

__attribute__((export_name("compile")))
HsPtr
wasm_compile(HsPtr x)
{
    return compile(x);
}
