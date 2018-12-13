#ifndef INCLUDED_DXZ_CAPI
#define INCLUDED_DXZ_CAPI

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef uint32_t item;
typedef uint32_t zdd;
typedef uint32_t option;

typedef struct dxz dxz;

extern dxz * dxz_new(int, int);
extern void dxz_delete(dxz *);
extern item dxz_add_items(dxz *, int);
extern item dxz_add_optional_items(dxz *, int);
extern option dxz_add_option(dxz *, item *, int);
extern zdd dxz_solve(dxz *);
extern const uint64_t * dxz_heap(const dxz *);

#ifdef __cplusplus
}
#endif

#endif
