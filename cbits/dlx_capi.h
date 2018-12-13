#ifndef INCLUDED_DLX_CAPI
#define INCLUDED_DLX_CAPI

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef uint32_t item;

typedef uint32_t option;

typedef struct dlx dlx;

extern dlx * dlx_new(int, int);

extern void dlx_delete(dlx *);

extern item dlx_add_items(dlx *, int);

extern item dlx_add_optional_items(dlx *, int);

extern option dlx_add_option(dlx *, item *, int);

/**
 * Extract the next solution.
 *
 * returns 0 if no solution, otherwise,
 * changes the pointers to point to the current solution
 * advancing to the next solution invalidates these pointers
 */
extern int dlx_next(dlx *, item **, int *);

#ifdef __cplusplus
}
#endif

#endif
