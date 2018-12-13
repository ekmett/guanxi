#include "dxz_capi.h"
#include "dxz.hpp"

extern "C" {
  struct dxz * dxz_new(int n, int k) {
    return new dxz(n,k);
  }
  void dxz_delete(dxz * p) {
    delete p;
  }
  item dxz_add_items(dxz * p, int n) {
    return p->add_items(n);
  }
  item dxz_add_optional_items(dxz * p, int k) {
    return p->add_optional_items(k);
  }
  option dxz_add_option(dxz * p, item * is, int n) {
    return p->add_option(is,is+n);
  }
  zdd dxz_solve(dxz * p) {
     return p->solve();
  }
  const uint64_t * dxz_heap(const dxz * p) {
     return reinterpret_cast<const uint64_t*>(p->heap.data());
  }
};
