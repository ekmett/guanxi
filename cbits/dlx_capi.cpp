#include "dlx_capi.h"
#include "dlx.hpp"

extern "C" {
  struct dlx * dlx_new(int n, int k) {
    return new dlx(n,k);
  }
  void dlx_delete(dlx * p) {
    delete p;
  }
  item dlx_add_items(dlx * p, int n) {
    return p->add_items(n);
  }
  item dlx_add_optional_items(dlx * p, int k) {
    return p->add_optional_items(k);
  }
  option dlx_add_option(dlx * p, item * is, int n) {
    return p->add_option(is,is+n);
  }
  int dlx_next(dlx * p, item ** results, int * nresults) {
     return p->next(*results,*nresults);
  }
  void dlx_reset(dlx * p) {
     p->reset();
  }
  int dlx_count(dlx * p) {
     return p->count();
  }
};
