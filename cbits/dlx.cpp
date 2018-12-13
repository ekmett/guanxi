#include "dlx.hpp"

// compute exact covers using dancing links

using namespace std;

dlx::dlx(uint32_t n, uint32_t k) noexcept
  : cells(0)
  , items(0)
  , result(0)
  , current_state(state::guessing) {
  uint32_t N = n+k;
  cells.reserve(N?N+1:2);
  items.reserve(N+1);

  for (uint32_t i=0;i<n;++i) {
    cells.emplace_back(1,i,i,i);
    items.emplace_back((i+n-1)%n, (i+1)%n, i, 0);
  }

  for (uint32_t i=n;i<N;++i) {
    cells.emplace_back(1,i,i,i);
    items.emplace_back(i,i,i,0);
  }

  cells.emplace_back(0); // terminating sentinel

  if (n) {
    items.emplace_back(n-1,0);
    items[0].p = items[n-1].n = N;
  } else {
    items.emplace_back(N,N);
  }
}

// dynamically add k items to the problem that must be satisfied
// but can only be satisfied once
item dlx::add_items(uint32_t k) {
  if (!k) return items.size()-1; // how many items do we have

  auto cellbase = cells.size()-1;
  auto parity = cells[cellbase].parity;
  auto itembase = items.size()-1;
  auto p = items[itembase].p; // grab old root prev
  auto n = items[itembase].n; // grab old root next

  items[p].n = n; // unlink root
  items[n].p = p;

  bool was_empty = n == itembase;

  cells.pop_back();
  items.pop_back();

  uint32_t i = 0;
  for (;i<k;++i) {
    cells.emplace_back(parity,itembase+i,cellbase+i,cellbase+i);
    items.emplace_back(i ? itembase+(i+k-1)%k : itembase+k, itembase+(i+1)%k, cellbase+i, 0);
  }
  items.emplace_back(was_empty ? itembase+k-1 : items[n].p, itembase, 0, 0);
  if (!was_empty) {
    items[n].p = itembase+k-1;
    items[itembase+k-1].n = n;
  } else {
    items[itembase+k-1].n = itembase+k;
  }
  cells.emplace_back(~parity);
  return itembase;
}

// dynamically add k items to the problem that may be satisfied
// but can only be satisfied once
item dlx::add_optional_items(uint32_t k) {
  if (!k) return items.size()-1; // how many items do we have
  auto cellbase = cells.size()-1;
  auto parity = cells[cellbase].parity;
  cells.pop_back();
  auto itembase = items.size()-1;
  auto p = items[itembase].p; // grab old root prev
  if (p == itembase) p = itembase + k;
  auto n = items[itembase].n; // grab old root next
  if (n == itembase) n = itembase + k;
  items.pop_back();
  uint32_t i = 0;
  for (;i<k;++i) {
    cells.emplace_back(parity,itembase+i,cellbase+i,cellbase+i);
    items.emplace_back(itembase+i, itembase+i, cellbase+i,0);
  }
  items.emplace_back(p, n, 0, 0);
  cells.emplace_back(~parity);
  items[p].n = itembase+i;
  items[n].p = itembase+i;
  return itembase;
}

option dlx::add_option(std::initializer_list<item> values) noexcept {
  return add_option(values.begin(),values.end());
}

option dlx::pick(link c) noexcept {
  return for_option_containing(c, [&](link i) {
    auto & x = cells[i];
    auto & item = items[x.item];
    auto header = item.cell;
    items[item.n].p = item.p;
    items[item.p].n = item.n;
    for (auto j = x.u; j != header; j = cells[j].u)
      for_option_containing_exclusive(j, [&](link k) {
        auto & y = cells[k];
        cells[y.u].d = y.d;
        cells[y.d].u = y.u;
      });
    for (auto j = x.d; j != header; j = cells[j].d)
      for_option_containing_exclusive(j, [&](link k) {
        auto & y = cells[k];
        cells[y.u].d = y.d;
        cells[y.d].u = y.u;
      });
  });
}

void dlx::unpick(link c) noexcept {
  for_option_containing(c, [&](link i) {
    auto & x = cells[i];
    auto & item = items[x.item];
    auto header = item.cell;
    items[item.n].p = x.item;
    items[item.p].n = x.item;
    for (auto j = x.u; j != header; j = cells[j].u)
      for_option_containing_exclusive(j, [&](link k) {
        auto & y = cells[k];
        cells[y.u].d = k;
        cells[y.d].u = k;
      });
    for (auto j = x.d; j != header; j = cells[j].d)
      for_option_containing_exclusive(j, [&](link k) {
        auto & y = cells[k];
        cells[y.u].d = k;
        cells[y.d].u = k;
      });
  });
}

item dlx::best_item() const noexcept {
  item best = root();
  uint32_t best_count = INT32_MAX;
  for (item i = items[root()].n; i != root(); i = items[i].n) {
     uint32_t count = items[i].count;
     if (count < best_count) {
       best_count = count;
       best = i;
     }
  }
  return best;
}

bool dlx::next(item * & results, int & nresults) noexcept {
  for (;;)
    switch (current_state) {
    case state::done:
      current_state = state::guessing;
      return false;

    case state::guessing:
      {
        int best = best_item();
        if (best == root()) {
          current_state = state::done;
          results = result.data();
          nresults = result.size();
          return true;
        }
        auto header = items[best].cell;
        auto candidate = cells[header].d;
        if (candidate == header) {
          current_state = state::backtracking;
        } else {
          stack.emplace_back(candidate);
          result.emplace_back(pick(candidate));
        }
        break;
      }
    case state::backtracking:
      if (stack.size() == 0) {
        current_state == state::guessing;
        return false;
      } else {
        auto bad_choice = stack[stack.size()-1];
        unpick(bad_choice);
        auto & bad = cells[bad_choice];
        stack.pop_back();
        result.pop_back();
        auto header = items[bad.item].cell;
        auto next_choice = bad.d;
        if (bad.d != header) {
          stack.emplace_back(bad.d);
          result.emplace_back(pick(bad.d));
          current_state = state::guessing;
        }
        break;
      }
    }
}
