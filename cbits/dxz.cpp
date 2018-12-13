#include "dxz.hpp"

// compute exact covers using dancing links
// and zero suppressed binary decision diagrams

using namespace std;

dxz::dxz(uint32_t n, uint32_t k) noexcept
  : cells(0)
  , items(0)
  , item_mask(n+k,true)
  , heap{zdd_node(),zdd_node()} // reserve terminals
  , cache()
  , memo() {
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
item dxz::add_items(uint32_t k) noexcept {
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
    item_mask.emplace_back(true);
    cells.emplace_back(parity,itembase+i,cellbase+i,cellbase+i);
    items.emplace_back(
      i ? itembase+(i+k-1)%k : itembase+k,
      itembase+(i+1)%k,
      cellbase+i,
      0
    );
  }
  items.emplace_back(was_empty ? itembase+k-1 : items[n].p, itembase, 0, 0);
  if (!was_empty) {
    items[n].p = itembase+k-1;
    items[p].n = itembase+k;
    items[itembase+k-1].n = n;
  } else {
    items[itembase+k-1].n = itembase+k;
  }
  cells.emplace_back(~parity);
  return itembase;
}

// dynamically add k items to the problem that may be satisfied
// but can only be satisfied once
item dxz::add_optional_items(uint32_t k) noexcept {
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
    item_mask.emplace_back(true);
    cells.emplace_back(parity,itembase+i,cellbase+i,cellbase+i);
    items.emplace_back(itembase+i, itembase+i, cellbase+i,0);
  }
  items.emplace_back(p, n, 0, 0);
  cells.emplace_back(~parity);
  items[p].n = itembase+i;
  items[n].p = itembase+i;
  return itembase;
}

option dxz::add_option(std::initializer_list<item> values) noexcept {
  return add_option(values.begin(),values.end());
}

option dxz::pick(link c) noexcept {
  return for_option_containing(c, [&](link i) noexcept {
    auto & x = cells[i];
    auto & item = items[x.item];
    auto header = item.cell;
    items[item.n].p = item.p;
    items[item.p].n = item.n;
    item_mask[x.item] = false;
    for (auto j = x.u; j != header; j = cells[j].u)
      for_option_containing_exclusive(j, [&](link k) noexcept {
        auto & y = cells[k];
        cells[y.u].d = y.d;
        cells[y.d].u = y.u;
      });
    for (auto j = x.d; j != header; j = cells[j].d)
      for_option_containing_exclusive(j, [&](link k) noexcept {
        auto & y = cells[k];
        cells[y.u].d = y.d;
        cells[y.d].u = y.u;
      });
  });
}

void dxz::unpick(link c) noexcept {
  for_option_containing(c, [&](link i) noexcept {
    auto & x = cells[i];
    auto & item = items[x.item];
    auto header = item.cell;
    items[item.n].p = x.item;
    items[item.p].n = x.item;
    item_mask[x.item] = true;
    for (auto j = x.u; j != header; j = cells[j].u)
      for_option_containing_exclusive(j, [&](link k) noexcept {
        auto & y = cells[k];
        cells[y.u].d = k;
        cells[y.d].u = k;
      });
    for (auto j = x.d; j != header; j = cells[j].d)
      for_option_containing_exclusive(j, [&](link k) noexcept {
        auto & y = cells[k];
        cells[y.u].d = k;
        cells[y.d].u = k;
      });
  });
}

item dxz::best_item() const noexcept {
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

zdd dxz::solve() noexcept {
  auto item = best_item();
  if (item == root())
    return top;
  auto it = memo.find(item_mask);
  if (it != memo.end()) return it->second;
  zdd x = bottom;
  auto header = items[item].cell;
  auto candidate = cells[header].d;
  while (candidate != header) {
    auto row = pick(candidate);
    zdd y = solve();
    if (y != bottom) x = unique(row,x,y);
    unpick(row);
    candidate = cells[candidate].d;
  }
  memo.emplace(item_mask, x);
  return x;
}

zdd dxz::unique(option label, zdd lo, zdd hi) noexcept {
  auto entry = zdd_node(label,lo,hi);
  auto it = cache.find(entry);

  if (it != cache.end())
    return it->second;

  zdd slot = heap.size();
  heap.emplace_back(entry);
  cache.emplace(entry,slot);
  return slot;
}
