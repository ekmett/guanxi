#include <algorithm>
#include <vector>
#include <cstdint>
#include <map>
#include <iostream>
#include <assert.h>

// compute exact covers using dancing links

using namespace std;

typedef uint32_t option;

struct cell {
  uint32_t parity:1, item:31, u, d;
  cell(int32_t parity=0, uint32_t item=0, uint32_t u=0, uint32_t d=0)
  : parity(parity), item(item), u(u), d(d) {}
};

struct item {
  uint32_t p, n, cell, count;
  item(uint32_t p=0, uint32_t n=0, uint32_t cell=0, uint32_t count=0)
  : p(p), n(n), cell(cell), count(count) {}
};


struct dlx {
  std::vector<cell> cells;
  std::vector<item> items;
  std::vector<option> result; // rows

  dlx(uint32_t n=0, uint32_t k=0) noexcept
  : cells(0)
  , items(0)
  , result(0) {
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
  uint32_t add_items(uint32_t k) {
    if (!k) return items.size()-1; // how many columns do we have

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
  uint32_t add_optional_items(uint32_t k) {
    if (!k) return items.size()-1; // how many columns do we have
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

  // add an option to the
  template <typename Iterator>
  uint32_t add_option(Iterator first, Iterator last) {
    auto base = cells.size()-1;
    auto parity = cells[base].parity;
    cells.pop_back(); // drop terminating sentinel
    uint32_t i = 0;
    for (Iterator it = first; it != last; ++it) {
      uint32_t j = *it;
      assert(j < items.size()-1); // exclude root
      auto u = cells[j].u;
      cells.emplace_back(parity,j,u,j);
      cells[u].d = cells[j].u = base + i++;
      ++items[cells[j].item].count; // bump counts of the columns
    }
    cells.emplace_back(!parity);
    return base;
  }

  template <typename T>
  uint32_t add_option(T values) {
    return add_option(values.begin(),values.end());
  }

  uint32_t add_option(std::initializer_list<uint32_t> values) noexcept {
    return add_option(values.begin(),values.end());
  }
  
  template <typename Fn> 
  void for_row_containing_exclusive(uint32_t cell, Fn f) noexcept {
    auto parity = cells[cell].parity;
    auto i=cell-1;
    if (parity) {
      for (;cells[i].parity;--i) f(i);
      for (i=cell+1;cells[i].parity;++i) f(i);
    } else {
      for (;!cells[i].parity;--i) f(i);
      for (i=cell+1;!cells[i].parity;++i) f(i);
    }
  }

  // returns row# of the row containing the cell
  template <typename Fn> 
  uint32_t for_row_containing(uint32_t cell, Fn f) noexcept {
    auto parity = cells[cell].parity;
    auto i=cell;
    uint32_t row_id=0;
    if (parity) {
      for (;cells[i].parity;--i) f(i);
      row_id = i+1;
      for (i=cell+1;cells[i].parity;++i) f(i);
    } else {
      for (;!cells[i].parity;--i) f(i);
      row_id = i+1;
      for (i=cell+1;!cells[i].parity;++i) f(i);
    }
    return row_id; // ok
  }

  uint32_t pick(uint32_t c) {
    return for_row_containing(c, [&](uint32_t i) {
      auto & x = cells[i];
      auto & col = items[x.item];
			auto header = col.cell;
      items[col.n].p = col.p;
      items[col.p].n = col.n;
      for (auto j = x.u; j != header; j = cells[j].u)
        for_row_containing_exclusive(j, [&](uint32_t k) {
          auto & y = cells[k];
          cells[y.u].d = y.d;
          cells[y.d].u = y.u;
        });
      for (auto j = x.d; j != header; j = cells[j].d)
        for_row_containing_exclusive(j, [&](uint32_t k) {
          auto & y = cells[k];
          cells[y.u].d = y.d;
          cells[y.d].u = y.u;
        });
    });
  }

  void unpick(uint32_t c) {
    for_row_containing(c, [&](uint32_t i) {
      auto & x = cells[i];
      auto & col = items[x.item];
			auto header = col.cell;
      items[col.n].p = x.item;
      items[col.p].n = x.item;
      for (auto j = x.u; j != header; j = cells[j].u)
        for_row_containing_exclusive(j, [&](uint32_t k) {
          auto & y = cells[k];
          cells[y.u].d = k;
          cells[y.d].u = k;
        });
      for (auto j = x.d; j != header; j = cells[j].d)
        for_row_containing_exclusive(j, [&](uint32_t k) {
          auto & y = cells[k];
          cells[y.u].d = k;
          cells[y.d].u = k;
        });
    });
  }

  constexpr uint32_t root() const { return items.size()-1; }

  uint32_t best_column() const {
    uint32_t best = root();
    uint32_t best_count = INT32_MAX;
    for (uint32_t i = items[root()].n; i != root(); i = items[i].n) {
       uint32_t count = items[i].count;
       if (count < best_count) {
         best_count = count;
         best = i;
       }
    }
    return best;
  }

  template <typename Fn>
  void solve(Fn f) {
    uint32_t col = best_column();
    if (col == root()) {
      f((std::vector<uint32_t> const &)result); // otherwise the empty solution is a solution
      return;
    }
    auto header = items[col].cell;
    auto candidate = cells[header].d;
    while (candidate != header) {
      auto row = pick(candidate);
      if (row) {
        result.emplace_back(row);
        solve(f);
        result.pop_back();
        unpick(row);
      }
      candidate = cells[candidate].d;
    }
  }
};

void queens(uint32_t n) {
  auto x = dlx();
	auto rows = x.add_items(n);
  auto cols = x.add_items(n);
  uint32_t nn = n+n-2;
	auto a = x.add_optional_items(nn);
	auto b = x.add_optional_items(nn);
  auto organ = [&](int i) { return (i&1?n-1-i:n+i)>>1; };
  std::vector<uint32_t> option;
  for(uint8_t j=0;j<n;++j) {
    option.resize(0);
    int r = organ(j);
    option.emplace_back(rows + r);
    for (uint8_t k=0;k<n;++k) {
      option.resize(1);
			int c = organ(k);
      option.emplace_back(cols + c);
      uint8_t t = r+c;
      if (t && t < nn) option.emplace_back(a+t);
      t = n-1-r+c;
      if (t && t < nn) option.emplace_back(b+t);
      x.add_option(option.begin(),option.end());
    }
  }
  x.solve([&](const std::vector<uint32_t> & is) {
    bool first = true;
    for (auto i : is) {
      if (!first) std::cout << ' ';
      std::cout << (x.cells[i].item-rows) << ',' << (x.cells[i+1].item-cols);
      first = false;
    }
    std::cout << '\n';
  });
}

int main(int argc, char ** argv) {
  queens(12);
}
