#ifndef INCLUDED_DLX_HPP
#define INCLUDED_DLX_HPP

#include <algorithm>
#include <vector>
#include <cstdint>
#include <map>
#include <iostream>
#include <assert.h>

// compute exact covers using dancing links

using namespace std;

typedef std::uint32_t link;
typedef std::uint32_t item;
typedef std::uint32_t option;

struct cell {
	std::uint32_t parity:1, item:31, u, d;
  cell(uint32_t parity=0, std::uint32_t item=0, link u=0, link d=0)
  : parity(parity), item(item), u(u), d(d) {}
};

struct item_info {
  std::uint32_t p, n, cell, count;
  item_info(item p=0, item n=0, link cell=0, std::uint32_t count=0)
  : p(p), n(n), cell(cell), count(count) {}
};

enum class state {
  guessing, backtracking
};

struct dlx {
  std::vector<cell> cells;
  std::vector<item_info> items;
  std::vector<option> result; // rows
  std::vector<link> stack; // actual selections
  state current_state;

  dlx(std::uint32_t n=0, std::uint32_t k=0) noexcept;

  item add_items(std::uint32_t k);

  item add_optional_items(std::uint32_t k);

  // add an option to the
  template <typename Iterator> option add_option(Iterator first, Iterator last);

  template <typename T> option add_option(T values) {
    return add_option(values.begin(),values.end());
  }

  option add_option(std::initializer_list<item> values) noexcept;

  bool next(item * & results, int & nresults) noexcept;

  void reset() noexcept; // reset so that next starts from the start

  int count() noexcept; // count the solutions

  template <typename Fn> void solve(Fn f);

private:
  
  template <typename Fn> option for_option_containing(link cell, Fn f) noexcept;
  template <typename Fn> void for_option_containing_exclusive(link cell, Fn f) noexcept;

  option pick(link c) noexcept;
  void unpick(link c) noexcept;

  inline item root() const noexcept { return items.size()-1; }
  item best_item() const noexcept;

};

// --------------------------------------------------------------------------------
// implementation details
// --------------------------------------------------------------------------------

template <typename Fn> 
void dlx::for_option_containing_exclusive(link cell, Fn f) noexcept {
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
option dlx::for_option_containing(link cell, Fn f) noexcept {
  auto parity = cells[cell].parity;
  auto i=cell;
  option option_id=0;
  if (parity) {
    for (;cells[i].parity;--i) f(i);
    option_id = i+1;
    for (i=cell+1;cells[i].parity;++i) f(i);
  } else {
    for (;!cells[i].parity;--i) f(i);
    option_id = i+1;
    for (i=cell+1;!cells[i].parity;++i) f(i);
  }
  return option_id; // ok
}

template <typename Fn>
void dlx::solve(Fn f) {
  auto item = best_item();
  if (item == root()) {
    f(const_cast<std::vector<option> const &>(result)); // otherwise the empty solution is a solution
    return;
  }
  auto header = items[item].cell;
  auto candidate = cells[header].d;
  while (candidate != header) {
    auto row = pick(candidate);
    result.emplace_back(row);
    solve(f);
    result.pop_back();
    unpick(row);
    candidate = cells[candidate].d;
  }
}

template <typename Iterator> option dlx::add_option(Iterator first, Iterator last) {
  auto base = cells.size()-1;
  auto parity = cells[base].parity;
  cells.pop_back(); // drop terminating sentinel
  std::uint32_t i = 0;
  for (Iterator it = first; it != last; ++it) {
    item j = *it;
    assert(j < items.size()-1); // exclude root
    auto u = cells[j].u;
    cells.emplace_back(parity,j,u,j);
    cells[u].d = cells[j].u = base + i++;
    ++items[cells[j].item].count; // bump counts of the items
  }
  cells.emplace_back(!parity);
  return base;
}

#endif
