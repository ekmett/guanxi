#ifndef INCLUDED_DXZ_HPP
#define INCLUDED_DXZ_HPP

#include <algorithm>
#include <vector>
#include <cstdint>
#include <map>
#include <iostream>
#include <assert.h>
#include <unordered_map>
#include "dlx.hpp"

// compute exact covers using dancing links

using namespace std;

typedef uint32_t zdd;

struct zdd_node {
  union {
    struct {
      uint64_t label : 12, lo : 26, hi : 26;
    };
    uint64_t value;
  };
  zdd_node() noexcept : value(0) {}
  zdd_node(zdd_node && rhs) noexcept : value(std::move(rhs.value)) {}
  zdd_node(const zdd_node & rhs) noexcept : value(rhs.value) {}
  zdd_node(option label, zdd lo, zdd hi) noexcept
    : label(label), lo(lo), hi(hi) {}
  
  bool operator == (const zdd_node & rhs) const noexcept {
    return value == rhs.value;
  }
};


namespace std {
  template <> struct hash<zdd_node> {
    std::size_t operator()(const zdd_node & n) const noexcept {
      uint64_t k = n.value;
      k ^= k >> 33;
      k *= 0xff51afd7ed558ccdULL;
      k ^= k >> 33;
      k *= 0xc4ceb9fe1a85ec53ULL;
      k ^= k >> 33;
      return k;
    }
  };
}

using namespace std;

struct dxz {
  std::vector<cell> cells;
  std::vector<item_info> items;
  std::vector<bool> item_mask;
  std::vector<zdd_node> heap;
  std::unordered_map<zdd_node, zdd> cache;
  std::unordered_map<std::vector<bool>, zdd> memo;

  static constexpr const zdd bottom = 0;
  static constexpr const zdd top = 1;

  static constexpr bool terminal(zdd z) noexcept { return z <= top; }

  dxz(std::uint32_t n=0, std::uint32_t k=0) noexcept;

  item add_items(std::uint32_t k) noexcept;

  item add_optional_items(std::uint32_t k) noexcept;

  // add an option to the
  template <typename Iterator> option add_option(Iterator first, Iterator last);

  template <typename T> option add_option(T values) {
    return add_option(values.begin(),values.end());
  }

  option add_option(std::initializer_list<item> values) noexcept;

  zdd solve() noexcept;

  zdd unique(option label, zdd lo, zdd hi) noexcept;

  zdd_node decode(zdd z) const noexcept { return heap[z]; }

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
void dxz::for_option_containing_exclusive(link cell, Fn f) noexcept {
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
option dxz::for_option_containing(link cell, Fn f) noexcept {
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


template <typename Iterator> option dxz::add_option(Iterator first, Iterator last) {
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
