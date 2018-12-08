#include <algorithm>
#include <vector>
#include <cstdint>
#include <map>
#include <iostream>
#include <assert.h>

// compute exact covers using dancing links

using namespace std;

typedef std::uint32_t link;

struct cell {
  // in the cell header, parity stores whether we've already used this column
  std::uint32_t parity:1, column:31;
  link u,d;
  cell(std::int32_t parity, std::int32_t column, link u, link d)
  : parity(parity), column(column), u(u), d(d) {}
};

struct column {
  link p,n;
  column(link p, link n)
  : p(p), n(n) {}
};

struct torus {
  uint32_t columns, total_columns;
  uint32_t starting_column;
  std::vector<cell> cells;
  std::vector<uint32_t> counts;
  std::vector<column> links;

  constexpr uint32_t pred_mod(uint32_t i) const noexcept {
    return (i+columns-1)%columns;
  }

  constexpr uint32_t succ_mod(uint32_t i) const noexcept {
    return (i+1)%columns;
  }

  torus(uint32_t columns, uint32_t optional_columns) noexcept
  : columns(columns)
  , total_columns(columns + optional_columns)
  , starting_column(0)
  , cells()
  , counts(columns,0)
  , links() {
    assert(total_columns>0);
    cells.reserve(total_columns+2);
    links.reserve(columns);

    // build cells, and two sentinels
    for (uint32_t i=0;i<total_columns+2;++i)
      cells.emplace_back(0,i,i,i);

    // mark the cell<->row sentinel, so we can identify rows
    cells[total_columns].parity = 1;

    // link the primary columns in a cycle
    for (uint32_t i=0;i<columns;++i)
      links.emplace_back(pred_mod(i), succ_mod(i));

    // link the secondary columns to themselves
    for (uint32_t i=columns;i<total_columns;++i)
      links.emplace_back(i,i);
  }

  template <typename T>
  uint32_t add_row(T values) {
    auto base = cells.size()-1;
    auto parity = cells[base].parity;
    cells.pop_back(); // drop the current terminating sentinel
    uint32_t i = 0;
    for (auto j : values) {
      assert(j < total_columns);
      auto u = cells[j].u;
      cells.emplace_back(parity,j,u,j);
      cells[u].d = cells[j].u = base + i++;
      inc(j);
    }
    cells.emplace_back(!parity,base+i,base+i,base+i);
    return base;
  }

  uint32_t add_row(std::initializer_list<uint32_t> values) noexcept {
    auto base = cells.size()-1;
    auto parity = cells[base].parity;
    cells.pop_back(); // drop the current terminating sentinel
    uint32_t i = 0;
    for (auto j : values) {
      assert(j < total_columns);
      auto u = cells[j].u;
      cells.emplace_back(parity,j,u,j);
      cells[u].d = cells[j].u = base + i++;
      inc(j);
    }
    cells.emplace_back(!parity,base+i,base+i,base+i);
    return base;
  }

  void inc(uint32_t column) noexcept {
    assert(column < total_columns);
    ++counts[column];
  }

  bool mark(uint32_t column) noexcept {
    assert(column < total_columns);
    if (cells[column].parity) return true;
    cells[column].parity = 1;
    auto &cell = links[column];
    links[cell.n].p = cell.p;
    links[cell.p].n = cell.n;
    return false;
  }

  void release(uint32_t column) noexcept {
    assert(column < total_columns);
    assert(cells[column].parity);
    cells[column].parity = 0;
    auto &cell = links[column];
    links[cell.p].n = column;
    links[cell.n].p = column;
  }

  // unlink a single cell and mark the column it is in
  // returns true on conflict and if so, does _not_ remove the link.
  bool unlink(uint32_t i) noexcept {
    assert(i < cells.size());
    auto & cell = cells[i];
    if (mark(cell.column)) return true;
    cells[cell.u].d = cell.d;
    cells[cell.d].u = cell.u;
    return false;
  }

  // must be done in the opposite order of unlink.
  void relink(uint32_t i) noexcept {
    assert(i < cells.size());
    auto & cell = cells[i];
    release(cell.column);
    cells[cell.u].d = i;
    cells[cell.d].u = i;
  }

  // returns 0 on conflict, row# of the row containing the cell otherwise
  uint32_t unlink_row_containing(uint32_t cell) noexcept {
    assert(0<cell&&cell<cells.size());
    auto parity = cells[cell].parity;
    auto i=cell;
    uint32_t row_id;
    if (parity) {
      for (;cells[i].parity;--i)
        if (unlink(i)) {
          while (i++<cell) relink(i);
          return 0;
        }
      row_id = i+1;
      for (i=cell+1;cells[i].parity;++i)
        if (unlink(i)) {
          while (cells[--i].parity) relink(i);
          return 0;
        }
    } else {
      for (;!cells[i].parity;--i)
        if (unlink(i)) {
          while (i++<cell) relink(i);
          return 0;
        }
      row_id = i+1;
      for (i=cell+1;!cells[i].parity;++i)
        if (unlink(i)) {
          while (!cells[--i].parity) relink(i);
          return 0;
        }
    }
    return row_id; // ok
  }

  template <typename Fn> void for_row(uint32_t row, Fn f) {
    assert(row<cells.size()); // valid cell
    assert(cells[row].parity != cells[row-1].parity); // is row
    auto parity = cells[row].parity;
    if (parity)
      for (auto i=row;cells[i].parity;++i)
        f(i);
    else
      for (auto i=row;!cells[i].parity;++i)
        f(i);
  }

  void relink_row(uint32_t row) noexcept {
    for_row(row, [&](uint32_t c) { relink(c); });
  }

  // blech
  void sort_columns() noexcept {
    std::vector<uint32_t> by_count;

    for (uint32_t i=0;i<columns;++i)
      by_count.emplace_back(i);

    std::sort(by_count.begin(),by_count.end(), [&](uint32_t i, uint32_t j) noexcept {
      assert(i<columns);
      assert(j<columns);
      return counts[i] < counts[j];
    });

    for (uint32_t i=0;i<columns;++i) {
      auto c = by_count[i];
      links[c].p = by_count[pred_mod(i)];
      links[c].n = by_count[succ_mod(i)];
    }
    starting_column = by_count[0];
  }

  template <typename OStream> OStream & show_row(OStream & os, uint32_t row) noexcept {
    bool first = true;
    for_row(row, [&](uint32_t i) noexcept {
      os << (first ? '{' : ',');
      first = false;
      os << cells[i].column;
    });
    return os << '}';
  }
};

struct recursive_solver {
  torus & problem;
  std::vector<uint32_t> result; // rows

  recursive_solver(torus & t) : problem(t), result() {}

  template <typename Fn>
  void solve(Fn f) {
    // check to make sure we have _any_ mandatory columns first
    if (problem.columns) solve(f,problem.starting_column);
    else f(result); // otherwise the empty solution is a solution
  }

  template <typename Fn>
  void solve(Fn f, uint32_t col) {
    if (problem.cells[col].parity) {
      f(result);
      return;
    }
    auto candidate = problem.cells[col].d;
    while (candidate != col) {
      auto row = problem.unlink_row_containing(candidate);
      if (row) {
        result.emplace_back(row);
        solve(f, problem.links[col].n);
        result.pop_back();
        problem.relink_row(row);
      }
      candidate = problem.cells[candidate].d;
    }
  }
};

template <typename OStream> OStream & operator << (OStream & os, const torus & t) {
  os << t.columns << '|' << t.total_columns << " cells: " << t.cells.size() << '\n';

  for (auto c : t.cells) {
    os << '{' << c.parity << ',' << c.column << ',' << c.u << ',' << c.d << "}\n";
  }
  return os;
}

void queens(uint32_t n) {
  uint32_t nn = n+n-2;
//  auto organ = [=](int i) { return (i&1 ? n-1-i : n+i) >> 1; };
  auto organ = [=](uint32_t i) { return i; };

  auto row = [=](uint32_t i) { return organ(i); };
  auto col = [=](uint32_t i) { return n + organ(i); };
  auto a = [=](uint32_t i) { return 2*n + i; };
  auto b = [=](uint32_t i) { return 2*n + nn + i; };
  auto x = torus(2*n,2*nn);
  std::vector<uint32_t> option;
  std::map<uint32_t,std::tuple<uint8_t,uint8_t>> rows;
  for(uint8_t j=0;j<n;++j) {
    option.resize(0);
    option.emplace_back(row(j));
    for (uint8_t k=0;k<n;++k) {
      option.resize(1);
      option.emplace_back(col(k));
      uint8_t t = j+k;
      if (t && t < nn) option.emplace_back(a(t));
      t = n-1-j+k;
      if (t && t < nn) option.emplace_back(b(t));
      rows.emplace(x.add_row(option),std::tuple<uint8_t,uint8_t>(j,k)); // interpret the row
    }
  }
  x.sort_columns();
  auto y = recursive_solver(x);
  y.solve([&](std::vector<uint32_t> & is) {
    bool first = true;
    for (auto i : is) {
      auto result = rows.find(i);
      if (result != rows.end()) {
        auto & p = rows.find(i)->second;
        if (!first) std::cout << ' ';
        std::cout << int(std::get<0>(p)) << ',' << int(std::get<1>(p));
        first = false;
      } else {
        std::cout << "!!!";
      }
    }
    std::cout << '\n';
  });
}

void simple() {
  auto x = torus(4,2);
  x.add_row({0,1,3,4,5});
  x.add_row({2});
  x.add_row({3});
  x.add_row({0,1});
  x.sort_columns(); // lame
  // std::cout << x;
  auto y = recursive_solver(x);
  y.solve([&](std::vector<uint32_t> & is) {
    bool first = true;
    for (auto i : is) {
      if (!first) std::cout << ' ';
      x.show_row(std::cout, i);
      first = false;
    }
    std::cout << '\n';
  });
}

int main(int argc, char ** argv) {
  queens(12);
}
