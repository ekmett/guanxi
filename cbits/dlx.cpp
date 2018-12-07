#include <algorithm>
#include <vector>
#include <cstdint>
#include <map>
#include <iostream>

// compute exact covers using dancing links

/*              01234567890
                     CFHIABDEGJ

                     2212212112
  ABCDEFGHIJ|KLM     ABCDEFGHIJ0
  1100001001 010 +   AB    G  J
  0100100010 000 -    B  E
  0011010100 000 +     CD F H
  0000100010 100 +       E   I
  1001001001 000 -   A  D  G  J0
*/

using namespace std;

typedef std::int32_t link;

struct cell {
  std::int32_t parity:1, column:31;
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
  int columns, total_columns;
  int starting_column;
  std::vector<cell> cells;
  std::vector<int> counts;
  std::vector<column> links;

  constexpr int pred_mod(int i) const noexcept {
    return (i+columns-1)%columns;
  }

  constexpr int succ_mod(int i) const noexcept {
    return (i+1)%columns;
  }

  torus(int columns, int optional_columns) noexcept
  : columns(columns)
  , total_columns(columns + optional_columns)
  , starting_column(0)
  , cells()
  , counts(columns,0)
  , links() {
    cells.reserve(total_columns+2);
    links.reserve(columns);

    for (int i=0;i<total_columns;++i)
      cells.emplace_back(0,i,i,i);

    for (int i=0;i<columns;++i)
      links.emplace_back(pred_mod(i), succ_mod(i));

    for (int i=columns;i<total_columns;++i)
      links.emplace_back(i,i);

    cells.emplace_back(1,-1,-1,-1); // sentinel between the columns and rows
    cells.emplace_back(0,-1,-1,-1); // sentinel after all of the rows
  }

  template <typename T>
  int add_row(T values) {
    auto base = cells.size()-1;
    auto parity = cells[base].parity;
    cells.pop_back(); // drop the current terminating sentinel
    int i = 0;
    for (auto j : values) {
      auto u = cells[j].u;
      cells.emplace_back(parity,j,u,j);
      cells[u].d = cells[j].u = base + i++;
      inc(j);
    }
    cells.emplace_back(!parity,-1,-1,-1); // restore the sentinel after all of the rows
    return base;
  }

  int add_row(std::initializer_list<int> values) noexcept {

    auto base = cells.size()-1;
    auto parity = cells[base].parity;
    cells.pop_back(); // drop the current terminating sentinel
    int i = 0;
    for (auto j : values) {
      auto u = cells[j].u;
      cells.emplace_back(parity,j,u,j);
      cells[u].d = cells[j].u = base + i++;
      inc(j);
      // TODO: this should shuffle links incrementally
      // rather than make me pay for it at the end
    }
    cells.emplace_back(!parity,-1,-1,-1); // restore the sentinel after all of the rows
    return base;
  }

  void inc(int column) noexcept {
    ++counts[column];
  }

  // TODO: make pithy
  bool mark(int column) noexcept {
    if (cells[column].parity) return true;
    cells[column].parity = 1;
    auto &cell = links[column];
    links[cell.n].p = cell.p;
    links[cell.p].n = cell.n;
    return false;
    //
  } // mark a column used, return true if already used

  void release(int column) noexcept {
    cells[column].parity = 0;
    auto &cell = links[column];
    links[cell.p].n = column;
    links[cell.n].p = column;
  } // unmark a used column

  // unlink a single cell and mark the column it is in
  // returns true on conflict and if so, does _not_ remove the link.
  bool unlink(int i) noexcept {
    auto & cell = cells[i];
    if (mark(cell.column)) return true;
    cells[cell.u].d = cell.d;
    cells[cell.d].u = cell.u;
    return false;
  }

  // must be done in the opposite order of unlink.
  void relink(int i) noexcept {
    auto & cell = cells[i];
    release(cell.column);
    cells[cell.u].d = i;
    cells[cell.d].u = i;
  }

  // returns 0 on conflict
  int unlink_row_containing(int cell) noexcept {
    auto parity = cells[cell].parity;
    int i=cell;
    int row_id;
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

  template <typename Fn> void for_row(int row, Fn f) {
    auto parity = cells[row].parity;
    if (parity)
      for (int i=row;cells[i].parity;++i)
        f(i);
    else
      for (int i=row;!cells[i].parity;++i)
        f(i);
  }

  void relink_row(int row) noexcept {
    for_row(row, [&](int c) { relink(c); });
  }

  // blech
  void sort_links() noexcept {
    std::vector<int> by_count;

    for (int i=0;i<columns;++i)
      by_count.emplace_back(i);

    std::sort(by_count.begin(),by_count.end(), [&](int i, int j) noexcept {
      return counts[i] <= counts[j];
    });

    for (int i=0;i<columns;++i) {
      auto c = by_count[i];
      links[c].p = by_count[pred_mod(i)];
      links[c].n = by_count[succ_mod(i)];
    }
    starting_column = by_count[0];
  }

  template <typename OStream> OStream & show_row(OStream & os, int row) noexcept {
    bool first = true;
    for_row(row, [&](int i) noexcept {
      os << (first ? '{' : ',');
      first = false;
      os << cells[i].column;
    });
    return os << '}';
  }
};

struct recursive_solver {
  torus & problem;
  std::vector<int> result; // rows

  recursive_solver(torus & t) : problem(t), result() {}

  template <typename Fn>
  void solve(Fn f) {
    solve(f,problem.starting_column);
  }

  template <typename Fn>
  void solve(Fn f, int col) {
    if (problem.cells[col].parity) {
      f(result);
      return;
    }
    int candidate = problem.cells[col].d;
    while (candidate != col) {
      int row = problem.unlink_row_containing(candidate);
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

void queens(int n) {
  std::cout << n << " queens\n";
  int nn = n+n-2;
//  auto organ = [=](int i) { return (i&1 ? n-1-i : n+i) >> 1; };
  auto organ = [=](int i) { return i; };

  auto row = [=](int i) { return organ(i); };
  auto col = [=](int i) { return n + organ(i); };
  auto a = [=](int i) { return 2*n + i; };
  auto b = [=](int i) { return 2*n + nn + i; };
  auto x = torus(2*n,2*nn);
  std::vector<int> option;
  std::map<int,std::tuple<int,int>> rows;
  for(int j=0;j<n;++j) {
    option.resize(0);
    option.emplace_back(row(j));
    for (int k=0;k<n;++k) {
      option.resize(1);
      option.emplace_back(col(k));
      int t = j+k;
      if (t && t < nn) option.emplace_back(a(t));
      t = n-1-j+k;
      if (t && t < nn) option.emplace_back(b(t));
      rows.emplace(x.add_row(option),std::tuple(j,k)); // interpret the row
    }
  }
  x.sort_links();
  auto y = recursive_solver(x);
  y.solve([&](std::vector<int> & is) {
    bool first = true;
    for (auto i : is) {
      auto result = rows.find(i);
      if (result != rows.end()) {
        auto & [r,c] = rows.find(i)->second;
        if (!first) std::cout << ' ';
        std::cout << r << ',' << c;
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
  x.sort_links(); // lame
  // std::cout << x;
  auto y = recursive_solver(x);
  y.solve([&](std::vector<int> & is) {
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
  queens(8);
  // simple();
}
