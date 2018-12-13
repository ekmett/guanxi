#include <vector>
#include <iostream>
#include "dxz.hpp"

int queens(uint32_t n) {
  dxz x;
  auto rows = x.add_items(n);
  auto cols = x.add_items(n);
  uint32_t nn = n+n-2;
  auto diagonals1 = x.add_optional_items(nn);
  auto diagonals2 = x.add_optional_items(nn);
  // "organ pipe" order
  auto organ = [=](int i) { return (i&1?n-1-i:n+i)>>1; };
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
      if (t && t < nn) option.emplace_back(diagonals1+t);
      t = n-1-r+c;
      if (t && t < nn) option.emplace_back(diagonals2+t);
      x.add_option(option.begin(),option.end());
    }
  }
  return x.solve();
}

int main(int argc, char ** argv) {
  int n = argc < 2 ? 8 : atoi(argv[1]);
  if (n >= 1) std::cout << queens(n) << " zdd nodes required\n";
}
