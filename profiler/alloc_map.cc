#include <bits/stdc++.h>
using namespace std;

static map<void *, size_t> mp;
static int totalAllocSize = 0;

extern "C" int c_malloc_table(void *addr, size_t size) {
  mp.insert({addr, size});
  totalAllocSize+=size;
}

extern "C" int c_free_table(void *addr) {

  size_t size;

  auto itr =  mp.find(addr);
  if (itr != mp.end()) {
    mp.erase(itr);
    size = itr->second;

    totalAllocSize-=size;
  }

}

extern "C" size_t returnTotalMem() {
  return totalAllocSize;
}

  
