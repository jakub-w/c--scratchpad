#ifndef CPP_SCRATCHPAD_UTILS_H
#define CPP_SCRATCHPAD_UTILS_H

#include <chrono>
#include <functional>

namespace cpps {
// Print buffer as a hex string (to stdout).
void print_hex(unsigned char* buffer, size_t size) {
  for (int i = 0; i < size; ++i) {
    char s[3];
    snprintf(s, 3, "%02x", buffer[i]);
    std::cout.write(s, 2);
  }
  std::cout << '\n';
}

// Count the time it takes the function f to complete (in milliseconds).
template <typename Func, typename ...Args>
void time(Func&& f, Args&&... args) {
  auto start = std::chrono::high_resolution_clock::now();
  std::invoke(std::forward<Func>(f), std::forward<Args>(args)...);
  auto end = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double, std::ratio<1, 1000>> duration = (end - start);
  std::cout << "Time elapsed: " << duration.count() << " ms\n";
}
}

#endif // CPP_SCRATCHPAD_UTILS_H
