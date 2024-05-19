#include <iostream>
#include "../include/s.hpp"

int main() {
    auto f = [](int x) { return [x](int y) { return x + y; }; };
    auto g = [](int x) { return x * 2; };

    auto s_comb = S<decltype(f), decltype(g), int>(f, g);

    std::cout << s_comb(3) << std::endl;
    return 0;
}