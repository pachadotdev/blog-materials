#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
int add1_(int x) {
    int y = x + 1;
    return y;
}
