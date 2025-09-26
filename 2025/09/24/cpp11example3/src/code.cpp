#include <cpp11.hpp>
using namespace cpp11;

int a_plus_b_ (integers a, integers b) {
 int x = a[0];
 int y = b[0];
 return x + y;
}

size_t a_plus_b_2_ (integers a, integers b) {
 int x = a[0];
 int y = b[0];
 return x + y;
}

[[cpp11::register]] int a_plus_b (integers a, integers b) {
    // return a_plus_b_(a,b);
    return a_plus_b_2_(a,b);
}
