#include <cpp11.hpp>
using namespace cpp11;

int a_plus_b_ (int a, int b) {
 return a + b;
}

[[cpp11::register]] int a_plus_b (integers a, integers b) {
    // if (a.size() > 1) {
    //     stop("a length > 1");
    // }

    // if (b.size() > 1) {
    //     stop("b length > 1");
    // }

    int c = a[0];
    int d = b[0];
    return a_plus_b_(c,d);
}
