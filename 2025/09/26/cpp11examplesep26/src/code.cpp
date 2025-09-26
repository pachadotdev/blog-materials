#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
void fun() {}

[[cpp11::register]]
int sum_int(int a, int b) {
  return a + b;
}

[[cpp11::register]]
int sum_integers(integers a, integers b) {
  int x = a[0]; // R: a[1] or a[[1]]
  int y = b[0];
  return x + y;
}
