#include <cmath>
#include <iomanip>
#include <iostream>

#include "function.h"

const int MAX_ITER = 100;
const double ERROR_PRECISION = 0.00001;
const int x = 2;

int main() {
  Function f{0.5};
  double error = 10;  // Any amount > 0.000001
  double past_second_derivative = f.SecondDerivative(x);
  int iter = 0;
  while (error > ERROR_PRECISION && iter < MAX_ITER) {
    f.dx = (f.dx / 2);
    double second_derivative = f.SecondDerivative(x);
    error = fabs(
        ((second_derivative - past_second_derivative) / second_derivative));
    past_second_derivative = second_derivative;
    std::cout << "Iter: " << iter << " Delta: " << f.dx
              << " f(x): " << f.Calculate(x) << " f''(x): " << std::fixed
              << second_derivative << " Error: " << error << '\n';
    ++iter;
  }

  return 0;
}