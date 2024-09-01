#include <cmath>
#include <iomanip>
#include <iostream>

#include "utils/function/include/function.h"

const int MAX_ITER = 100;
const double ERROR_PRECISION = 0.00001;

int main() {
  auto calculate_lambda = [](double x) {
    return (sqrt(exp(3 * x) + 4 * pow(x, 2)));
  };
  Function f{0.5, calculate_lambda};
  double error = 10;  // Any amount > 0.000001
  int iter = 0;
  double point = 2.;
  double past_second_derivative = f.SecondDerivative(point);
  while (error > ERROR_PRECISION && iter < MAX_ITER) {
    f.dx = (f.dx / 2);
    double second_derivative = f.SecondDerivative(point);
    error = fabs(
        ((second_derivative - past_second_derivative) / second_derivative));
    past_second_derivative = second_derivative;
    std::cout << "Iter: " << iter << " Delta: " << f.dx
              << " f(x): " << f.Calculate(point) << " f''(x): " << std::fixed
              << second_derivative << " Error: " << error << '\n';
    ++iter;
  }

  return 0;
}