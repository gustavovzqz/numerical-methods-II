#include "utils/function/include/function.h"

#include <cmath>

Function::Function(double dx, std::function<double(double)> calculate_lambda)
    : dx{dx}, calculate_{calculate_lambda} {}

double Function::Calculate(double x) { return calculate_(x); }

// Central

double Function::SecondDerivative(double x) {
  return ((Calculate(x + dx) + Calculate(x - dx) + 5 * Calculate(x + 2 * dx) +
           5 * Calculate(x - 2 * dx) - Calculate(x + 3 * dx) -
           Calculate(x - 3 * dx) - 10 * Calculate(x)) /
          (12 * dx * dx));
}

// Second order derivatives

double Function::SecondDerivativeForward(double x) {
  return (2 * Calculate(x) - 5 * Calculate(x + dx) + 4 * Calculate(x + 2 * dx) -
          Calculate(x + 3 * dx)) /
         (dx * dx);
}

double Function::SecondDerivativeBackward(double x) {
  return (2 * Calculate(x) - 5 * Calculate(x - dx) + 4 * Calculate(x - 2 * dx) -
          Calculate(x - 3 * dx)) /
         (dx * dx);
}
