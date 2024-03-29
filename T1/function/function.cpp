#include "function.h"

#include <cmath>

Function::Function(double dx) : dx{dx} {}

// Change this to change the function!
double Function::Calculate(double x) {
  return (sqrt(exp(3 * x) + 4 * pow(x, 2)));
}

double Function::SecondDerivative(double x) {
  return ((Calculate(x + dx) + Calculate(x - dx) + 5 * Calculate(x + 2 * dx) +
           5 * Calculate(x - 2 * dx) - Calculate(x + 3 * dx) -
           Calculate(x - 3 * dx) - 10 * Calculate(x)) /
          (12 * dx * dx));
}
