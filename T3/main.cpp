#include <cmath>
#include <iomanip>
#include <iostream>

#include "utils/function/include/newton_cotes.h"

const int MAX_ITER = 100;
const double ERROR_PRECISION = 0.00001;
const unsigned int FLOAT_PRECISION = 10;

int main() {
  auto function = [](double x) {
    return (pow(sin(2 * x) + 4 * pow(x, 2) + 3 * x, 2));
  };

  NewtonCotes newton{function};
  // Function, a and b.
  {
    std::cout << "Closed Integral Values:\n";
    double error = 10;  // Any amount > 0.000001
    double iter = 0;
    double past_integration = newton.CalculateClosedIntegral(1, 0, 1);
    double number_of_partitions = 1;
    while (error > ERROR_PRECISION && iter < MAX_ITER) {
      number_of_partitions++;
      double integration =
          newton.CalculateClosedIntegral(number_of_partitions, 0, 1);
      error = fabs(((integration - past_integration) / integration));
      past_integration = integration;
      std::cout << std::setprecision(FLOAT_PRECISION) << "Iter: " << iter
                << " Partitons: " << number_of_partitions
                << " Integral: " << integration << " Error: " << error << '\n';
      ++iter;
    }
  }

  {
    std::cout << "Open Integral Values:\n";
    double error = 10;  // Any amount > 0.000001
    double iter = 0;
    double past_integration = newton.CalculateOpenIntegral(1, 0, 1);
    double number_of_partitions = 1;
    while (error > ERROR_PRECISION && iter < MAX_ITER) {
      number_of_partitions++;
      double integration =
          newton.CalculateOpenIntegral(number_of_partitions, 0, 1);
      error = fabs(((integration - past_integration) / integration));
      past_integration = integration;
      std::cout << std::setprecision(FLOAT_PRECISION) << "Iter: " << iter
                << " Partitons: " << number_of_partitions
                << " Integral: " << integration << " Error: " << error << '\n';
      ++iter;
    }
  }
}