#include <cmath>
#include <iomanip>
#include <iostream>

#include "utils/function/include/singularity_integration.h"

int main() {
  double constexpr kIntervalPrecision{0.00006};
  double constexpr kInnerPrecision{0.00006};

  auto first_example = [](double x) { return (1. / cbrt(x * x)); };
  auto second_example = [](double x) { return (1 / (sqrt(4 - pow(x, 2)))); };

  // First example

  SingularityIntegration first_simple{first_example, 0, 1,
                                      ExponentialType::Simple};
  std::cout << std::setprecision(10) << "First example - simple: "
            << first_simple.CalculatePreciseIntegral(kIntervalPrecision,
                                                     kInnerPrecision)
            << '\n';

  SingularityIntegration first_double{first_example, 0, 1,
                                      ExponentialType::Double};

  std::cout << "First example - double: "
            << first_double.CalculatePreciseIntegral(kIntervalPrecision,
                                                     kInnerPrecision)
            << '\n';

  // Second example

  SingularityIntegration second_simple{second_example, -2, 0,
                                       ExponentialType::Simple};
  std::cout << "Second example - simple: "
            << second_simple.CalculatePreciseIntegral(kIntervalPrecision,
                                                      kInnerPrecision)
            << '\n';

  SingularityIntegration second_double{second_example, -2, 0,
                                       ExponentialType::Double};

  std::cout << "Second example - double: "
            << second_double.CalculatePreciseIntegral(kIntervalPrecision,
                                                      kInnerPrecision)
            << '\n';

  return 0;
}