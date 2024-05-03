#include "utils/function/include/singularity_integration.h"

#include <cmath>
#include <iostream>

#include "utils/function/include/newton_cotes.h"

using std::cosh;
using std::sinh;
using std::tanh;

// Esse método está correto!
SingularityIntegration::SingularityIntegration(std::function<double(double)> f,
                                               double a, double b,
                                               ExponentialType exp) {
  if (exp == ExponentialType::Simple) {
    auto x = [a, b](double s) {
      return ((a + b) / 2. + ((b - a) / 2.) * tanh(s));
    };

    auto dx = [a, b](double s) { return (((b - a) / 2) / (pow(cosh(s), 2))); };

    function_ = [f, x, dx](double s) { return (f(x(s)) * dx(s)); };

  } else {
    auto x = [a, b](double s) {
      return ((a + b) / 2) + ((b - a) / 2 * tanh(M_PI_2 * (sinh(s))));
    };
    auto dx = [a, b](double s) {
      return ((b - a) / 2 *
              (M_PI_2 * cosh(s) / pow(cosh(M_PI_2 * sinh(s)), 2)));
    };
    function_ = [f, x, dx](double s) { return (f(x(s)) * dx(s)); };
  }
}

double SingularityIntegration::CalculatePreciseIntegral(
    double interval_precision, double inner_precision) {
  constexpr double kLimit{10000000};
  double c = 1;
  NewtonCotes newton{function_};
  double past_integration =
      newton.CalculatePreciseIntegral(inner_precision, -c, c);

  double error = 10;
  double integration;
  while (error > interval_precision) {
    c *= 1.1;
    integration = newton.CalculatePreciseIntegral(inner_precision, -c, c);
    if (integration < -kLimit || integration > kLimit) {
      return past_integration;
    }
    error = fabs(((integration - past_integration) / integration));
    past_integration = integration;
  }

  return past_integration;
}
