#include "utils/function/include/gauss_legendre.h"

#include <cmath>
#include <iostream>

GaussLegendre::GaussLegendre(std::function<double(double)> function,
                             double interval_start, double interval_end,
                             std::vector<double> w, std::vector<double> alpha)
    : function_{function},
      interval_start_{interval_start},
      interval_end_{interval_end},
      w_{w},
      alpha_{alpha} {}

double GaussLegendre::ComputeGaussLegendreIntegration(double xi, double xf) {
  auto x = [xi, xf](double alpha) {
    return ((xi + xf) / 2 + alpha * (xf - xi) / 2);
  };

  int roots_count = alpha_.size();
  double result = 0;
  for (int i = 0; i < roots_count; ++i) {
    result += function_(x(alpha_[i])) * w_[i];
  }

  return ((xf - xi) / 2 * result);
}

double GaussLegendre::CalculateIntegral(unsigned int partitions) {
  double dx = (interval_end_ - interval_start_) / partitions;
  double s = 0;
  double xi, xf;
  for (int i = 0; i < partitions; ++i) {
    xi = interval_start_ + i * dx;
    xf = xi + dx;
    s = s + ComputeGaussLegendreIntegration(xi, xf);
  }
  return s;
}

double GaussLegendre::CalculatePreciseIntegral(double precision) {
  double error = 10;  // Any amount > 0.000001
  double past_integration = CalculateIntegral(1);
  double number_of_partitions = 1;
  double integration;
  while (error > precision) {
    number_of_partitions++;
    integration = CalculateIntegral(number_of_partitions);
    error = fabs(((integration - past_integration) / integration));
    past_integration = integration;
  }
  return past_integration;
}