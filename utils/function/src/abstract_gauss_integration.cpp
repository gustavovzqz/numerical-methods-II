#include "utils/function/include/abstract_gauss_integration.h"

#include <cmath>
#include <iostream>

AbstractGaussIntegration::AbstractGaussIntegration(
    std::function<double(double)> function, std::vector<double> w,
    std::vector<double> alpha)
    : function_{function}, w_{w}, alpha_{alpha} {}

double AbstractGaussIntegration::ComputeAbstractGaussIntegration() {
  int roots_count = alpha_.size();
  double result = 0;
  for (int i = 0; i < roots_count; ++i) {
    result += function_(alpha_[i]) * w_[i];
  }

  return (result);
}

double AbstractGaussIntegration::CalculateIntegral() {
  return (ComputeAbstractGaussIntegration());
}
