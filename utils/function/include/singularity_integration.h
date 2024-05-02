#ifndef UTILS_FUNCTION_INCLUDE_INGULARITY_INTEGRATION_H
#define UTILS_FUNCTION_INCLUDE_SINGULARITY_INTEGRATION_H
#include <functional>

enum class ExponentialType { Simple, Double };

class SingularityIntegration {
 public:
  SingularityIntegration(std::function<double(double)> function,
                         double interval_start, double interval_end,
                         ExponentialType exp);
  double CalculatePreciseIntegral(double interval_precision,
                                  double inner_precision);

  std::function<double(double)> function_;
};

#endif