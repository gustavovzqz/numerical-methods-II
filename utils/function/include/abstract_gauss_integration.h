#ifndef UTILS_FUNCTION_INCLUDE_ABSTRACT_GAUSS_INTEGRATION_H
#define UTILS_FUNCTION_INCLUDE_ABSTRACT_GAUSS_INTEGRATION_H
#include <functional>
#include <vector>

class AbstractGaussIntegration {
 public:
  AbstractGaussIntegration(std::function<double(double)> function,
                           std::vector<double> w_, std::vector<double> alpha_);
  double CalculateIntegral();

 protected:
  std::function<double(double)> function_;
  double ComputeAbstractGaussIntegration();
  std::vector<double> w_;
  std::vector<double> alpha_;
};
#endif
