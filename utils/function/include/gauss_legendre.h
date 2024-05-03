#ifndef UTILS_FUNCTION_INCLUDE_GAUSS_LEGENDRE_H
#define UTILS_FUNCTION_INCLUDE_GAUSS_LEGENDRE_H
#include <functional>
#include <vector>

class GaussLegendre {
 public:
  GaussLegendre(std::function<double(double)> function, std::vector<double> w_,
                std::vector<double> alpha_);
  double CalculateIntegral(unsigned int partitions, double interval_start_,
                           double interval_end_);
  double CalculatePreciseIntegral(double precision, double interval_start_,
                                  double interval_end_);

 protected:
  std::function<double(double)> function_;
  double ComputeGaussLegendreIntegration(double xi, double xf);
  std::vector<double> w_;
  std::vector<double> alpha_;
};

#endif