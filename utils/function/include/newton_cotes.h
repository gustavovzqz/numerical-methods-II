#ifndef T3_INTEGRAL_INCLUDE_NEWTON_COTES_H
#define T3_INTEGRAL_INCLUDE_NEWTON_COTES_H
#include <functional>

class NewtonCotes {
 public:
  NewtonCotes(std::function<double(double)> function, double interval_start,
              double interval_end);
  double CalculateOpenIntegral(unsigned int partitions);
  double CalculateClosedIntegral(unsigned int partitions);
  double CalculatePreciseIntegral(double precision);

 private:
  std::function<double(double)> function_;
  double interval_start_;
  double interval_end_;
  double OpenFormula(double xi, double xf);
  double ClosedFormula(double xi, double xf);
};

#endif