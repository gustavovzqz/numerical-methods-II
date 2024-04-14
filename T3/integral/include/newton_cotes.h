#ifndef T3_INTEGRAL_INCLUDE_NEWTON_COTES_H
#define T3_INTEGRAL_INCLUDE_NEWTON_COTES_H
#include "utils/function/function.h"

class NewtonCotes {
 public:
  NewtonCotes(Function function, double interval_start, double interval_end);
  double CalculateOpenIntegral(unsigned int partitions);
  double CalculateClosedIntegral(unsigned int partitions);

 private:
  Function function_;
  double interval_start_;
  double interval_end_;
  double OpenFormula(double xi, double xf);
  double ClosedFormula(double xi, double xf);
};

#endif