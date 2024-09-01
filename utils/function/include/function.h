#ifndef UTILS_FUNCTION_INCLUDE_FUNCTION_H
#define UTILS_FUNCTION_INCLUDE_FUNCTION_H
#include <functional>

struct Function {
  Function(double dx, std::function<double(double)> calculate_lambda);
  double Calculate(double x);
  double SecondDerivative(double x);
  double SecondDerivativeForward(double x);
  double SecondDerivativeBackward(double x);
  double dx;
  std::function<double(double)> calculate_;
};

#endif
