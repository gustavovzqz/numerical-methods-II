#ifndef FUNCTION_FUNCTION_H
#define FUNCTION_FUNCTION_H

struct Function {
  Function(double dx);
  double Calculate(double x);
  double SecondDerivative(double x);
  double dx;
};

#endif
