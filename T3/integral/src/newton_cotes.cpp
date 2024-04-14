#include "newton_cotes.h"

#include <functional>

NewtonCotes::NewtonCotes(Function func, double start, double end)
    : function_{func}, interval_start_{start}, interval_end_{end} {}

double NewtonCotes::OpenFormula(double xi, double xf) {
  double h = (xf - xi) / 6;
  return (h / 10 *
          (33 * function_.Calculate(xi + h) -
           42 * function_.Calculate(xi + 2 * h) +
           78 * function_.Calculate(xi + 3 * h) -
           42 * function_.Calculate(xi + 4 * h) +
           33 * function_.Calculate(xi + 5 * h)));
}

double NewtonCotes::ClosedFormula(double xi, double xf) {
  double h = (xf - xi) / 4;
  return (h / 45 *
          (14 * function_.Calculate(xi) + 64 * function_.Calculate(xi + h) +
           24 * function_.Calculate(xi + 2 * h) +
           64 * function_.Calculate(xi + 3 * h) +
           14 * function_.Calculate(xf)));
}

double NewtonCotes::CalculateOpenIntegral(unsigned int partitions) {
  double dx = (interval_end_ - interval_start_) / partitions;
  double s = 0;
  double xi, xf;
  for (int i = 0; i < partitions; ++i) {
    xi = interval_start_ + i * dx;
    xf = xi + dx;
    s = s + OpenFormula(xi, xf);
  }
  return s;
}

double NewtonCotes::CalculateClosedIntegral(unsigned int partitions) {
  double dx = (interval_end_ - interval_start_) / partitions;
  double s = 0;
  double xi, xf;
  for (int i = 0; i < partitions; ++i) {
    xi = interval_start_ + i * dx;
    xf = xi + dx;
    s = s + ClosedFormula(xi, xf);
  }
  return s;
}