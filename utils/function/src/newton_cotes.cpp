#include "utils/function/include/newton_cotes.h"

#include <cmath>
#include <functional>
#include <iostream>

NewtonCotes::NewtonCotes(std::function<double(double)> func, double start,
                         double end)
    : function_{func}, interval_start_{start}, interval_end_{end} {}

double NewtonCotes::OpenFormula(double xi, double xf) {
  double h = (xf - xi) / 6;
  return (h / 10 *
          (33 * function_(xi + h) - 42 * function_(xi + 2 * h) +
           78 * function_(xi + 3 * h) - 42 * function_(xi + 4 * h) +
           33 * function_(xi + 5 * h)));
}

double NewtonCotes::ClosedFormula(double xi, double xf) {
  double h = (xf - xi) / 4;
  return (h / 45 *
          (14 * function_(xi) + 64 * function_(xi + h) +
           24 * function_(xi + 2 * h) + 64 * function_(xi + 3 * h) +
           14 * function_(xf)));
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

double NewtonCotes::CalculatePreciseIntegral(double precision) {
  double error = 10;  // Any amount > 0.000001
  double past_integration = CalculateClosedIntegral(1);
  double number_of_partitions = 1;
  double integration;
  while (error > precision) {
    number_of_partitions++;
    integration = CalculateClosedIntegral(number_of_partitions);
    error = fabs(((integration - past_integration) / integration));
    past_integration = integration;
  }
  return past_integration;
}