#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>

#include "utils/function/include/gauss_legendre.h"

constexpr double kPrecision{0.000006};

int main() {
  auto fun = [](double x) {
    return (pow(sin(2 * x) + 4 * pow(x, 2) + 3 * x, 2));
  };

  std::cout << "2 Points Gauss Legendre: ";

  std::vector<double> two_points_alpha{-sqrt(1. / 3.), sqrt(1. / 3.)};
  std::vector<double> two_points_w{1., 1.};
  GaussLegendre gauss_legendre_2p{fun, 0, 1, two_points_w, two_points_alpha};

  std::cout << std::setprecision(10)
            << gauss_legendre_2p.CalculatePreciseIntegral(kPrecision) << '\n';

  std::cout << "3 Points Gauss Legendre: ";

  std::vector<double> three_points_alpha{-sqrt(3. / 5.), 0., sqrt(3. / 5.)};
  std::vector<double> three_points_w{5. / 9., 8. / 9., 5. / 9.};
  GaussLegendre gauss_legendre_3p{fun, 0, 1, three_points_w,
                                  three_points_alpha};

  std::cout << gauss_legendre_3p.CalculatePreciseIntegral(kPrecision) << '\n';

  std::cout << "4 Points Gauss Legendre: ";

  std::vector<double> four_points_alpha{-0.33998, 0.33998, -0.86114, 0.86114};
  std::vector<double> four_points_w{0.652147, 0.652147, 0.347852, 0.347852};
  GaussLegendre gauss_legendre_4p{fun, 0, 1, four_points_w, four_points_alpha};

  std::cout << gauss_legendre_4p.CalculatePreciseIntegral(kPrecision) << '\n';
}
