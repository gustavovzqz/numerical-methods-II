#include <cmath>
#include <iomanip>
#include <iostream>
#include <vector>

#include "utils/function/include/abstract_gauss_integration.h"

int main() {
  // Remember: The integral is performed by the function below multiplied by the
  // value that characterizes the quadrature.
  std::vector<double> alpha;
  std::vector<double> w;

  // Integration limits: -infinity, +infinity.
  auto chebyshev_fun = [](double x) { return 1; };

  // Integration limits: 0, +infinity.
  auto laguerre_fun = [](double x) { return 1; };

  // Integration limits: -1, +1.
  auto hermite_fun = [](double x) { return 1; };

  // ----- Chebyshev ---------
  std::cout << "2 Points Gauss Chebyshev: ";

  alpha = {-sqrt(1. / 2.), sqrt(1. / 2.)};
  w = {M_PI_2, M_PI_2};

  AbstractGaussIntegration gauss_chebyshev_2p{chebyshev_fun, w, alpha};
  std::cout << std::setprecision(10) << gauss_chebyshev_2p.CalculateIntegral()
            << '\n';

  std::cout << "3 Points Gauss Chebyshev: ";

  alpha = {-sqrt(3.) / 2., 0., sqrt(3.) / 2.};
  w = {M_PI / 3, M_PI / 3, M_PI / 3};
  AbstractGaussIntegration gauss_chebyshev_3p{chebyshev_fun, w, alpha};

  std::cout << gauss_chebyshev_3p.CalculateIntegral() << '\n';

  std::cout << "4 Points Gauss Chebyshev: ";
  alpha = {-0.92388, -0.38268, 0.38268, 0.92388};
  w = {M_PI_4, M_PI_4, M_PI_4, M_PI_4};

  AbstractGaussIntegration gauss_chebyshev_4p{chebyshev_fun, w, alpha};

  std::cout << gauss_chebyshev_4p.CalculateIntegral() << '\n';

  // ----- Laguerre -----------

  std::cout << "2 Points Gauss Laguerre: ";

  alpha = {2. - sqrt(2.), 2. + sqrt(2.)};
  w = {(2. - sqrt(2.)) / 4., (2. + sqrt(2.)) / 4.};

  AbstractGaussIntegration gauss_laguerre_2p{laguerre_fun, w, alpha};
  std::cout << std::setprecision(10) << gauss_laguerre_2p.CalculateIntegral()
            << '\n';

  std::cout << "3 Points Gauss Laguerre: ";

  alpha = {0.4157745568, 2.2942803603, 6.2899450829};
  w = {0.7110930099, 0.2785177336, 0.0103892565};
  AbstractGaussIntegration gauss_laguerre_3p{laguerre_fun, w, alpha};

  std::cout << gauss_laguerre_3p.CalculateIntegral() << '\n';

  std::cout << "4 Points Gauss Laguerre: ";
  alpha = {0.32255, 1.7458, 4.5366, 9.3951};
  w = {0.603154, 0.357419, 0.0388879, 0.000539295};
  // The result is worse because the weights and roots are in approximated form

  AbstractGaussIntegration gauss_laguerre_4p{laguerre_fun, w, alpha};

  std::cout << gauss_laguerre_4p.CalculateIntegral() << '\n';

  // ----- Hermite -----------
  std::cout << "2 Points Gauss Hermite: ";

  alpha = {-sqrt(1. / 2.), sqrt(1. / 2.)};
  w = {sqrt(M_PI) / 2., sqrt(M_PI) / 2.};

  AbstractGaussIntegration gauss_hermite_2p{hermite_fun, w, alpha};
  std::cout << std::setprecision(10) << gauss_hermite_2p.CalculateIntegral()
            << '\n';

  std::cout << "3 Points Gauss Hermite: ";

  alpha = {-sqrt(3. / 2), 0., sqrt(3. / 2.)};
  w = {sqrt(M_PI) / 6., 2. * sqrt(M_PI) / 3., sqrt(M_PI) / 6.};
  AbstractGaussIntegration gauss_hermite_3p{hermite_fun, w, alpha};

  std::cout << gauss_hermite_3p.CalculateIntegral() << '\n';

  std::cout << "4 Points Gauss Hermite: ";
  alpha = {-1.6507, -0.52465, 0.52465, 1.6507};
  w = {sqrt(M_PI) / (4 * (3. - sqrt(6.))), sqrt(M_PI) / (4 * (3. + sqrt(6.))),
       sqrt(M_PI) / (4 * (3. + sqrt(6.))), sqrt(M_PI) / (4 * (3. - sqrt(6.)))};
  // The result can be worse because the weights and roots are in approximated
  // form

  AbstractGaussIntegration gauss_hermite_4p{hermite_fun, w, alpha};

  std::cout << gauss_hermite_4p.CalculateIntegral() << '\n';
}