project(Function)

set(HEADERS
  "include/function.h"
  "include/gauss_legendre.h"
  "include/newton_cotes.h"
  "include/abstract_gauss_integration.h"
  "include/singularity_integration.h"

)

set(SOURCES
  "src/function.cpp"
  "src/gauss_legendre.cpp"
  "src/newton_cotes.cpp"
  "src/abstract_gauss_integration.cpp"
  "src/singularity_integration.cpp"
)


add_library(function ${SOURCES} ${HEADERS})

