project(Function)

set(HEADERS
  "include/function.h"
  "include/abstract_gauss_legendre.h"
)

set(SOURCES
  "src/function.cpp"
  "src/abstract_gauss_legendre.cpp"
)


add_library(function ${SOURCES} ${HEADERS})

