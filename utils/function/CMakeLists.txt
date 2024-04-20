project(Function)

set(HEADERS
  "include/function.h"
  "include/gauss_legendre.h"
)

set(SOURCES
  "src/function.cpp"
  "src/gauss_legendre.cpp"
)


add_library(function ${SOURCES} ${HEADERS})

