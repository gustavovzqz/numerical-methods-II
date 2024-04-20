project(Function)

set(HEADERS
  "include/function.h"
  "include/gauss_legendre.h"
  "include/newton_cotes.h"
)

set(SOURCES
  "src/function.cpp"
  "src/gauss_legendre.cpp"
  "src/newton_cotes.cpp"
)


add_library(function ${SOURCES} ${HEADERS})

