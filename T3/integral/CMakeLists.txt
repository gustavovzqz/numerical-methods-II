
project(Integral)

set(HEADERS
  "include/newton_cotes.h"
)

set(SOURCES
  "src/newton_cotes.cpp"
)


add_library(integral ${SOURCES} ${HEADERS})
