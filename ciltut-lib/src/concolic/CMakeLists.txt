
find_ocaml_package(ocamlyices)
find_ocaml_package(gmp)

set(CONC_SRCS
    concolic_util.ml
    concolic_exp.ml
    concolic_ctxt.ml
    concolic_paths.ml
    concolic_callbacks.ml)

add_ocaml_library(tut15callbacks
  SOURCES ${CONC_SRCS}
  PACKAGES ocamlyices gmp
)

install (FILES "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/ocaml.tut15callbacks.dir/libtut15callbacks.a" DESTINATION lib)