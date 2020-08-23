# Find OCaml binaries.

include( FindPackageHandleStandardArgs )

# find base ocaml binary
find_program( CMAKE_OCAML_OCAML
    NAMES ocaml
    )
# get the directory, used for hints for the rest of the binaries
if( CMAKE_OCAML_OCAML )
    get_filename_component( OCAML_ROOT_DIR ${CMAKE_OCAML_OCAML} PATH )
endif()

# ocamlfind
find_program( CMAKE_OCAML_OCAMLFIND
    NAMES ocamlfind ocamlfind_opt
    HINTS ${OCAML_ROOT_DIR}
    )

if( CMAKE_OCAML_OCAMLFIND )
    execute_process(
        COMMAND ${CMAKE_OCAML_OCAMLFIND} ocamlc -version
        OUTPUT_VARIABLE CMAKE_OCAML_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )
    execute_process(
        COMMAND ${CMAKE_OCAML_OCAMLFIND} ocamlc -where
        OUTPUT_VARIABLE CMAKE_OCAML_STDLIB_PATH
        OUTPUT_STRIP_TRAILING_WHITESPACE
        )
endif()

find_package_handle_standard_args( OCAML "Could NOT find OCaml."
    CMAKE_OCAML_VERSION
    CMAKE_OCAML_STDLIB_PATH
    CMAKE_OCAML_OCAML
    CMAKE_OCAML_OCAMLFIND
    )
        
mark_as_advanced(
    CMAKE_OCAML_OCAML
    CMAKE_OCAML_OCAMLFIND
    )
