cmake_minimum_required(VERSION 2.6)
project(cil-template)

include(CheckLibraryExists)

if(NOT DEFINED CMAKE_ISNTALL_PREFIX)
set(CMAKE_INSTALL_PREFIX "/usr/local/lib/ciltut/")
endif()

if(NOT DEFINED BUILD_TUT11)
set(BUILD_TUT11 false)
endif()

if(NOT DEFINED BUILD_TUT15)
set(BUILD_TUT15 false)
endif()

list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/CMakeModules/")

check_library_exists(rt clock_gettime "" HAVE_RT_L)
check_library_exists(dl dlopen "" HAVE_DL_L)

# No optimization
#set(CMAKE_C_FLAGS "-Wall -Werror -Og -ggdb")
#set(CMAKE_CXX_FLAGS "-std=c++0x -Wall -Werror -Og -ggdb")

# Full optimization
set(CMAKE_C_FLAGS "-flto -Wall -Werror -O3")
set(CMAKE_CXX_FLAGS "-flto -std=c++0x -Wall -Werror -O3")
set(CMAKE_EXE_LINKER_FLAGS "-flto -fuse-linker-plugin")

# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE) 

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

find_package(OCaml REQUIRED)
include(UseOCaml)

set(CILTUT_SRC src/tututil
               src/ciltutoptions
               src/tut0
               src/tut1
               src/tut2
               src/tut3
               src/tut4
               src/tut5
               src/tut6
               src/tut7
               src/tut8
               src/tut9
               src/tut10
               src/tut11
               src/tut12
               src/tut13
               src/tut14
               src/tut15
               src/main)

set(CILTUT_PACKS unix str ocamlgraph cil)

if(BUILD_TUT11 STREQUAL "true")
  set(PPARGS "${PPARGS} -DBUILD_TUT11")
  list(APPEND CILTUT_PACKS dynlink)
  list(APPEND CILTUT_PACKS why3)
endif()

if(BUILD_TUT15 STREQUAL "true")
  set(PPARGS "${PPARGS} -DBUILD_TUT15")
endif()

message("isntall prefix = " ${CMAKE_INSTALL_PREFIX})
message("PPARGS = " ${PPARGS})

foreach(PACK ${CILTUT_PACKS})
  find_ocaml_package(${PACK})
endforeach()

add_ocaml_executable(ciltutcc.exe
                     OCAMLOPTS -pp "camlp4o pa_macro.cmo ${PPARGS}"
                     OCAMLCOPTS -pp "camlp4o pa_macro.cmo ${PPARGS}"
                     SOURCES ${CILTUT_SRC}
                     PACKAGES ${CILTUT_PACKS})

add_subdirectory(ciltut-lib)

install_ocaml_executable(ciltutcc.exe)
install(PROGRAMS ${PROJECT_SOURCE_DIR}/bin/ciltutcc DESTINATION bin)
install(FILES ${PROJECT_SOURCE_DIR}/lib/Ciltut.pm DESTINATION lib)
install(FILES ${PROJECT_SOURCE_DIR}/ciltut-include/ciltut.h DESTINATION include)

# make a symlink in /usr/local/bin
if(CMAKE_HOST_UNIX)
    install(CODE "
    EXECUTE_PROCESS(COMMAND
        ${CMAKE_COMMAND} -E create_symlink
          ${CMAKE_INSTALL_PREFIX}/bin/ciltutcc
          /usr/local/bin/ciltutcc
       WORKING_DIRECTORY /
       )
    ")
endif(CMAKE_HOST_UNIX)
