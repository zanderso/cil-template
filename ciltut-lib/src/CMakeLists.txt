
if(${BUILD_TUT15} STREQUAL "true")
  add_subdirectory(concolic)
  set(CONCOLIC_STUBS tut15.c)
else()
  set(CONCOLIC_STUBS )
endif()

set(SHELTER_SRCS ciltut_libc.c tut4.c tut8.c tut6.c tut10.c
                 ${CONCOLIC_STUBS})

add_library(ciltut-static ${SHELTER_SRCS})
set_target_properties(ciltut-static
  PROPERTIES OUTPUT_NAME ciltut
  CLEAN_DIRECT_OUTPUT 1)


add_library(ciltut-shared SHARED ${SHELTER_SRCS})
set_target_properties(ciltut-shared
  PROPERTIES OUTPUT_NAME ciltut
  CLEAN_DIRECT_OUTPUT 1)

install (TARGETS ciltut-static DESTINATION lib)
install (TARGETS ciltut-shared DESTINATION lib)

