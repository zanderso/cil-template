
include_directories("${PROJECT_SOURCE_DIR}/ciltut-include")
link_directories("${PROJECT_BINARY_DIR}/ciltut-lib/src")
link_directories("${PROJECT_BINARY_DIR}/ciltut-lib/src/concolic")

add_subdirectory(src)

