cmake_minimum_required(VERSION 3.16)
project(copilot_sdk_e2e LANGUAGES Fortran)

# -------------------------------------------------------------------
# Compiler flags (inherit from parent or set standalone)
# -------------------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -Wall -Wextra")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fcheck=all -fbacktrace")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08 -warn all")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -check all -traceback")
endif()

# -------------------------------------------------------------------
# Import the SDK library from the parent build
# -------------------------------------------------------------------
# When built as part of the top-level CMake (recommended):
#   cmake -B build -S fortran && cmake --build build
# the copilot_sdk_fortran target is already available.
# For standalone builds, pull in the parent project.
if(NOT TARGET copilot_sdk_fortran)
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}/sdk)
endif()

# -------------------------------------------------------------------
# E2E test executable
# -------------------------------------------------------------------
# test_harness.f90 must be compiled before test_e2e.f90 (module dependency)
add_executable(test_e2e
  test_harness.f90
  test_e2e.f90
)

target_link_libraries(test_e2e PRIVATE copilot_sdk_fortran)

# Make .mod files from the SDK visible
target_include_directories(test_e2e PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}/../sdk   # when built standalone
  ${CMAKE_BINARY_DIR}                  # when built from parent
)

# -------------------------------------------------------------------
# Register as a CTest test
# -------------------------------------------------------------------
enable_testing()
add_test(NAME fortran_e2e COMMAND test_e2e)

# Allow the test runner to find npx/node
set_tests_properties(fortran_e2e PROPERTIES
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  TIMEOUT 120
)
