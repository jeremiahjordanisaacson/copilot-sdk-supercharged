cmake_minimum_required(VERSION 3.16)
project(copilot_sdk_fortran
  VERSION 2.0.2
  LANGUAGES Fortran
)

# -------------------------------------------------------------------
# Options
# -------------------------------------------------------------------
option(COPILOT_SDK_BUILD_TESTS    "Build unit tests"  ON)
option(COPILOT_SDK_BUILD_EXAMPLES "Build examples"    ON)

# -------------------------------------------------------------------
# Compiler flags
# -------------------------------------------------------------------
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -Wall -Wextra")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -fcheck=all -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -stand f08 -warn all")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -check all -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
endif()

# -------------------------------------------------------------------
# SDK library
# -------------------------------------------------------------------
add_library(copilot_sdk_fortran
  src/copilot_version.f90
  src/copilot_types.f90
  src/copilot_jsonrpc.f90
  src/copilot_session.f90
  src/copilot_tools.f90
  src/copilot_client.f90
)

target_include_directories(copilot_sdk_fortran PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
  $<INSTALL_INTERFACE:include>
)

# -------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------
if(COPILOT_SDK_BUILD_TESTS)
  enable_testing()
  add_executable(test_client tests/test_client.f90)
  target_link_libraries(test_client PRIVATE copilot_sdk_fortran)
  add_test(NAME copilot_sdk_tests COMMAND test_client)
endif()

# -------------------------------------------------------------------
# Examples
# -------------------------------------------------------------------
if(COPILOT_SDK_BUILD_EXAMPLES)
  add_executable(basic_example examples/basic_example.f90)
  target_link_libraries(basic_example PRIVATE copilot_sdk_fortran)
endif()

# -------------------------------------------------------------------
# Install
# -------------------------------------------------------------------
install(TARGETS copilot_sdk_fortran
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/
  DESTINATION include
  FILES_MATCHING PATTERN "*.mod"
)
