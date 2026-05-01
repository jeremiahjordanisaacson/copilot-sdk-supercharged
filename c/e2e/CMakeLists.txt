##---------------------------------------------------------------------------------------------
##  Copyright (c) Microsoft Corporation. All rights reserved.
##--------------------------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.14)
project(copilot-sdk-c-e2e LANGUAGES C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Propagate the repo root so the test harness can locate snapshot files.
get_filename_component(REPO_ROOT "${CMAKE_CURRENT_SOURCE_DIR}/../.." ABSOLUTE)

# Build the parent SDK library
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_BINARY_DIR}/c_sdk)

# E2E test executable
add_executable(e2e_session_test
    test_harness.c
    e2e_session_test.c
)

target_include_directories(e2e_session_test PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/../include
)

target_link_libraries(e2e_session_test PRIVATE copilot_sdk)

# Pass REPO_ROOT as a compile definition so the harness can find snapshots
target_compile_definitions(e2e_session_test PRIVATE
    COPILOT_REPO_ROOT_DEFINE="${REPO_ROOT}"
)

# Platform-specific link libraries
if(WIN32)
    target_link_libraries(e2e_session_test PRIVATE ws2_32)
endif()

# Register with CTest
enable_testing()

add_test(
    NAME e2e_session_create_disconnect
    COMMAND e2e_session_test
    WORKING_DIRECTORY ${REPO_ROOT}
)

# Set environment for CTest
set_tests_properties(e2e_session_create_disconnect PROPERTIES
    ENVIRONMENT "COPILOT_REPO_ROOT=${REPO_ROOT}"
    TIMEOUT 120
)
