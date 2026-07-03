cmake_minimum_required(VERSION 3.16)
project(pipeline_cpp CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Header-only interface library: the whole framework lives in include/pipeline/.
add_library(pipeline INTERFACE)
target_include_directories(pipeline INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(NOT MSVC)
  add_compile_options(-Wall -Wextra)
endif()

add_executable(telemetry_monitor examples/telemetry_monitor.cpp)
target_link_libraries(telemetry_monitor PRIVATE pipeline)

add_executable(validation examples/validation.cpp)
target_link_libraries(validation PRIVATE pipeline)

enable_testing()
add_test(NAME validation COMMAND validation)
add_test(NAME telemetry_monitor COMMAND telemetry_monitor)
