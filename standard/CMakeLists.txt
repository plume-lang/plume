cmake_minimum_required(VERSION 3.10)

# Project name and version
project(plume-natives VERSION 1.0)

# Find and include libcurl package
find_package(CURL REQUIRED)

# Define the shared library target
add_library(native SHARED)

add_subdirectory(runtime)

# Set the output directory for the target
set_target_properties(native PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/."
    PREFIX ""
    SUFFIX ".plmc"
)

# Add the source files
file(GLOB FFI_FILES "ffi/*.c")
target_sources(native PRIVATE ${FFI_FILES} ${RUNTIME_FILES})

# Include directories
target_include_directories(native PRIVATE runtime/include)

# Link the CURL library
target_link_libraries(native PRIVATE CURL::libcurl)
target_link_libraries(native PRIVATE plume-runtime-lib)
