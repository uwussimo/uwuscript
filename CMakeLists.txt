cmake_minimum_required(VERSION 3.12)

project(uwuscript)

# Setup the make environment. #
################################
# Activate very verbose makefiles.
set(CMAKE_VERBOSE_MAKEFILE ON)
# Allow else() and endif() constructs (with empty parentheses)
set(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS true)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON) #To use with clang-tidy, not supported w/ MSVC generator.

# Setup the cmake directory containing numerous cmake scripts and macros.
set(CMAKE_MODULE_PATH "${uwuscript_SOURCE_DIR}/cmake")

add_subdirectory(src)