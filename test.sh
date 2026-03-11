#!/bin/bash
# MAPL test wrapper script
# Simplifies running tests with different compilers

set -e

# Default values
COMPILER="${1:-nag}"
TEST_PATTERN="${2:-}"
BUILD_DIR="build-${COMPILER}"

# Supported compilers
SUPPORTED_COMPILERS="nag gfortran ifort"

# Check if compiler is supported
if [[ ! " $SUPPORTED_COMPILERS " =~ " $COMPILER " ]]; then
    echo "Error: Unsupported compiler '$COMPILER'"
    echo "Supported compilers: $SUPPORTED_COMPILERS"
    exit 1
fi

# Check if build directory exists
if [[ ! -d "$BUILD_DIR" ]]; then
    echo "Error: Build directory '$BUILD_DIR' does not exist"
    echo "Please run './build.sh $COMPILER' first"
    exit 1
fi

echo "========================================="
echo "MAPL Test Configuration"
echo "========================================="
echo "Compiler:      $COMPILER"
echo "Build Dir:     $BUILD_DIR"
if [[ -n "$TEST_PATTERN" ]]; then
    echo "Test Pattern:  $TEST_PATTERN"
fi
echo "========================================="

# Load the appropriate compiler stack
echo "Loading ${COMPILER}-stack module..."
module purge
module load ${COMPILER}-stack

echo ""
echo "Loaded modules:"
module list

# Change to build directory
cd "$BUILD_DIR"

# Run tests using ctest
echo ""
echo "Running tests..."
if [[ -n "$TEST_PATTERN" ]]; then
    ctest --output-on-failure -R "$TEST_PATTERN"
else
    ctest --output-on-failure
fi

echo ""
echo "========================================="
echo "Tests completed!"
echo "========================================="
