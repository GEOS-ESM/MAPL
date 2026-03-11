#!/bin/bash
# MAPL build wrapper script
# Simplifies building with different compilers by handling module loading

set -e

# Default values
COMPILER="${1:-nag}"
BUILD_TYPE="${2:-Debug}"
BUILD_DIR="build-${COMPILER}"

# Supported compilers
SUPPORTED_COMPILERS="nag gfortran ifort"

# Check if compiler is supported
if [[ ! " $SUPPORTED_COMPILERS " =~ " $COMPILER " ]]; then
    echo "Error: Unsupported compiler '$COMPILER'"
    echo "Supported compilers: $SUPPORTED_COMPILERS"
    exit 1
fi

echo "========================================="
echo "MAPL Build Configuration"
echo "========================================="
echo "Compiler:    $COMPILER"
echo "Build Type:  $BUILD_TYPE"
echo "Build Dir:   $BUILD_DIR"
echo "========================================="

# Load the appropriate compiler stack
echo "Loading ${COMPILER}-stack module..."
module purge
module load ${COMPILER}-stack

echo ""
echo "Loaded modules:"
module list

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure with CMake
echo ""
echo "Configuring with CMake..."
cmake .. \
    -DCMAKE_BUILD_TYPE="$BUILD_TYPE" \
    -DCMAKE_INSTALL_PREFIX=../install

# Build
echo ""
echo "Building..."
cmake --build . -j8

echo ""
echo "========================================="
echo "Build completed successfully!"
echo "========================================="
echo "Build directory: $BUILD_DIR"
echo "To run tests: ./test.sh $COMPILER"
