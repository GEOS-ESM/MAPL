#!/bin/bash
# MAPL build wrapper script
# Simplifies building with different compilers by handling module loading

set -e

# Default values
COMPILER="${1:-nag}"
BUILD_TYPE="${2:-Debug}"
INSTALL_PREFIX="${3:-}"

# Use full paths for safety
SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${SOURCE_DIR}/build-${COMPILER}"
if [[ -z "$INSTALL_PREFIX" ]]; then
    INSTALL_DIR="${SOURCE_DIR}/install-${COMPILER}"
else
    INSTALL_DIR="$INSTALL_PREFIX"
fi

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
echo "Install Dir: $INSTALL_DIR"
echo "========================================="

# Load the appropriate compiler stack
echo "Loading ${COMPILER}-stack module..."
module purge
module load ${COMPILER}-stack

echo ""
echo "Loaded modules:"
module list

# Configure with CMake (modern syntax)
echo ""
echo "Configuring with CMake..."
cmake -B "${BUILD_DIR}" \
      -S "${SOURCE_DIR}" \
      --install-prefix="${INSTALL_DIR}" \
      -DCMAKE_BUILD_TYPE="${BUILD_TYPE}"

# Build
echo ""
echo "Building..."
cmake --build "${BUILD_DIR}" --parallel 8

echo ""
echo "========================================="
echo "Build completed successfully!"
echo "========================================="
echo "Build directory: $BUILD_DIR"
echo "Install directory: $INSTALL_DIR"
echo "To run tests: ./test.sh $COMPILER"
echo "To install: cmake --build ${BUILD_DIR} --target install"
