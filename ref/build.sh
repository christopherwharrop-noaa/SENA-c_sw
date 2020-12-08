#!/bin/bash

set -e

# Check usage
if [ $# -ne 2 ]; then
  echo "Usage: build.sh gcc|intel debug|release"
  exit 1
fi

# Get arguments
COMPILER=$1
BUILD_TYPE=$2

# Start with a clean module environment
module purge

# Handle compiler settings
case ${COMPILER} in
  "gcc"|"gnu" )
    export CC=gcc
    export FC=gfortran
    module load gcc 2>/dev/null || module load gnu
  ;;
  "intel" )
    export CC=icc
    export FC=ifort
    module load intel
  ;;
  *) 
    echo "Unsupported compiler: ${COMPILER}"
    exit 1
  ;;
esac

# Handle build type
case ${BUILD_TYPE} in
  "debug"|"release" )
    echo "Building with ${COMPILER} in ${BUILD_TYPE} mode"
  ;;
  *)
    echo "Unsupported build type: ${BUILD_TYPE}"
    exit 1
  ;;
esac

# Print out modules in use
module load netcdf cmake
module list

# Create build dir
rm -rf build
mkdir build
cd build

# Build
cmake -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ..
make -j4 VERBOSE=1

# Run the test suite
export OMP_PLACES=cores
export OMP_PROC_BIND=close
export OMP_NUM_THREADS=4
ctest
