#!/bin/bash

set -e

# Check usage
if [ $# -lt 2 -o $# -gt 6 -o $# -eq 3 -o $# -eq 5 ]; then
  echo "Usage: build.sh compiler build_type [options]"
  echo
  echo "  compiler:   must be intel or gcc"
  echo "  build_type: must be debug or release"
  echo
  echo "  options"
  echo
  echo "    gptl mode: mode must be off (the default), manual, or auto"
  echo "     mpi mode: mode must be off (the default) or on"
  echo
  echo "  examples"
  echo
  echo "    build.sh intel debug"
  echo "    build.sh intel debug gptl manual"
  echo "    build.sh intel debug mpi on"
  echo "    build.sh intel debug gptl auto mpi on"
  echo "    build.sh gnu release mpi on gptl auto"
  echo
  exit 1
fi

# Get arguments
COMPILER=$1
BUILD_TYPE=$2
GPTL="off"
MPI="off"
shift 2
while (( "$#" )); do
  case $1 in
    "gptl"|"GPTL" )
      GPTL=$2
    ;;
    "mpi"|"MPI" )
      MPI=$2
    ;;
    *)
      echo "Invalid option"
      exit 1
    ;;
  esac
  shift 2
done

echo "COMPILER = $COMPILER"
echo "BUILD_TYPE = $BUILD_TYPE"
echo "GPTL = $GPTL"
echo "MPI = $MPI"

# Start with a clean module environment
module purge

# Handle compiler choice
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

# Handle gptl profiling flag
GPTL_CMAKE_OPTIONS=""
case ${GPTL} in
  "off" )
    echo "Building without GPTL enabled"
  ;;
  "manual" )
    echo "Building with GPTL enabled"
    GPTL_CMAKE_OPTIONS="-DENABLE_GPTL=1"
    module load gptl
  ;;
  "auto" )
    echo "Building with GPTL auto-profiling enabled"
    GPTL_CMAKE_OPTIONS="-DENABLE_GPTL=1 -DENABLE_AUTOPROFILING=1"
    module load gptl
  ;;
  *)
    echo "Unsupportd GPTL profile mode: ${GPTL}"
    exit 1
  ;;
esac

# Handle MPI flag
MPI_CMAKE_OPTIONS=""
case ${MPI} in
  "off" )
    echo "Building without MPI enabled"
  ;;
  "on" )
    echo "Building with MPI enabled"
    MPI_CMAKE_OPTIONS="-DENABLE_MPI=1"
    case ${COMPILER} in
      "intel" )
        module load impi
      ;;
      "gcc"|"gnu" )
        module load openmpi
      ;;
      *)
        echo "Unsupported compiler: ${COMPILER}"
        exit 1
      ;;
    esac
  ;;
  *)
    echo "Unsupported MPI mode: ${MPI}"
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
cmake -DCMAKE_BUILD_TYPE=${BUILD_TYPE} ${GPTL_CMAKE_OPTIONS} ${MPI_CMAKE_OPTIONS} ..
make -j4 VERBOSE=1

# Run the test suite
export OMP_NUM_THREADS=4
export OMP_PLACES=cores
export OMP_PROC_BIND=close

ctest
