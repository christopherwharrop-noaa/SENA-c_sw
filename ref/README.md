# The C_SW reference kernel

NOTE: If you are reading this with a plain text editor, please note that this document is
formatted with Markdown syntax elements.  See https://www.markdownguide.org/cheat-sheet/
for more information.

This is the reference implementation of the `c_sw` kernel extracted from the FV3 model.
It is an excellent kernel for OpenMP analysis. The number of threads is determined by
the `OMP_NUM_THREADS` environment variable, which defaults to the number of hyperthreads
on the machine if it is not set.  Currently the number of threads is hard-coded to 4 in
the build script when running `ctest`, but users can customize it at runtime for other
runs.

## Dependencies
The following packages are required for building and running this kernel:

* C compiler
* Fortran compiler
* [netcdf-c](https://www.unidata.ucar.edu/downloads/netcdf/)
* [netcdf-fortran](https://www.unidata.ucar.edu/downloads/netcdf/)
* [cmake](https://cmake.org/download/) (version >= 3.10)
* git
* [git-lfs](https://git-lfs.github.com/)

## Optional Dependencies
The following packages are optional for building and running this kernel:

* [GPTL](https://github.com/jmrosinski/GPTL) (General Purpose Timing Library)
* MPI

## Prerequisites
This code requires git-lfs. Before cloning the repository, verify that git-lfs is installed, by issuing the following command. This only needs to be done once per user per machine.

```bash
$ git lfs install
```

If the above gives an error you (or your systems administrator) may need to install git-lfs.

Some systems that use modules to manage software provide git with git-lfs support via a
module (e.g. `module load git`).  If you are using a system that uses modules, use
`module avail` to look for alternative versions of git that may have git-lfs support.

Make sure the files in `data/inputs` are NetCDF data files (not text) before proceeding to
the build step. A simple way to do that is with the file command as shown below:

```
$ file data/inputs/*
data/inputs/c_sw_12x24.nc: NetCDF Data Format data
data/inputs/c_sw_24x24.nc: NetCDF Data Format data
data/inputs/c_sw_48x24.nc: NetCDF Data Format data
data/inputs/c_sw_48x48.nc: NetCDF Data Format data
```

**NOTE**: If you cloned the repository with a version of git without git-lfs installed, or before you ran `git lfs install`, you
must run the following command (with a version of git that does support git-lfs) from the base
of the repository to fetch the input data before proceeding to the build steps. Or you can
reclone the repository with git-lfs installed, instead.

```bash
$ git lfs pull
```

Alternatively, you can reclone the repository with git-lfs installed.

## Building the kernel

This kernel uses an out-of-source cmake build, meaning that the build must be done in 
directory that is not in the source tree.

### Basic build procedure (from the directory containing this file)

The basic build steps are as follows (**NOTE**: Make sure not to omit the two dots at the end
of the `cmake` step.):

```bash
$ rm -rf build ; mkdir build ; cd build
$ export CC=<name of C compiler>
$ export FC=<name of fortran compiler> 
$ cmake -DCMAKE_BUILD_TYPE=<debug | release> ..
$ make VERBOSE=1
```

On many systems, the above will suffice. However, some systems will require you to help cmake
find dependencies, particularly if software depencencies are not installed in standard locations.
See below for more information.

### Building with GPTL profiling support

[GPTL](https://github.com/jmrosinski/GPTL) is a timing library that can be used to generate timing
statistics for codes during execution. See the [GPTL documentation](https://jmrosinski.github.io/GPTL/)
for more information and examples.

If GPTL is installed and loaded onto system paths, this kernel may be built with support for
profiling using GPTL. There are two options: autoprofiling and manual insertion of custom GPTL timers.

Autoprofiling will give you timings for all subroutine calls in the kernel. To enable GPTL
autoprofiling add `-DCMAKE_BUILD_TYPE=debug -DENABLE_GPTL=1 -DENABLE_AUTOPROFILING=1` to the
`cmake` command. For example:

```bash
$ cmake -DCMAKE_BUILD_TYPE=debug -DENABLE_GPTL=1 -DENABLE_AUTOPROFILING=1 ..
```

Manual insertion of custom timers allows you to time the execution of specific blocks of code.
To enable GPTL so you can add your own custom timers, add `-DENABLE_GPTL=1` to the `cmake`
command.  For example:

```bash
$ cmake -DCMAKE_BUILD_TYPE=debug -DENABLE_GPTL=1 ..
```

GPTL timing information is written to `timing.*` files in the directory where the code executes.

### Building with MPI support

This kernel can be built with MPI to simulate a parallel execution.  The `c_sw` kernel was extracted
from a much larger MPI code where halo exchanges took place at a higher level. Therefore, this kernel
is not a true parallel program. However, when built with MPI support this kernel simulates a parallel 
MPI execution by running N copies of the serial version and performing a single, simulated, halo
exchange (not implemented yet). To enable simulated parallel MPI execution, add `-DENABLE_MPI=1` to
the `cmake` command. For example:

```bash
$ cmake -DCMAKE_BUILD_TYPE=debug -DENABLE_MPI=1 ..
```

On execution, the input and output statistics for each rank are written to files named according to
their MPI rank. For example:

```
c_sw_12x24.log.0000
c_sw_12x24.log.0001
c_sw_12x24.log.0002
c_sw_12x24.log.0003
```

Each of the log files should be identical because each rank is running the same code with the same input.
The tests in the test suite verify correctness by comparing each of the outputs to the original serial
baseline output.

### Machines that use modules to manage software

Most HPC systems use modules to manage software.  Make sure you have loaded the versions of
the compiler and software you want to use before running the build steps above.  This will allow build
dependencies to be found properly.  For example:

```bash
$ module load intel netcdf cmake
```

### Machines that do not use modules to manage software

If compilers and/or NetCDF is not installed in a standard location where cmake can find it, you
may need to add their installation paths to the `CMAKE_PREFIX_PATH` before running the steps
above. For example:

```bash
$ export CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH:/path/to/netcdf:/path/to/netcdf-fortran
```

### Building on a Mac

By default, gcc points to the clang compiler on Mac.  To use the GNU compiler on Mac, depending
on how the GNU compiler was installed, you may need to specify the C compiler name as gcc-$version.
For example:

```bash
$ export CC=gcc-10
```

## Testing the kernel

First, set the OpenMP variables, including number of threads you want to use for the tests. For example:

```bash
$ export OMP_PLACES=cores
$ export OMP_PROC_BIND=close
$ export OMP_NUM_THREADS=4
```

Then, to run the test suite (from the build directory):

```bash
$ ctest
```

To run a specific test (for example):

```bash
$ ctest -R regression_12x24
```

To run a specific test with full output to get more information about a failure (for example):

```bash
$ ctest -VV -R regression_12x24
```

NOTE: The MPI tests are only run by `ctest` when the kernel is built with MPI support.

## Build and test script

For convenience, a build script is provided that builds the code and runs the test suite. Two
optional arguments (as shown below) specify whether to turn on support for GPTL and/or MPI.
Both of those optional features are off by default.

**(NOTE: This script is written for machines that use modules and it may need to be modified,
depending on how modules are set up on your system)**

```bash
Usage: build.sh compiler build_type [options]

  compiler:   must be intel or gcc
  build_type: must be debug or release

  options

    gptl mode: mode must be off (the default), manual, or auto
     mpi mode: mode must be off (the default) or on

  examples

    build.sh intel debug
    build.sh intel debug gptl manual
    build.sh intel debug mpi on
    build.sh intel debug gptl auto mpi on
    build.sh gnu release mpi on gptl auto
```

## Installation and running

To (optionally) install the built executable into exe/

```bash
$ make install
```

To run the installed executable (for example):

```bash
$ export OMP_PLACES=cores
$ export OMP_PROC_BIND=close
$ export OMP_NUM_THREADS=4
$ exe/c_sw ../test/test_input/c_sw_12x24.nl
```
To run the installed executable built with MPI, the appropriate MPI launch
command must be used.  The correct MPI launch command is dependent on the MPI
implementation used to build the kernel.  For example:

```bash
$ mpirun -np 4 exe/c_sw ../test/test_input/c_sw_12x24.nl
```

## NOTES:

1. The test suite does not measure performance, but reports how long each test takes to run.
2. The MPI tests in the test suite are only run when the kernel is built with MPI support.
3. Detailed performance timings are printed in the stdout when the kernel runs.
4. To view kernel output, either run ctest in verbose mode, or run the kernel manually.

## Here is a list of the files and what they contain.

- `cmake/` contains compiler flag settings and cmake helper scripts
- `src/` contains the kernel source code
- `test/` contains the tests, test input, and test output
- `test/data/outputs` is where test output data is written
- `test/test_input` contains the test namelist input files
- `test/test_output` contains the test baselines
- `exe/` contains the installed executable

## Troubleshooting

1. All tests fail on my machine.

    Check to make sure git-lfs is installed and that all files in `data/inputs` are NetCDF 
    data files and are not text. Run `git lfs pull` to download NetCDF files if necessary.

2. I get `Skipping object checkout, Git LFS is not installed.` when running `git lfs pull`

    Run `git lfs install` to perform the one-time installation that git-lfs requires per user per machine.

3. I get `git: 'lfs' is not a git command.` when running `git lfs pull`

    Your version of git does not support git-lfs. Install git-lfs or load a version of git that supports it.

4. I get `git-lfs smudge -- 'data/inputs/c_sw_12x24.nc': git-lfs: command not found` when cloning.

    Your version of git does not support git-lfs. Install git-lfs or load a version of git that supports it.

5. I get unresolved symbols when testing / running the kernel

    If you are on a machine that uses modules to manage software, you probably need to load the modules
    for your compiler and/or NetCDF **(make sure to use the same modules to build, test, and run)**.  For example:
    ```bash
    $ module load intel netcdf
    ```

    If you are on a machine that does not use modules, you probably need to add the paths of your compiler
    and/or NetCDF libraries to `LD_LIBRARY_PATH`.  For example:
    ```bash
    $ export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/netcdf-c/lib:/path/to/netcdf-fortran/lib
    ```
