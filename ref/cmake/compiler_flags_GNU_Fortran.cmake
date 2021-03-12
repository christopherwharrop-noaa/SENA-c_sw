####################################################################
# COMMON FLAGS
####################################################################
set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace -fdefault-real-8 -fdefault-double-8")

####################################################################
# RELEASE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -funroll-all-loops -finline-functions")

####################################################################
# DEBUG FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow" )

####################################################################
# FLAGS FOR AUTOPROFILING
####################################################################

set( Fortran_AUTOPROFILING_FLAGS        "-finstrument-functions -rdynamic" )
