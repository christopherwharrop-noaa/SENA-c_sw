####################################################################
# COMMON FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -traceback -i4 -r8" )

####################################################################
# RELEASE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost -qopt-zmm-usage=high " )

####################################################################
# DEBUG FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_DEBUG   "-g -O0 -debug -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -check bounds -check uninit -fp-stack-check -init=snan,array -warn unused" )

####################################################################
# FLAGS FOR AUTOPROFILING
####################################################################

set( Fortran_AUTOPROFILING_FLAGS        "-finstrument-functions -rdynamic" )
