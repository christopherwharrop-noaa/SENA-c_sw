####################################################################
# COMMON FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback -i4 -r8 -heap-arrays -fp-model precise " )

####################################################################
# RELEASE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-O3 -xHost " )

####################################################################
# DEBUG FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_DEBUG   "-O0 -debug -gen-interfaces -warn interfaces -check -fpe0 -nolib-inline -fno-inline-functions -assume protect_parens,minus0 -prec-div -prec-sqrt -init=snan,array -warn unused" )

####################################################################
# FLAGS FOR AUTOPROFILING
####################################################################

set( Fortran_AUTOPROFILING_FLAGS        "-finstrument-functions -rdynamic" )
