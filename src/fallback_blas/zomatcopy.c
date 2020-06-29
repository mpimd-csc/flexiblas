#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>
#include <string.h>
#include "fortran_mangle.h"
/* #include "flexiblas.h" */


#ifndef Int
#ifndef INTEGER8
#define Int 	int
#else
#define Int 	int64_t
#endif
#endif


#define FNAME FC_GLOBAL(zomatcopy,ZOMATCOPY)
#define ENAME "ZOMATCOPY"
#define FLOAT double complex

#define _DOUBLE_PRECISION

#include "zomatcopy_kernel.c"


