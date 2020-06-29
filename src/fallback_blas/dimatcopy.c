#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <complex.h>
#include <ctype.h>

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

#define FNAME  FC_GLOBAL(dimatcopy,DIMATCOPY)
#define ENAME "DIMATCOPY"
#define FLOAT double
#define _DOUBLE_PRECISION

#include "imatcopy_kernel.c"


