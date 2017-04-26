#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <complex.h>
#include <ctype.h>
#include "fortran_mangle.h"
#include "flexiblas.h"


#define FNAME  FC_GLOBAL(fdimatcopy,FDIMATCOPY)
#define ENAME "DIMATCOPY"
#define FLOAT double
#define _DOUBLE_PRECISION 

#include "imatcopy_kernel.c"


