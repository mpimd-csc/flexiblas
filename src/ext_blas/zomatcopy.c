#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>

#include "fortran_mangle.h"
#include "flexiblas.h"


#define FNAME FC_GLOBAL(fzomatcopy,FZOMATCOPY)
#define ENAME "ZOMATCOPY"
#define FLOAT double complex 

#define _DOUBLE_PRECISION 

#include "zomatcopy_kernel.c"


