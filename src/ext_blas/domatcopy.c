#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>
#include "fortran_mangle.h"
#include "flexiblas.h"

#define FNAME FC_GLOBAL(fdomatcopy,FDOMATCOPY) 
#define ENAME "DOMATCOPY"
#define FLOAT double 
#define _DOUBLE_PRECISION 
#include "omatcopy_kernel.c"


