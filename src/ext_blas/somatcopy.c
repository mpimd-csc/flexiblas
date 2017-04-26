#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>
#include "fortran_mangle.h" 
#include "flexiblas.h"

#define FNAME FC_GLOBAL(fsomatcopy,FSOMATCOPY) 
#define ENAME "SOMATCOPY"
#define FLOAT float 

#include "omatcopy_kernel.c"


