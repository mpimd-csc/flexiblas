#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>

#include "fortran_mangle.h"
#include "flexiblas.h"



#define FNAME FC_GLOBAL(fzimatcopy,FZIMATCOPY)
#define ENAME "ZIMATCOPY"
#define FLOAT double complex 
#define _DOUBLE_PRECISION  
#include "zimatcopy_kernel.c"





