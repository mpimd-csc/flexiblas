#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>

#include "fortran_mangle.h"
#include "flexiblas.h"



#define FNAME FC_GLOBAL(fcimatcopy,FCIMATCOPY)
#define ENAME "CIMATCOPY"
#define FLOAT float complex 

#include "zimatcopy_kernel.c"


