#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>
#include <stdint.h>
#include "fortran_mangle.h"
#include "flexiblas.h"


#define FNAME FC_GLOBAL(fcomatcopy,FCOMATCOPY)
#define ENAME "COMATCOPY"
#define FLOAT float complex

#include "zomatcopy_kernel.c"


