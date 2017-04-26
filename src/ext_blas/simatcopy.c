#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <ctype.h>
#include <complex.h>
#include <ctype.h>
#include "fortran_mangle.h"
#include "flexiblas.h"

#define FNAME FC_GLOBAL(fsimatcopy,FSIMATCOPY)
#define ENAME "SIMATCOPY"
#define FLOAT float

#include "imatcopy_kernel.c"


