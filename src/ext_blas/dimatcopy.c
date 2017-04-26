#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fdimatcopy_
#define ENAME "DIMATCOPY"
#define FLOAT double
#define _DOUBLE_PRECISION 

#include "imatcopy_kernel.c"

