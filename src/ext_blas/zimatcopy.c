#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fzimatcopy_
#define ENAME "ZIMATCOPY"
#define FLOAT double complex 
#define _DOUBLE_PRECISION 

#include "zimatcopy_kernel.c"

