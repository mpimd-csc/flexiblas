#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fcomatcopy_
#define ENAME "COMATCOPY"
#define FLOAT float complex

#include "zomatcopy_kernel.c"

