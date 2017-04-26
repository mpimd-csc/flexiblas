#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fdomatcopy_
#define ENAME "DOMATCOPY"
#define FLOAT double 

#include "omatcopy_kernel.c"

