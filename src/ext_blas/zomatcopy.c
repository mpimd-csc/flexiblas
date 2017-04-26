#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fzomatcopy_
#define ENAME "ZOMATCOPY"
#define FLOAT double complex

#include "zomatcopy_kernel.c"

