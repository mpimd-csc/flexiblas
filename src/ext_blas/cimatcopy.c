#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fcimatcopy_
#define ENAME "CIMATCOPY"
#define FLOAT float complex 

#include "zimatcopy_kernel.c"

