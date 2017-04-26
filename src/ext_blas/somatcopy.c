#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fsomatcopy_
#define ENAME "SOMATCOPY"
#define FLOAT float 

#include "omatcopy_kernel.c"

