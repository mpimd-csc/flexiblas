#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <complex.h>

#include "../hooks.h"

void xerbla_(char *str, Int *info, Int len); 


#define FNAME fsimatcopy_
#define ENAME "SIMATCOPY"
#define FLOAT float 

#include "imatcopy_kernel.c"

