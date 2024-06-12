//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2024 Martin Koehler

   This program is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
   more details.

   You should have received a copy of the GNU General Public License along
   with this program. If not, see <https://www.gnu.org/licenses/>.
   */


#include <stdio.h>
#include <stdlib.h>
#include "flexiblas_fortran_mangle.h"
#include "flexiblas_api.h"
#include "blas_gnu.h"

int main(int argc, char **argv)
{
    int major, minor, patch;
    int i ;
    char fb_name[128];
    int ids[128];
    printf("FlexiBLAS available: %d\n", flexiblas_avail());

    flexiblas_get_version(&major, &minor, &patch);
    printf("Version: %d.%d.%d\n", major, minor, patch);
    flexiblas_print_loaded_backends(stdout);
    flexiblas_print_avail_backends(stdout);
    flexiblas_print_current_backend(stdout);

    printf("Try the other backends.\n");
    int nbackends = flexiblas_list(NULL, 0 , 0);
    for (i = 0; i < nbackends; i++) {
        flexiblas_list(fb_name, 128, i);
        printf("Load %s.\n", fb_name);
        ids[i] = flexiblas_load_backend(fb_name);
        printf("Switch to %s - %d\n", fb_name, ids[i]);
        flexiblas_switch(ids[i]);
        printf("Current loaded backend:\n");
        flexiblas_print_current_backend(stdout);
        printf("\n");

    }

    nbackends = flexiblas_list_loaded(NULL, 0 , 0);
    for (i = 0; i < nbackends; i++) {
        flexiblas_list_loaded(fb_name, 128, i);
        printf("Loaded %s.\n", fb_name);
    }


#ifdef LINK_BLAS
    {
        double test1[]={1,2,3,4,5,6,7,8,9,10};
        blasint N = 10;
        blasint one = 1;
        double ret = 0;

        printf("Generic Interface\n");
        ret = FC_GLOBAL(dasum,DASUM)(&N, test1, &one);
        printf("dasum_(test)      = %lg\n", ret );
    }
#endif

    return 0;
}

