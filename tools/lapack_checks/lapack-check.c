/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2016
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>
#include "cscutils/strutils.h"

#include "lapack-3.0.h"
#include "lapack-3.1.0.h"
#include "lapack-3.1.1.h"
#include "lapack-3.2.0.h"
#include "lapack-3.2.1.h"
#include "lapack-3.2.2.h"
#include "lapack-3.3.0.h"
#include "lapack-3.3.1.h"
#include "lapack-3.4.0.h"
#include "lapack-3.4.1.h"
#include "lapack-3.4.2.h"
#include "lapack-3.5.0.h"
#include "lapack-3.6.0.h"
#include "lapack-3.6.1.h"
#include "lapack-3.7.0.h"

#define LAPACK_3_0_0 0x01
#define LAPACK_3_1_0 0x02
#define LAPACK_3_1_1 0x03
#define LAPACK_3_2_0 0x04
#define LAPACK_3_2_1 0x05
#define LAPACK_3_2_2 0x06
#define LAPACK_3_3_0 0x07
#define LAPACK_3_3_1 0x08
#define LAPACK_3_4_0 0x09
#define LAPACK_3_4_1 0x0A
#define LAPACK_3_4_2 0x0B
#define LAPACK_3_5_0 0x0C
#define LAPACK_3_6_0 0x0D
#define LAPACK_3_6_0_DPRC 0x0E
#define LAPACK_3_6_1 0x0F
#define LAPACK_3_6_1_DPRC 0x10  
#define LAPACK_3_7_0 0x11
#define LAPACK_3_7_0_DPRC 0x12

char * lapack_name(int ver) {
    switch (ver) {
        case LAPACK_3_0_0:
            return "3.0";
        case LAPACK_3_1_0:
            return "3.1.0";
        case LAPACK_3_1_1:
            return "3.1.1";
        case LAPACK_3_2_0:
            return "3.2.0";
        case LAPACK_3_2_1:
            return "3.2.1";
        case LAPACK_3_2_2:
            return "3.2.2";
        case LAPACK_3_3_0:
            return "3.3.0";
        case LAPACK_3_3_1:
            return "3.3.1";
        case LAPACK_3_4_0:
            return "3.4.0";
        case LAPACK_3_4_1:
            return "3.4.1";
        case LAPACK_3_4_2:
            return "3.4.2";
        case LAPACK_3_5_0:
            return "3.5.0";
        case LAPACK_3_6_0:
            return "3.6.0";
        case LAPACK_3_6_0_DPRC:
            return "3.6.0-DPRC";
        case LAPACK_3_6_1:
            return "3.6.1";
        case LAPACK_3_6_1_DPRC:
            return "3.6.1-DPRC";
        case LAPACK_3_7_0:
            return "3.7.0";
        case LAPACK_3_7_0_DPRC:
            return "3.7.0-DPRC";

        default:
            return "X.X.X";
    }
    return "X.X.X";
}



char *ignore_list [] = {
    "dlamc1", 
    "dlamc2", 
    "dlamc3", 
    "dlamc4", 
    "dlamc5", 
    "slamc1", 
    "slamc2",
    "slamc3", 
    "slamc4", 
    "slamc5", 
    "dlasd9",    /* Old relict from LAPACK < 3.0 but existing in 3.0  although not needed. */
    "slasd9", 
    NULL
};

int is_in_list(char **haystack, char *needle) 
{
    int i = 0;
    while(haystack[i] != NULL) {
        if (csc_strcasecmp(haystack[i], needle) == 0 ) return -1;
        i++;
    }
    return 0;
}

void check_symbols(char **list, void *lib, int *found, int *missing, int *ignore) 
{
    int i;
    char name[128];
    i = 0;
    *found = 0;
    *missing = 0;
    *ignore = 0;


    while ( list[i] != NULL )
    {
        void *sym1, *sym2;
        snprintf(name, 128, "%s_", list[i]);
        sym1 = dlsym ( lib, list[i] );
        sym2 = dlsym ( lib, name );
        if ( sym1 == NULL && sym2 == NULL) {
            if ( ! is_in_list(ignore_list, list[i]) ) {
                *missing = *missing +1;
                printf("MISS : %s\n", list[i]);
            } else {
                *ignore = *ignore +1;
                // printf("IGNORE: %s\n", list[i]);
            }
        } else {
            *found = *found +1;
            // printf("FOUND: %s at %lx\n", list[i], (long unsigned int) ((sym1==NULL)?sym2:sym1));
        }
        i++;
    }
}

int main(int argc, char **argv)
{
    void *ptr;
    int f = 0;
    int m = 0;
    int ign = 0;
    char **symbolset;
    int lapack_number;
    int lapack_save = -1;


    if ( argc != 2 ) 
    {
        printf("Usage: %s <sofile>\n", argv[0]);
        return -1;
    }

    ptr = dlopen(argv[1], RTLD_LOCAL | RTLD_LAZY);
    if ( ptr == NULL ) {
        printf("Failed to open :%s\n", argv[1]);
        return -1;
    }
    for (lapack_number = 1; lapack_number <= LAPACK_3_7_0_DPRC; lapack_number++) {
        switch(lapack_number) {
            case LAPACK_3_0_0:
                symbolset = lapack_3_0;
                break;
            case LAPACK_3_1_0:
                symbolset = lapack_3_1_0;
                break;
            case LAPACK_3_1_1:
                symbolset = lapack_3_1_1;
                break;
            case LAPACK_3_2_0:
                symbolset = lapack_3_2_0;
                break;
            case LAPACK_3_2_1:
                symbolset = lapack_3_2_1;
                break;
            case LAPACK_3_2_2:
                symbolset = lapack_3_2_2;
                break;
            case LAPACK_3_3_0:
                symbolset = lapack_3_3_0;
                break;
            case LAPACK_3_3_1:
                symbolset = lapack_3_3_1;
                break;
            case LAPACK_3_4_0:
                symbolset = lapack_3_4_0;
                break;
            case LAPACK_3_4_1:
                symbolset = lapack_3_4_1;
                break;
            case LAPACK_3_4_2:
                symbolset = lapack_3_4_2;
                break;
            case LAPACK_3_5_0:
                symbolset = lapack_3_5_0;
                break;
            case LAPACK_3_6_0_DPRC:
                symbolset = lapack_3_6_0_full;
                break;
            case LAPACK_3_6_0:
                symbolset = lapack_3_6_0;
                break;
            case LAPACK_3_6_1_DPRC:
                symbolset = lapack_3_6_1_full;
                break;
            case LAPACK_3_6_1:
                symbolset = lapack_3_6_1;
                break; 
            case LAPACK_3_7_0_DPRC:
                symbolset = lapack_3_7_0_full;
                break;
            case LAPACK_3_7_0:
                symbolset = lapack_3_7_0;
                break; 

            default:
                symbolset = NULL;

        }

        printf("Check for LAPACK %s\n", lapack_name(lapack_number));
        check_symbols(symbolset, ptr, &f, &m, &ign);

        if ( m == 0 ) {
            lapack_save = lapack_number; 
        }

        printf("%s - f = %d \t m = %d\n", argv[1], f, m );

    }

    if ( lapack_save > 0 ) {
        printf("%s is compatible to LAPACK %s\n", argv[1], lapack_name(lapack_save));
    }

    dlclose(ptr);
    return 0;
}

