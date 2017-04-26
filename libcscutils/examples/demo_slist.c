/* 
 * Demo Program - SLIST from libcscutils 
 * Copyright (C) Martin Koehler, 2015
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, see <http://www.gnu.org/licenses/>.
 * 
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "cscutils/ds.h"

int compare(const void *a, const void *b)
{
    size_t _a = (size_t) a;
    size_t _b = (size_t) b;
    if ( _a == _b ) return 0;
    if ( _a < _b) return -1;
    else return 1;
}

int main(int argc, char **argv)
{
    csc_ds_t *list;

    list = csc_ds_slist(compare, NULL);

    csc_ds_insert(list, (void*) 1);
    csc_ds_insert(list, (void*) 3);
    csc_ds_insert_at(list, (void *) 2, 1);
    csc_ds_insert_at(list, (void *) 0, 0);
    csc_ds_insert_at(list, (void *) 99, -1);
    csc_ds_insert_at(list,(void *) 5, 5);
    csc_ds_insert(list,(void *) 999);


    if ( csc_ds_find(list, (void*)4)) {
        printf("found 4\n");
    } else {
        printf("did not find 4\n");
    }
    if ( csc_ds_find(list, (void *)2)){
        printf("found 2\n");
    } else {
        printf("did not find 2\n");
    }

    csc_ds_dump(stdout, list);


    csc_ds_free(list);

    return 0;
}
