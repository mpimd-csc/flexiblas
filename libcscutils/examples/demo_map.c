/*
 * LICSCUTILS
 * Copyright (C) Martin Koehler, 2019
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
#include <math.h>
#include <string.h>

#include "cscutils/map.h"


int main(int argc, char **argv)
{
    csc_map_t *map;
    void *iter;
    char *val;
    (void) argc;
    (void) argv;

    map = csc_map_new_string_key(17, free);
    csc_map_insert(map, strdup("key 1") , strdup("value 1"));
    csc_map_insert(map, strdup("key 2") , strdup("value 2"));
    csc_map_insert(map, strdup("key 3") , strdup("value 3"));
    csc_map_insert(map, strdup("key 4") , strdup("value 4"));
    csc_map_insert(map, strdup("key 5") , strdup("value 5"));
    csc_map_insert(map, strdup("key 6") , strdup("value 6"));
    csc_map_insert(map, strdup("key 7") , strdup("value 7"));
    csc_map_insert(map, strdup("key 8") , strdup("value 8"));
    csc_map_insert(map, strdup("Ratzupaltuff 9") , strdup("value 9"));
    csc_map_insert(map, strdup("Test Extrem") , strdup("value 10"));
    csc_map_insert(map, strdup("Test Extrem 4711") , strdup("value 11"));
    csc_map_insert(map, strdup("Numerous Fun With Test Extrem 4711") , strdup("value 12"));
    csc_map_insert(map, strdup("Hey Ho 12345678") , strdup("value 12"));
    csc_map_replace(map, "key 8", strdup("value xx"));


    csc_map_dump(stdout, map,1);
    if ( csc_map_has_key(map, "key 1")) printf("Map has \"key 1\"\n");
    if ( !csc_map_has_key(map, "key X")) printf("Map has no \"key X\"\n");

    printf("Value of \"key 8\" = %s\n", (char *) csc_map_get(map,"key 8"));
    csc_map_remove(map, "key 5");
    csc_map_remove(map, "key 1");
    csc_map_dump(stdout, map, 1);

    csc_map_dump(stdout, map, 0);
    csc_map_sort(map,CSC_MAP_SORT_ASC);
    csc_map_dump(stdout, map, 0);
    csc_map_sort(map,CSC_MAP_SORT_DESC);
    csc_map_dump(stdout, map, 0);

    csc_map_dump(stdout, map, 1);

    iter = NULL;
    while ( (val = csc_map_iterate(map, &iter))) {
        printf("val = %s\n", val);
    }


    csc_map_free(map);
    return 0;
}
