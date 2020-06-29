/*
 * LIBCSCUTILS -- efficient map
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


/* Expert creation routine   */
csc_map_t * csc_map_new_expert(uint64_t size, csc_map_hash_func_t *hf, csc_map_key_free_t *kf, csc_map_value_free_t *vf, csc_map_key_compare_t *cmp)
{
    csc_map_t * new_map;
    uint64_t i;

    new_map = (csc_map_t *) malloc(sizeof(csc_map_t) * (1));
    if ( !new_map ) {
        return NULL;
    }

    new_map -> size = size;
    new_map -> len  = 0;
    new_map -> hash_func = hf;
    new_map -> key_free  = kf;
    new_map -> value_free = vf;
    new_map -> cmp      = cmp;
    new_map -> first = NULL;
    new_map -> last  = NULL;
    new_map -> hash_table  = (csc_map_entry_t **) malloc(sizeof(csc_map_entry_t*) * (size));
    if ( ! new_map -> hash_table ) {
        free(new_map);
        return NULL;
    }
    for (i = 0; i < new_map->size; i++) {
        new_map -> hash_table[i] = NULL;
    }

    pthread_mutex_init(&(new_map->lock), NULL);

    return new_map;
}


/* Internal compare functions for strings   */
static int  str_cmp_func(void * keya, void *keyb)
{
    return strcmp((char*) keya, (char*) keyb);
}

/* Internal compare functions for ints   */
static int  int_cmp_func(void * keya, void *keyb)
{
    int _keya = (int)((size_t) keya);
    int _keyb = (int)((size_t) keyb);

    return _keya-_keyb;
}


/* New map with string keys */
csc_map_t * csc_map_new_string_key(uint64_t size, csc_map_value_free_t *vf)
{
    return csc_map_new_expert(size, csc_map_hash_string, free, vf, str_cmp_func);
}

csc_map_t * csc_map_new_int_key(uint64_t size, csc_map_value_free_t * vf) {
     return csc_map_new_expert(size, csc_map_hash_int, NULL, vf, int_cmp_func);

}

/* Standard hash for strings  */
uint64_t csc_map_hash_string(const void *_key, uint64_t size) {
    const char *key = _key;
    size_t h = 0;
    size_t i = 0;
    size_t len = strlen(key);
    for (i = 0; i < len; i++) {
        h = ( h + key[i]) % size;
    }
    return h;
}

uint64_t csc_map_hash_int(const void *key, uint64_t size)
{
    int _key = (int)((size_t) key);
    return _key % size;
}


/* Free everything  */
void csc_map_free(csc_map_t *map)
{
    csc_map_entry_t *tmp, *tmpx;
    if ( !map ) return;

    tmp = map->first;
    while (tmp != NULL) {
        if ( map->key_free && tmp->key != NULL) map->key_free(tmp->key);
        if ( map->value_free && tmp->value != NULL) map->value_free(tmp->value);
        tmpx = tmp->pnext;
        free(tmp);
        tmp = tmpx;
    }
    free(map->hash_table);
    free(map);

}

/* Free without values */
void csc_map_free2(csc_map_t *map)
{
    csc_map_entry_t *tmp, *tmpx;
    if ( !map ) return;

    tmp = map->first;
    while (tmp != NULL) {
        if ( map->key_free && tmp->key != NULL) map->key_free(tmp->key);
        tmpx = tmp->pnext;
        free(tmp);
        tmp = tmpx;
    }
    free(map->hash_table);
    free(map);

}

static csc_map_entry_t *csc_map_has_key_intern(csc_map_t *map, void *key)
{
    csc_map_entry_t *tmp;
    csc_map_entry_t * ret;
    uint64_t h;

    if ( !map ) return NULL;

    h = map->hash_func(key, map->size);

    tmp = map->hash_table[h];

    if ( tmp == NULL) {
        ret = NULL;
    } else {
        while ( tmp!= NULL && map->cmp(tmp->key,key) != 0 ) {
            tmp = tmp -> phash;
        }
        ret = tmp;
    }
    return ret;
}

int csc_map_insert(csc_map_t *map, void *key, void *val)
{
    csc_map_entry_t * entry;
    if ( !map) {
        return 1;
    }

    /* Search if key is in ?  */
    if ( csc_map_has_key(map, key)) {
        return 1;
    }

    entry = (csc_map_entry_t *) malloc(sizeof(csc_map_entry_t) * (1));
    if ( !entry) {
        return 2;
    }
    entry -> key = key;
    entry -> value = val;
    entry -> hash  = map->hash_func(key, map->size);
    entry -> pnext = NULL;
    entry -> phash = NULL;
    entry -> pprev = map->last;

    pthread_mutex_lock(&map->lock);
    /* Add to list */
    if ( map->last) {
        map->last->pnext = entry;
    }
    map->last = entry;

    if ( map->first == NULL) {
        map->first = entry;
    }
    map->len ++ ;

    /* Add to hash table  */
    if ( map->hash_table[ entry->hash ] == NULL) {
        map->hash_table[ entry->hash ] = entry;
    } else {
        csc_map_entry_t * tmp = map->hash_table[entry->hash];
        while ( tmp->phash != NULL) {
            tmp = tmp->phash;
        }
        tmp->phash = entry;
    }

    pthread_mutex_unlock(&map->lock);
    return 0;
}

int csc_map_insert_key(csc_map_t *map, void *key)
{
    return csc_map_insert(map, key, NULL);
}

int csc_map_replace(csc_map_t *map, void *key, void *value)
{
    csc_map_entry_t *tmp;
    if ( !map) return 1;

    pthread_mutex_lock(&map->lock);
    tmp = csc_map_has_key_intern(map, key);

    if ( tmp == NULL ) {
        pthread_mutex_unlock(&map->lock);
        return 1;
    }
    if ( tmp->value && map->value_free ) map->value_free(tmp->value);
    tmp->value = value;
    pthread_mutex_unlock(&map->lock);
    return 0;
}

/* Return true when a key exists   */
int csc_map_has_key(csc_map_t *map, void *key)
{
    pthread_mutex_lock(&map->lock);
    if ( csc_map_has_key_intern(map, key) == NULL) {
        pthread_mutex_unlock(&map->lock);
        return 0;
    } else {
        pthread_mutex_unlock(&map->lock);
        return 1;
    }
}

void *csc_map_get(csc_map_t *map, void *key)
{
    csc_map_entry_t *tmp;
    pthread_mutex_lock(&map->lock);
    tmp = csc_map_has_key_intern(map, key);
    pthread_mutex_unlock(&map->lock);

    return (void *) tmp->value;
}

int csc_map_remove(csc_map_t *map, void *key)
{
    uint64_t h;
    int f = 0;
    csc_map_entry_t *tmp2;

    h = map->hash_func(key, map->size);

    pthread_mutex_lock(&map->lock);

    csc_map_entry_t **pp = &(map->hash_table[h]);
    csc_map_entry_t *tmp = map->hash_table[h];

    while (tmp) {
        tmp2 = tmp->phash;
        if ( map->cmp(tmp->key, key) == 0 ) {
            *pp = tmp2;
            if ( map->key_free && tmp->key != NULL) map->key_free(tmp->key);
            if ( map->value_free && tmp->value != NULL) map->value_free(tmp->value);
            if ( tmp->pprev ) tmp->pprev->pnext = tmp->pnext;
            if ( tmp->pnext ) tmp->pnext->pprev = tmp->pprev;
            if ( map->first == tmp) {
                map->first = tmp->pnext;
            }
            if ( map->last == tmp) {
                map->last = tmp->pprev;
            }
            map->len --;
            free(tmp);
        }
        pp = &(tmp2);
        tmp = tmp2;
    }

    pthread_mutex_unlock(&map->lock);
    return f;
}

void *csc_map_iterate(csc_map_t *map, void **ptr)
{
    if ( ! map ) return NULL;
    if ( *ptr == NULL) {
        /* Start iteration  */
        if ( map->first) {
            *ptr = (void *) map->first;
            return (void *) map->first->value;
        }
        return NULL;
    }
    csc_map_entry_t *tmp = *ptr;
    tmp = tmp->pnext;
    *ptr = tmp;
    if ( tmp ) {
        return tmp->value;
    } else {
        return NULL;
    }
    return NULL;
}

void *csc_map_iterate_key(csc_map_t *map, void **ptr)
{
    if ( ! map ) return NULL;
    if ( *ptr == NULL) {
        /* Start iteration  */
        if ( map->first) {
            *ptr = (void *) map->first;
            return (void *) map->first->key;
        }
        return NULL;
    }
    csc_map_entry_t *tmp = *ptr;
    tmp = tmp->pnext;
    *ptr = tmp;
    if ( tmp ) {
        return tmp->key;
    } else {
        return NULL;
    }
    return NULL;
}


static csc_map_entry_t *mergsesort(csc_map_t *map, csc_map_entry_t *list, uint64_t len, csc_map_sort_direction_t dir)
{
    csc_map_entry_t *list1, *list2;
    csc_map_entry_t *tmp;
    csc_map_entry_t *sorted_list;
    csc_map_entry_t **s;
    uint64_t i;

    if ( list == NULL ) return NULL;
    if ( list->pnext == NULL) return list;

    i = 0;
    tmp = list;
    while ( tmp && i < len/2) {
        tmp = tmp->pnext;
        i++;
    }
    if ( tmp->pprev ) tmp->pprev->pnext = NULL;
    list2 = tmp;

    list1 = mergsesort(map, list, len/2, dir);
    list2 = mergsesort(map, list2, len-len/2, dir);

    /* Merge  */
    s = &(sorted_list);
    while( list1 != NULL && list2 != NULL) {
        if ( (map->cmp(list1->key, list2->key) < 0 && CSC_MAP_SORT_ASC == dir) ||
             (map->cmp(list1->key, list2->key) > 0 && CSC_MAP_SORT_DESC == dir)) {
            *s = list1;
            list1=list1->pnext;
        } else {
            *s = list2;
            list2=list2->pnext;
        }
        s = &( (*s)->pnext);
    }
    if ( list1 ) *s = list1;
    if ( list2 ) *s = list2;
    return sorted_list;
}


void csc_map_sort(csc_map_t *map, csc_map_sort_direction_t dir)
{
    csc_map_entry_t * tmp, *pprev;
    if ( !map) return;
    if ( !map->first) return;
    map->first = mergsesort(map, map->first, map->len, dir);

    if ( map->first->pnext == NULL) {
        map->last = map->first;
        return;
    }

    pprev = map->first;
    tmp = pprev->pnext;
    while ( tmp != NULL) {
        tmp->pprev = pprev;
        tmp = tmp->pnext;
        pprev = pprev->pnext;
    }
    map->last = pprev;
}

void csc_map_foreach(csc_map_t * map, csc_map_foreach_func_t * func, void *data)
{
    csc_map_entry_t *tmp;
    if ( !map ) {
        return;
    }
    tmp = map->first;
    while (tmp) {
        func ( tmp->key, tmp->value, data);
    }
    return;
}


#if 0
void        csc_map_set_hash( csc_map_t * map, csc_map_hash_func_t *hf)
{

}

void        csc_map_rehash(csc_map_t *map)
{

}

void        csc_map_set_key_free(csc_map_t *map, csc_map_key_free_t *kf)
{

}

void        csc_map_set_key_compare(csc_map_t * map, csc_map_key_compare_t *cf)
{

}

void        csc_map_set_value_free(csc_map_t * map, csc_map_value_free_t *vf)
{

}
#endif

void        csc_map_dump(FILE * out, csc_map_t *map, int table)
{
    uint64_t i;
    csc_map_entry_t * tmp;
    if ( !map ) {
        return;
    }
    if ( table )  {
        for (i = 0; i < map->size; i++) {
            fprintf(out, "Bucket [%5lu]: ", (unsigned long) i);
            tmp = map->hash_table[i];
            while (tmp != NULL) {
                fprintf(out, "[%s]", (char *) tmp->key);
                if ( tmp -> phash ) {
                    fprintf(out, " -> ");
                }
                tmp = tmp -> phash;
            }
            fprintf(out, "\n");

        }
    } else {
        tmp = map -> first;
        while(tmp) {
            fprintf(out, "[ %s ] ", (char *) tmp->key);
            if ( tmp->pnext )  fprintf(out, " -> ");
            tmp = tmp->pnext;
        }
        fprintf(out, "\n");
    }
}


size_t      csc_map_len(csc_map_t *map)
{
    if (!map) return 0;
    return map->len;
}

void csc_map_at(csc_map_t *map, size_t idx, void **key, void **value)
{
    csc_map_entry_t *tmp;
    size_t i;
    if ( !map ) {
        if ( key ) *key = NULL;
        if ( value ) *value = NULL;
        return ;
    }

    tmp = map->first;
    i = 0;
    while ( i != idx && tmp !=NULL) {
        tmp = tmp->pnext;
        i++;
    }
    if ( tmp ) {
        if ( key ) * key = tmp->key;
        if ( value) * value = tmp->value;
    }
    return;
}

