/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
 * Copyright (C) 2015 Martin Koehler
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
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 500
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <pthread.h>
#include <limits.h>
#include <stdlib.h>
#include <unistd.h>


#include "cscutils/inifile.h"

#define LOCK  do { pthread_mutex_lock(&(ini->lock)); } while(0);
#define UNLOCK  do { pthread_mutex_unlock(&(ini->lock)); } while(0);
#define CHANGED do { ini->changed = 1; } while(0);
#define UNCHANGED do { ini->changed = 0; } while(0);

/*  Helpers  */
static void free_kvstore(csc_ini_kvstore_t *head) {
    csc_ini_kvstore_t *tmp;

    while ( head != NULL ){
        if ( head -> key != NULL)
            free(head->key);
        if ( head -> value != NULL)
            free(head->value);
        tmp = head;
        head = head->next;
        free(tmp);
    }
}

csc_ini_error_t csc_ini_section_add(csc_ini_file_t * ini, const char * section)
{
    LOCK;

    if ( ini->sections == NULL ) {
        ini->sections = calloc (1 , sizeof(csc_ini_section_t));
        if (ini->sections == NULL) {
            UNLOCK;
            return CSC_INI_MALLOC;
        }
        ini->sections->next = NULL;
        ini->section_tail= ini->sections;
        ini->sections->parent = ini;
        /* NULL Section */
        if ( section == NULL ) {
            ini->sections->section_name = NULL;
        } else {
            ini->sections->section_name = calloc(strlen(section)+5, sizeof(char));
            if ( ini->sections->section_name == NULL) {
                free(ini->sections);
                ini->sections = NULL;
                UNLOCK;
                return CSC_INI_MALLOC;
            }
            strcpy(ini->sections->section_name, section);
        }
        CHANGED;
        UNLOCK;
        return CSC_INI_SUCCESS;
    } else {
        csc_ini_section_t * tmp = ini->sections;
        while (tmp != NULL) {
            if ( section != NULL ){
                if  (tmp->section_name != NULL &&  (strcmp( tmp->section_name, section) == 0)){
                    UNLOCK;
                    return CSC_INI_SUCCESS;
                }
            }  else {
                if (tmp->section_name == section) {
                    UNLOCK;
                    return CSC_INI_SUCCESS;
                }
            }
            tmp = tmp->next;
        }
        if (tmp == NULL) {
            tmp = malloc(sizeof(csc_ini_section_t));
            if ( tmp == NULL ) {
                UNLOCK;
                return CSC_INI_MALLOC;
            }
            memset(tmp, 0, sizeof(csc_ini_section_t));
            tmp -> next = NULL;

            if ( section ==  NULL ) {
                tmp->section_name = NULL;
            } else {
                tmp->section_name = strdup(section);
                if ( tmp ->section_name == NULL ) {
                    free(tmp);
                    UNLOCK;
                    return CSC_INI_MALLOC;
                }
            }
            ini->section_tail->next = tmp;
            ini->section_tail = tmp;
            tmp->parent = ini;
            CHANGED;
            UNLOCK;
            return CSC_INI_SUCCESS;
        }
    }
    return CSC_INI_SUCCESS;


}

static void free_section(csc_ini_section_t * section) {
    if ( section == NULL) return ;
    free_kvstore(section->kvstore);
    if ( section->section_name != NULL)
        free(section->section_name);
}

static char *ltrim(char *str) {
    while (isspace(*str))
        str++;
    return str;
}
static void rtrim(char *str) {
    size_t len = strlen(str)-1;
    while ( isspace(str[len]) && len > 0 ){
        len --;
    }
    str[len+1] = '\0';
}
static void *trim(char* str){
    rtrim(str);
    return ltrim(str);
}

static void remove_newline(char * str, ssize_t *len){
    ssize_t l2 = *len -1;
    while ( (str[l2] == '\n' || str[l2] == '\r') && l2 > 0){
        l2--;
    }
    str[l2+1] = '\0';
    * len = l2+1;
}

static char *uppercase(char *str) {
    char *ret = str;
    if ( str == NULL ) return NULL;
    while (*str != '\0') {
        *str = toupper(*str);
        str++;
    }
    return ret;
}

static char *lowercase(char *str) {
    char *ret = str;
    if ( str == NULL ) return NULL;
    while (*str != '\0') {
        *str = tolower(*str);
        str++;
    }
    return ret;
}

csc_ini_error_t  csc_ini_empty(csc_ini_file_t *ini)
{
    if ( ini == NULL)
        return CSC_INI_NULL;
    memset(ini, 0, sizeof(csc_ini_file_t));
    pthread_mutex_init(&(ini->lock), NULL);
    return CSC_INI_SUCCESS;
}

csc_ini_error_t  csc_ini_load(const char *filename, csc_ini_file_t * content, unsigned int flag)
{
    FILE * fp ;
    char *current_section_name = NULL;
    char *read_line = NULL , *line = NULL;
    size_t line_len = 0;
    ssize_t len;
    int line_number = 0;
    csc_ini_error_t ret = CSC_INI_SUCCESS, iret = CSC_INI_SUCCESS;
    char * tmp;
    char * key, *value;
    size_t path_max;


    if (filename == NULL) {
        return CSC_INI_NULL;
    }
    if (content == NULL) {
        return CSC_INI_NULL;
    }

    fp = fopen(filename, "r");
    if ( fp == NULL) {
        return CSC_INI_FILEOPEN;
    }

#ifdef PATH_MAX
    path_max = PATH_MAX;
#else
    path_max = pathconf(path, _PC_PATH_MAX);
    if (path_max <= 0)
        path_max = 4096;
#endif

    if ( content->filename == NULL ) {
        content->filename = malloc(sizeof(char) * path_max);
    }
    if ( content->filename == NULL ) {
        fclose(fp);
        return CSC_INI_MALLOC;
    }
    if ( realpath(filename, content->filename) == NULL ) {
        free(content->filename);
        content->filename = NULL;
        fclose(fp);
        return CSC_INI_FILEOPEN;
    }

    line_len = 1024;
    read_line = calloc(line_len, sizeof(char));

    while ( (len = getline(&read_line, &line_len, fp )) >= 0){
        line_number ++;
        remove_newline(read_line, &len);
        // printf("read line: '%s' (%d) \n", read_line, len);
        line = trim(read_line);

        /* Skip comments */
        if (   line[0] == '#'
                || line[0] == ';'
                || line[0] == '%' )
            continue;

        /* Skip empty lines  */
        if ( strlen(line) < 2 )
            continue;


        /* Found Section  */
        if ( line[0] == '[' ){
            char * tmp_name = &line[1];

            tmp = strstr(tmp_name, "]");
            if ( tmp == NULL){
                fprintf(stderr, "Invalid line (%4d): \"%s\"\n", line_number, read_line);
                ret = CSC_INI_SYNTAX;
                goto finish;
            }
            *tmp = '\0';

            if (current_section_name != NULL)
                free(current_section_name);
            current_section_name = strdup(trim(tmp_name));

            // printf("Found Section \"%s\"\n",current_section_name);
            if ( flag & CSC_INI_LOAD_SECTION_UPPERCASE ) {
                current_section_name = uppercase(current_section_name);
            } else if ( flag & CSC_INI_LOAD_SECTION_LOWERCASE ) {
                current_section_name = lowercase(current_section_name);
            }

            ret = csc_ini_section_add(content,current_section_name);
            if ( ret != CSC_INI_SUCCESS) {
                goto finish;
            }
            continue;
        }

        /*  Search KEY-Value pair  */

        tmp = strstr(line, "=");

        if ( tmp == NULL ) {
            fprintf(stderr, "Invalid line (%4d) = \"%s\"\n",line_number, read_line );
            ret = CSC_INI_SYNTAX;
            goto finish;
        }

        tmp[0] = '\0';

        key = trim(line);
        value = &tmp[1];
        value = trim ( value );

        if ( flag & CSC_INI_LOAD_KEY_UPPERCASE ) {
            key = uppercase(key);
        } else if (flag & CSC_INI_LOAD_KEY_LOWERCASE) {
            key = lowercase(key);
        }

        iret = csc_ini_setstring(content, current_section_name, key, value);

        if ( iret != CSC_INI_SUCCESS) {
            fprintf(stderr, "Error: Failed to store the key/value pair from line %d\n",line_number);
            ret = iret;
            goto finish;
        }
    }

    content->changed = 0;
finish:
    if ( current_section_name != NULL)
        free(current_section_name);
    free(read_line);
    fclose(fp);
    return ret;
}

csc_ini_error_t  csc_ini_save(csc_ini_file_t *ini)
{
    if ( ini->filename == NULL ) {
        return CSC_INI_NULL;
    }
    return csc_ini_write(ini->filename, ini);
}

int csc_ini_has_changed(csc_ini_file_t *ini){
    if ( ini == NULL) return -1;
    return ini->changed;
}

csc_ini_error_t  csc_ini_write(const char * filename, csc_ini_file_t *ini)
{
    csc_ini_section_t * sec;
    csc_ini_iterator_t kv_iter;
    csc_ini_iterator_t sec_iter;
    csc_ini_kvstore_t  *kv;
    FILE *fp;
    size_t path_max;

    fp = fopen ( filename, "w") ;
    if ( fp == NULL) {
        return CSC_INI_FILEOPEN;
    }

    LOCK;
    /* Get the Default section */
    sec = csc_ini_getsection(ini, CSC_INI_DEFAULT_SECTION);
    if ( sec ) {
        kv_iter = NULL;
        while( (kv =  csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL ){
            fprintf(fp, "%s=%s\n",csc_ini_getkey(kv), csc_ini_getvalue(kv));
        }
        fprintf(fp, "\n");
    }

    /* Remaining Sections  */
    sec_iter = NULL;
    while ( (sec = csc_ini_section_iterator(ini, &sec_iter)) != NULL) {
        if ( csc_ini_getsectionname(sec) == NULL)
            continue;
        fprintf(fp, "[%s]\n", csc_ini_getsectionname(sec));
        kv_iter = NULL;
        while( (kv =  csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL ){
            fprintf(fp, "%s=%s\n",csc_ini_getkey(kv), csc_ini_getvalue(kv));
        }
        fprintf(fp, "\n");
    }

#ifdef PATH_MAX
    path_max = PATH_MAX;
#else
    path_max = pathconf(path, _PC_PATH_MAX);
    if (path_max <= 0)
        path_max = 4096;
#endif
    if ( ini->filename == NULL ) {
        ini->filename = malloc(sizeof(char) * path_max);
    }
    if ( ini->filename == NULL ) {
        UNLOCK;
        fclose(fp);
        return CSC_INI_MALLOC;
    }
    if ( realpath(filename, ini->filename) == NULL ) {
        free(ini->filename);
        ini->filename = NULL;
        UNLOCK;
        fclose(fp);
        return CSC_INI_FILEOPEN;
    }

    UNCHANGED;
    UNLOCK;
    fclose(fp);
    return CSC_INI_SUCCESS;
}


/* Clears an Inifile */
csc_ini_error_t  csc_ini_free(csc_ini_file_t *ini)
{
    csc_ini_section_t *tmp;

    while ( ini->sections != NULL ){
        tmp = ini->sections;
        ini->sections = ini->sections->next;
        free_section(tmp);
        free(tmp);
    }
    free(ini->filename);
    pthread_mutex_destroy(&(ini->lock));
    return CSC_INI_SUCCESS;
}



csc_ini_error_t  csc_ini_getstring(csc_ini_file_t * ini, const char *section, const char * key, char ** value)
{
    csc_ini_section_t * sec;
    csc_ini_error_t ret;

    if ( ini  == NULL ){
        return CSC_INI_NULL;
    }
    if ( key == NULL) {
        return CSC_INI_NULL;
    }
    if ( value == NULL ) {
        return CSC_INI_NULL;
    }

    LOCK;
    sec = csc_ini_getsection(ini, section);
    if ( sec  == NULL ) {
        UNLOCK;
        *value = NULL;
        return CSC_INI_NOSECTION;
    }

    ret = csc_ini_section_getstring(sec, key , value);
    UNLOCK;
    return ret;

}


csc_ini_error_t  csc_ini_getinteger(csc_ini_file_t * ini, const char *section, const char *key, int * value)
{
    csc_ini_section_t * sec;
    csc_ini_error_t ret ;

    if ( ini  == NULL ){
        return CSC_INI_NULL;
    }
    if ( key == NULL) {
        return CSC_INI_NULL;
    }
    if ( value == NULL ) {
        return CSC_INI_NULL;
    }

    LOCK;
    sec = csc_ini_getsection(ini, section);
    if ( sec  == NULL ) {
        UNLOCK;
        return CSC_INI_NOSECTION;
    }

    ret = csc_ini_section_getinteger(sec, key , value);
    UNLOCK;
    return ret;
}


csc_ini_error_t  csc_ini_getfloat(csc_ini_file_t *ini, const char *section, const char *key, double *value)
{
    csc_ini_section_t * sec;
    csc_ini_error_t ret ;

    if ( ini  == NULL ){
        return CSC_INI_NULL;
    }
    if ( key == NULL) {
        return CSC_INI_NULL;
    }
    if ( value == NULL ) {
        return CSC_INI_NULL;
    }

    LOCK;
    sec = csc_ini_getsection(ini, section);
    if ( sec  == NULL ) {
        UNLOCK;
        return CSC_INI_NOSECTION;
    }

    ret = csc_ini_section_getfloat(sec, key , value);
    UNLOCK;
    return ret;
}



csc_ini_error_t  csc_ini_setstring (csc_ini_file_t * ini, const char *section, const char *key, const char *value)
{
    csc_ini_error_t ret;

    if ( ini == NULL )
        return CSC_INI_NULL;

    LOCK;
    if ( ini->sections == NULL ) {
        ini->sections = malloc ( sizeof(csc_ini_section_t));
        if (ini->sections == NULL) {
            UNLOCK;
            return CSC_INI_MALLOC;
        }
        memset(ini->sections, 0, sizeof(csc_ini_section_t));
        ini->sections->next = NULL;
        ini->section_tail= ini->sections;
        ini->sections->parent = ini;
        /* NULL Section */
        if ( section == NULL ) {
            ini->sections->section_name = NULL;
        } else {
            ini->sections->section_name = strdup(section);
            if ( ini->sections->section_name == NULL) {
                free(ini->sections);
                ini->sections = NULL;
                UNLOCK;
                return CSC_INI_MALLOC;
            }
        }

        /* Insert Value */
        ret = csc_ini_section_setstring(ini->sections, key, value);
        CHANGED;
        UNLOCK;
        return ret;
    } else {
        csc_ini_section_t * tmp = ini->sections;

        while (tmp != NULL) {
            if ( section != NULL ) {
                if  (tmp->section_name != NULL &&  (strcmp( tmp->section_name, section) == 0)){
                    ret = csc_ini_section_setstring(tmp, key, value);
                    UNLOCK;
                    return ret;
                }
            }  else {
                if (tmp->section_name == section) {
                    ret = csc_ini_section_setstring(tmp, key, value);
                    UNLOCK;
                    return ret;
                }
            }
            tmp = tmp->next;
        }
        if (tmp == NULL) {
            tmp = malloc(sizeof(csc_ini_section_t));
            if ( tmp == NULL ) {
                UNLOCK;
                return CSC_INI_MALLOC;
            }
            memset(tmp, 0, sizeof(csc_ini_section_t));
            tmp -> next = NULL;

            if ( section ==  NULL ) {
                tmp->section_name = NULL;
            } else {
                tmp->section_name = strdup(section);
                if ( tmp ->section_name == NULL ) {
                    free(tmp);
                    UNLOCK;
                    return CSC_INI_MALLOC;
                }
            }
            ini->section_tail->next = tmp;
            ini->section_tail = tmp;
            tmp->parent = ini;
            ret = csc_ini_section_setstring(ini->section_tail, key, value);
            UNLOCK;
            return ret;
        }
    }
    return CSC_INI_SUCCESS;
}


csc_ini_error_t  csc_ini_setinteger(csc_ini_file_t * ini, const char *section, const char *key, const int value)
{
    char ivalue[40];
    snprintf(ivalue, 40, "%d", value);
    return csc_ini_setstring(ini, section, key, ivalue);
}


csc_ini_error_t  csc_ini_setfloat  (csc_ini_file_t * ini, const char *section, const char *key, const double value)
{
    char ivalue[40];
    snprintf(ivalue, 40, "%20.17e", value);
    return csc_ini_setstring(ini, section, key, ivalue);

}


csc_ini_section_t * csc_ini_section_iterator(csc_ini_file_t *ini, csc_ini_iterator_t *iter)
{
    if ( iter == NULL ) {
        fprintf(stderr, "iter == NULL\n");
        return NULL;
    }
    if ( ini == NULL) {
        *iter = NULL;
        return NULL;
    }
    if (*iter == NULL) {
        /* First Call */
        *iter = ini->sections;
        if ( *iter == NULL )
            return NULL;
        else
            return (csc_ini_section_t *) *iter;

    } else {
        /* >= 2nd call */
        *iter = ((csc_ini_section_t *) (*iter))-> next;
        if (*iter == NULL )
            return NULL;
        else
            return (csc_ini_section_t *) *iter;

    }
    return NULL;
}

csc_ini_section_t * csc_ini_getsection      (csc_ini_file_t *ini, const char *section)
{
    csc_ini_section_t * tmp = ini->sections;

    while (tmp != NULL) {
        if ( section != NULL ){
            if  (tmp->section_name != NULL &&  (strcmp( tmp->section_name, section) == 0)){
                break;
            }
        } else {
            if (tmp->section_name == section) {
                break;
            }
        }
        tmp = tmp->next;
    }
    return tmp;
}


csc_ini_error_t  csc_ini_section_getstring(csc_ini_section_t * sec, const char * key, char ** value)
{
    csc_ini_iterator_t  kv_iter;
    csc_ini_kvstore_t  * kv;

    if ( sec  == NULL ){
        return CSC_INI_NULL;
    }
    if ( key == NULL) {
        return CSC_INI_NULL;
    }
    if ( value == NULL ) {
        return CSC_INI_NULL;
    }

    kv_iter = NULL;

    while (( kv = csc_ini_kvstore_iterator(sec, &kv_iter)) != NULL ){
        if ( strcmp(csc_ini_getkey(kv), key) == 0 ) {
            *value = csc_ini_getvalue(kv);
            return CSC_INI_SUCCESS;
        }
    }
    *value = NULL;
    return CSC_INI_NOKEY;

}


csc_ini_error_t  csc_ini_section_getinteger(csc_ini_section_t * sec, const char * key, int * value)
{
    csc_ini_error_t ret;
    char *str, *endptr;
    int val = 0;

    ret =  csc_ini_section_getstring(sec, key, &str);
    if (ret != CSC_INI_SUCCESS) {
        return ret;
    }
    if ( str  == NULL ) {
        return CSC_INI_VALUE;
    }
    if ( *str == '\0') {
        return CSC_INI_VALUE;
    }

    errno = 0;
    val = (int) strtol(str, &endptr, 10);

    if ((errno == ERANGE )
            || (errno != 0 && val == 0)) {
        return CSC_INI_VALUE;
    }
    if (*endptr != '\0') {
        return CSC_INI_VALUE;
    }

    *value = val;
    return CSC_INI_SUCCESS;

}


csc_ini_error_t  csc_ini_section_getfloat(csc_ini_section_t * sec, const char * key, double *value)
{
    csc_ini_error_t ret;
    char *str, *endptr;
    double  val;

    ret =  csc_ini_section_getstring(sec, key, &str);
    if (ret != CSC_INI_SUCCESS) {
        return ret;
    }
    if ( str  == NULL ) {
        return CSC_INI_VALUE;
    }
    if ( *str == '\0') {
        return CSC_INI_VALUE;
    }

    errno = 0;
    val =  strtod(str, &endptr);

    if ((errno == ERANGE )
            || (errno != 0 && fabs(val) < 1e-16)) {
        return CSC_INI_VALUE;
    }
    if (*endptr != '\0') {
        return CSC_INI_VALUE;
    }

    *value = val;
    return CSC_INI_SUCCESS;

}



csc_ini_error_t  csc_ini_section_setstring(csc_ini_section_t * ini, const char * key, const char * value)
{
    csc_ini_kvstore_t *tmp, *prev;

    if ( key == NULL ){
        return CSC_INI_NULL;
    }
    if (strlen(key) == 0 ) {
        fprintf(stderr, "Error: Length 0 Key\n");
        return CSC_INI_NOKEY;
    }

    if ( ini->kvstore == NULL ){
        ini->kvstore = (csc_ini_kvstore_t *) malloc ( sizeof(csc_ini_kvstore_t));
        if ( ini->kvstore == NULL) {
            return CSC_INI_MALLOC;
        }
        memset(ini->kvstore, 0, sizeof(csc_ini_kvstore_t));
        ini->kvstore->key = strdup(key);
        ini->kvstore->value = strdup(value);
        ini->kvstore->next = NULL;
    } else {
        tmp = ini->kvstore;
        prev = NULL;
        while (tmp != NULL && ( strcmp(tmp->key, key) != 0)){
            prev = tmp;
            tmp = tmp->next;
        }
        if ( tmp == NULL) {
            tmp = (csc_ini_kvstore_t * ) malloc(sizeof(csc_ini_kvstore_t));
            if ( tmp == NULL ){
                return CSC_INI_MALLOC;
            }
            memset (tmp, 0 , sizeof(csc_ini_kvstore_t));
            tmp -> key = strdup(key);
            tmp -> value = strdup(value);
            tmp -> next  = NULL;
            prev-> next = tmp;
        } else {
            if ( tmp->value != NULL){
                free(tmp->value);
                tmp->value = NULL;
            }
            tmp->value = strdup(value);
        }
    }
    ini->parent->changed = 1;
    return CSC_INI_SUCCESS;
}


csc_ini_error_t  csc_ini_section_setinteger(csc_ini_section_t * ini, const char * key, const int value)
{
    char ivalue[40];
    snprintf(ivalue, 40, "%d", value);
    return csc_ini_section_setstring(ini, key, ivalue);
}


csc_ini_error_t  csc_ini_section_setfloat(csc_ini_section_t * ini, const char * key, const double value)
{
    char ivalue[40];
    snprintf(ivalue, 40, "%20.17e", value);
    return csc_ini_section_setstring(ini, key, ivalue);
}



csc_ini_kvstore_t * csc_ini_kvstore_iterator(csc_ini_section_t *section,  csc_ini_iterator_t *iter)
{
    if ( iter == NULL ) {
        fprintf(stderr, "iter == NULL\n");
        return NULL;
    }
    if ( section == NULL ) {
        *iter = NULL;
        return NULL;
    }
    if (*iter == NULL) {
        /* First Call */
        *iter = section->kvstore;;
        if ( *iter == NULL )
            return NULL;
        else
            return (csc_ini_kvstore_t *) *iter;

    } else {
        /* >= 2nd call */
        *iter = ((csc_ini_kvstore_t *) (*iter))-> next;
        if (*iter == NULL )
            return NULL;
        else
            return (csc_ini_kvstore_t *) *iter;

    }
    return NULL;

}

char * csc_ini_getkey(const csc_ini_kvstore_t * kvstore)
{
    if ( kvstore == NULL )
        return NULL;
    return kvstore->key;
}

char * csc_ini_getvalue(const csc_ini_kvstore_t * kvstore)
{
    if ( kvstore == NULL)
        return NULL;
    return kvstore->value;
}

char * csc_ini_getsectionname(const csc_ini_section_t *sec)
{
    if ( sec == NULL)
        return NULL;
    return sec->section_name;
}

csc_ini_error_t  csc_ini_key_remove(csc_ini_file_t *ini, const char * section, const char *key)
{
    csc_ini_section_t * sec;
    csc_ini_error_t ret ;

    LOCK;
    if ( ini == NULL ) {
        UNLOCK;
        return CSC_INI_NULL;
    }
    if ( key == NULL ){
        UNLOCK;
        return CSC_INI_NULL;
    }
    sec = csc_ini_getsection(ini, section);
    if ( sec  == NULL ){
        UNLOCK;
        return CSC_INI_NOSECTION;
    }
    ret = csc_ini_section_key_remove(sec, key);

    CHANGED;
    UNLOCK;
    return ret;
}

csc_ini_error_t  csc_ini_section_key_remove(csc_ini_section_t *section, const char *key)
{
    csc_ini_kvstore_t * tmp, *prev;

    if ( section == NULL ) {
        return CSC_INI_NULL;
    }
    if ( key == NULL ){
        return CSC_INI_NULL;
    }
    if ( section->kvstore == NULL) {
        return CSC_INI_SUCCESS;
    }

    if ( strcmp(csc_ini_getkey(section->kvstore), key) == 0) {
        tmp = section->kvstore;
        section->kvstore = tmp->next;
        tmp->next = NULL;
        free_kvstore(tmp);
    } else {
        prev = section->kvstore;
        tmp = section->kvstore->next;
        while (tmp != NULL ){
            if ( strcmp(csc_ini_getkey(tmp), key) == 0) {
                break;
            }
            prev = tmp;
            tmp = tmp->next;
        }
        if ( tmp != NULL ){
            prev -> next  = tmp->next;
            tmp->next = NULL;
            free_kvstore(tmp);
        }
    }
    section->parent->changed = 1;
    return CSC_INI_SUCCESS;
}

csc_ini_error_t csc_ini_section_remove(csc_ini_file_t * ini, const char * section)
{
    csc_ini_section_t * tmp , *prev;

    LOCK;
    if ( ini == NULL ) {
        UNLOCK;
        return CSC_INI_NULL;
    }

    if ( ( ini->sections->section_name == NULL && section == NULL)
            || ( ini->sections->section_name != NULL && section != NULL && strcmp(ini->sections->section_name, section) == 0)){
        if ( ini->sections->next == NULL ) {
            ini->section_tail = NULL;
        }
        tmp = ini->sections;
        ini->sections = ini->sections->next;
        tmp->next = NULL;
        free_section(tmp);
        free(tmp);
    } else {
        prev = ini->sections;
        tmp  = ini->sections->next;

        while (tmp != NULL) {
            if ( section != NULL ){
                if  (tmp->section_name != NULL &&  (strcmp( tmp->section_name, section) == 0)){
                    break;
                }
            } else {
                if (tmp->section_name == section) {
                    break;
                }
            }
            prev = tmp;
            tmp = tmp->next;
        }
        if ( tmp != NULL ) {
            if ( tmp == ini->section_tail ) {
                ini->section_tail = prev;
            }
            prev -> next = tmp->next;
            tmp->next = NULL;
            free_section(tmp);
            free(tmp);
        }
    }
    CHANGED;
    UNLOCK;
    return CSC_INI_SUCCESS;
}

