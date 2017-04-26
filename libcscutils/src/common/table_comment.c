/* 
 * libcscutils - Helper Routines of the CSC group  
 * Copyright (C) Martin Koehler, 2017
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
#include <stdarg.h>
#include <time.h>
#include "cscutils/error_message.h"
#include "cscutils/strutils.h"
#include "cscutils/table.h"

int csc_table_comment_printf(csc_table_t *t, const char * comment, ...)
{
    va_list ap; 
    int r; 
    if (!t) return -1; 
    if (!t->comment) return -1; 

    va_start (ap, comment); 
    r = csc_table_comment_add_va(t->comment, comment, ap); 
    va_end(ap); 
    return r; 
}

int csc_table_comment_sign(csc_table_t *t, const char *sign){
    if (!t) return -1; 
    if (!t->comment) return -1; 
    csc_table_comment_start(t->comment, sign); 
    return 0; 
}

void csc_table_comment_date(csc_table_t *t ) {
    time_t tp; 
    char buffer[CSC_TABLE_MAXLEN]; 
    struct tm * tmp; 
    if ( !t) return; 
    if (!t->comment) return; 
    tp = time(NULL); 
    tmp = localtime(&tp); 
    strftime(buffer, CSC_TABLE_MAXLEN, "%a, %d %b %Y %T ", tmp); 
    csc_table_comment_add(t->comment, "Date: %s", buffer); 
    return; 

}

void csc_table_comment_cmd(csc_table_t *t, int argc, char ** argv) {
    char buffer[32768]; 
    int pos = 0; 
    int i; 
    if ( !t ) return; 
    if ( !t->comment) return; 
    if ( argc <= 0 ) return; 
    if ( argv == NULL) return; 

    pos = pos + snprintf(buffer, 32768-pos, "Commandline:"); 
    for (i = 0; i < argc; i++) {
       pos = pos + snprintf(buffer+pos, 32768-pos," %s", argv[i]);        
    }
    buffer[pos] = '\0'; 
    csc_table_comment_add(t->comment, buffer); 
    return; 
}


/* Internal functions  */
void csc_table_comment_print(FILE *stream, csc_table_comment_t *c) {
    int i; 
    if ( ! c ) return; 
    for (i = 0; i < c->len; i++) {
        fprintf(stream, "%s%s\n",c->start,c->lines[i]);
    }
    return; 
}


csc_table_comment_t * csc_table_new_comment(void) {
    csc_table_comment_t * c; 
    c = (csc_table_comment_t  *) malloc(sizeof(csc_table_comment_t ) * (1)); 
    if (!c) return NULL; 
    c->len = 0; 
    c->lines = NULL; 
    strncpy(c->start, "# ", CSC_TABLE_MAXLEN); 
    return c; 
}

void csc_table_destroy_comment(csc_table_comment_t * c) {
    int i; 
    if (!c) return; 
    for (i = 0; i < c->len; i++) {
        if ( c->lines[i]) free(c->lines[i]); 
    }
    if (c->lines) free(c->lines); 
    free(c); 
    return; 
}

void csc_table_comment_start(csc_table_comment_t *c, const char *start){
    if ( !c) return; 
    strncpy(c->start, start, CSC_TABLE_MAXLEN); 
    return; 
}



int  csc_table_comment_add(csc_table_comment_t *c, const char * fmt, ...){
    int n;
    int size = 100;     
    char *p, *np;
    va_list ap;

    if ((p = malloc(size)) == NULL)
        return -1; 

    while (1) {
        va_start(ap, fmt);
        n = vsnprintf(p, size, fmt, ap);
        va_end(ap);

        /* If that worked, return the string. */
        if (n > -1 && n < size)
            break; 
        /* Else try again with more space. */
        if (n > -1)    /* glibc 2.1 */
            size = n+1; /* precisely what is needed */
        else           /* glibc 2.0 */
            size *= 2;  /* twice the old size */
        if ((np = realloc (p, size)) == NULL) {
            free(p);
            p = NULL; 
            break; 
        } else {
            p = np;
        }
    }

    if ( p ) {
        c->len ++ ;
        c->lines = realloc(c->lines, c->len * sizeof(char*)); 
        if ( !c->lines ) {
            c->len = 0; 
            free(p); 
            return -1; 
        }
        size = strlen(p); 
        for (n = 0; n < size; n++) {
            if ( p[n] == '\n' || p[n] == '\r') {
                p[n] = ' '; 
            }
        }
        c->lines[c->len-1] = p; 
        return 0; 
    } else {
        csc_error_message("Failed to add comment string.\n"); 
        return -1; 
    }
}
 
int  csc_table_comment_add_va(csc_table_comment_t *c, const char * fmt, va_list apx ){
    int n;
    int size = 100;     
    char *p, *np;
    va_list ap;

    if ((p = malloc(size)) == NULL)
        return -1;

    while (1) {
        va_copy(ap, apx);
        n = vsnprintf(p, size, fmt, ap);
        va_end(ap);

        /* If that worked, return the string. */
        if (n > -1 && n < size)
            break; 
        /* Else try again with more space. */
        if (n > -1)    /* glibc 2.1 */
            size = n+1; /* precisely what is needed */
        else           /* glibc 2.0 */
            size *= 2;  /* twice the old size */
        if ((np = realloc (p, size)) == NULL) {
            free(p);
            p = NULL; 
            break; 
        } else {
            p = np;
        }
    }

    if ( p ) {
        c->len ++ ;
        c->lines = realloc(c->lines, c->len * sizeof(char*)); 
        if ( !c->lines ) {
            c->len = 0; 
            free(p); 
            return -1; 
        }
        size = strlen(p); 
        for (n = 0; n < size; n++) {
            if ( p[n] == '\n' || p[n] == '\r') {
                p[n] = ' '; 
            }
        }

        c->lines[c->len-1] = p; 
        return 0; 
    } else {
        csc_error_message("Failed to add comment string.\n"); 
        return -1; 
    }
}

void csc_table_comment_clear(csc_table_comment_t *c) {
    int i; 
    if (!c) return; 

    for (i = 0; i < c->len; i++) {
        if ( c->lines[i] ) free(c->lines[i]); 
    }
    if ( c->lines ) free(c->lines); 
    c->lines = NULL; 
    c->len = 0; 
    return; 
}

