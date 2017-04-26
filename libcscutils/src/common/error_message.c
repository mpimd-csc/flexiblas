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

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "cscutils/error_message.h"


/*-----------------------------------------------------------------------------
 *  Default Print Functions 
 *-----------------------------------------------------------------------------*/
static void error_print(const char * str){
    fprintf(stderr, "ERROR:   %s\n", str);
}

static void warn_print(const char * str){
    fprintf(stderr, "WARNING: %s\n", str);
}

static void info_print(const char *str) {
    printf("INFO: %s\n", str);
}

csc_error_print_t error_handle = error_print; 
csc_error_print_t warn_handle  = warn_print; 
csc_error_print_t info_handle  = info_print; 


/*-----------------------------------------------------------------------------
 *  Set new Print handles
 *-----------------------------------------------------------------------------*/
void * csc_error_message_handle( csc_error_print_t fn){
    void *ret = (void *) error_handle; 
    if ( fn == NULL ) {
        error_handle  = error_print; 
    } else {
        error_handle = fn ; 
    }
    return ret; 
}

void * csc_warn_message_handle( csc_error_print_t fn){
    void * ret = (void * )warn_handle; 
    if ( fn == NULL ) {
        warn_handle = warn_print; 
    } else {
        warn_handle = fn; 
    }
    return ret; 
}

void * csc_info_message_handle( csc_error_print_t fn){
    void * ret = (void *)  info_handle; 
    if ( fn == NULL ) {
        info_handle = info_print; 
    } else {
        info_handle = fn; 
    }
    return ret; 
}



/*-----------------------------------------------------------------------------
 *  Hook malloc implementation we use, necessary if we use MATLAB 
 *-----------------------------------------------------------------------------*/
typedef void * (*malloc_call)(size_t size); 
typedef void * (*realloc_call)(void * ptr, size_t size); 
typedef void (*free_call)(void *ptr); 

static malloc_call error_malloc = malloc;
static realloc_call error_realloc = realloc; 
static free_call    error_free 	  = free; 

void csc_error_message_memory(void *m, void *r, void *f ){
    error_malloc = (malloc_call) m; 
    error_realloc= (realloc_call) r; 
    error_free   = (free_call) f; 
}

static char * make_message(int size, const char *fmt, va_list ap)
{
    int n = 0;
    char *p;

    if ((p = error_malloc((size_t)size)) == NULL)
        return NULL;


    /* Try to print in the allocated space. */
    n = (int) vsnprintf(p, size, fmt, ap);

    /* If that worked, return the string. */
    if (n > -1)
        return p;

    error_free(p);
    return NULL;
}

__attribute__((weak)) void csc_error_message(const char *fmt, ...)
{
    char *str; 
    va_list ap; 
    size_t size=0;

    /* compute size  */
    va_start(ap, fmt); 
    size = vsnprintf(NULL, size, fmt, ap);
    va_end(ap);
    size++; /* for terminating character*/

    /* Build the message  */
    va_start(ap, fmt); 
    str = make_message(size, fmt, ap);
    va_end(ap);


    error_handle(str); 
    error_free(str); 
}

__attribute__((weak)) void csc_warn_message(const char *fmt, ...)  
{
    char *str; 
    va_list ap;
    size_t size=0;

    /* compute size  */
    va_start(ap, fmt); 
    size = vsnprintf(NULL, size, fmt, ap);
    va_end(ap);
    size++; /* for terminating character*/

    /* Build the message  */
    va_start(ap, fmt); 
    str = make_message(size, fmt, ap); 
    va_end(ap);

    /* Print the message  */	
    warn_handle(str); 

    error_free(str); 

}

__attribute__((weak)) void csc_info_message(const char *fmt, ...) 
{
    char *str; 
    va_list ap;
    size_t size=0;

    /* compute size  */
    va_start(ap, fmt); 
    size = vsnprintf(NULL, size, fmt, ap);
    va_end(ap);
    size++; /* for terminating character*/

    /* Build the message  */
    va_start(ap, fmt); 
    str = make_message(size, fmt, ap); 
    va_end(ap);

    /* Print the message  */	
    info_handle(str); 

    error_free(str); 
}


