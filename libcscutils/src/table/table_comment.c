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
#include <omp.h>

#include "cscutils/error_message.h"
#include "cscutils/strutils.h"
#include "cscutils/table.h"
#include "cscutils/sysinfo.h"

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

int csc_table_comment_text(csc_table_t *t, const char * text)
{
    return csc_table_comment_printf(t, "%s", text);
}

void csc_table_comment_date(csc_table_t *t ) {
    time_t tp;
    char buffer[CSC_TABLE_MAXLEN];
    struct tm  result;
    if ( !t) return;
    if (!t->comment) return;
    tp = time(NULL);
#if defined(_WIN32) || defined(_WIN64)
    localtime_s(&result, &tp);
#else
    localtime_r(&tp, &result);
#endif
    strftime(buffer, CSC_TABLE_MAXLEN, "%a, %d %b %Y %H:%M:%S ", &result);
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


void csc_table_comment_sysinfo(csc_table_t * t)
{
    char buffer[32768];
    if ( !t ) return;
    if ( !t->comment) return;

    char * tmp, *tmp2;

    csc_table_comment_add(t->comment, "=== System Information ===");
    tmp = csc_sysinfo_hostname();
    snprintf(buffer, 32767, "Hostname: %s", tmp);
    csc_table_comment_add(t->comment, buffer);
    if (tmp) free(tmp);


    tmp = csc_sysinfo_sysname();
    tmp2 = csc_sysinfo_release();
    snprintf(buffer, 32767, "Opering System: %s (%s)", tmp, tmp2);
    csc_table_comment_add(t->comment, buffer);
    if (tmp) free(tmp);
    if (tmp2) free(tmp2);

    tmp = csc_sysinfo_machine();
    snprintf(buffer, 32767, "Architecture: %s", tmp);
    csc_table_comment_add(t->comment, buffer);
    if (tmp) free(tmp);

    tmp = csc_sysinfo_ccompiler();
    snprintf(buffer, 32767, "Compiler: %s", tmp);
    csc_table_comment_add(t->comment, buffer);
    if (tmp) free(tmp);

    tmp = csc_sysinfo_cpuname();
    snprintf(buffer, 32767, "CPU: %s", tmp);
    csc_table_comment_add(t->comment, buffer);
    if (tmp) free(tmp);

    unsigned int count = csc_sysinfo_cpu_count();
    snprintf(buffer, 32767, "CPU Cores: %u", count);
    csc_table_comment_add(t->comment, buffer);

    size_t memfree, memtotal = 0 , swapfree, swaptotal;
    csc_sysinfo_memory(&memtotal, &memfree, &swaptotal, &swapfree);

    memtotal /= 1024;
    memtotal /= 1024;
    memtotal = (memtotal+1023)/1024;
    memtotal *= 1024;

    snprintf(buffer, 32767, "Memory: %llu GiB", (unsigned long long) memtotal/1024);
    csc_table_comment_add(t->comment, buffer);

    csc_table_comment_add(t->comment, "===========================");
    csc_table_comment_add(t->comment, "");
    return;

}

// https://man7.org/linux/man-pages/man7/environ.7.html
extern char **environ;
void csc_table_comment_openmp_info(csc_table_t *t)
{
#ifdef _OPENMP
    int i = 0;
    if ( ! t ) return;
    csc_table_comment_add(t->comment, "=== OpenMP Environment  ===");
    for ( i = 0; environ[i] != NULL; i++) {
        const char * s = environ[i];
        if (strstr(s, "OMP") == s) {
            csc_table_comment_add(t->comment, s);
        }
    }
    csc_table_comment_add(t->comment, "omp_get_num_threads: %d", omp_get_num_threads());
    csc_table_comment_add(t->comment, "omp_get_num_procs: %d", omp_get_num_procs());
    csc_table_comment_add(t->comment, "omp_get_dynamic: %d", omp_get_dynamic());
    csc_table_comment_add(t->comment, "omp_get_nested: %d", omp_get_nested());
    omp_sched_t sched;
    int mod;
    omp_get_schedule(&sched, &mod);
    switch(sched) {
        case omp_sched_static:
            csc_table_comment_add(t->comment, "omp_get_schedule: static (%d)", mod);
            break;
        case omp_sched_dynamic:
            csc_table_comment_add(t->comment, "omp_get_schedule: dynamic (%d)", mod);
            break;
        case omp_sched_guided:
            csc_table_comment_add(t->comment, "omp_get_schedule: guided (%d)", mod);
            break;
        case omp_sched_auto:
            csc_table_comment_add(t->comment, "omp_get_schedule: auto (%d)", mod);
            break;
        default:
            csc_table_comment_add(t->comment, "omp_get_schedule: %d (%d)", (int) sched, mod);
    }
    csc_table_comment_add(t->comment, "omp_get_thread_limit: %d", omp_get_thread_limit());
    csc_table_comment_add(t->comment, "omp_get_max_active_levels: %d", omp_get_max_active_levels());
#if _OPENMP > 201107
    omp_proc_bind_t pr = omp_get_proc_bind();
    switch(pr) {
        case omp_proc_bind_false:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: false");
            break;
        case omp_proc_bind_true:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: true");
            break;
        case omp_proc_bind_close:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: close");
            break;
        case omp_proc_bind_master:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: master");

            break;
        case omp_proc_bind_spread:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: spread");

            break;
        default:
            csc_table_comment_add(t->comment, "omp_get_proc_bind: %d", (int) pr);
    }
#endif
    csc_table_comment_add(t->comment, "===========================");
    csc_table_comment_add(t->comment, "");
#else
    return;
#endif

}

void csc_table_comment_cmake(csc_table_t *t)
{
    if ( ! t ) return;

    csc_table_comment_add(t->comment, "=== CMAKE Command Line ====");
    csc_table_comment_add(t->comment, "%s", csc_sysinfo_cmake_args());
    csc_table_comment_add(t->comment, "===========================");
    csc_table_comment_add(t->comment, "");



}

void csc_table_comment_compilerflags(csc_table_t *t)
{
    if ( ! t ) return;
    const char *cf = csc_sysinfo_c_flags();
    const char *cxxf = csc_sysinfo_cxx_flags();
    const char *ff = csc_sysinfo_fortran_flags();
    const char *hipf = csc_sysinfo_hip_flags();
    const char *cudaf = csc_sysinfo_cuda_flags();

    csc_table_comment_add(t->comment, "=== Compiler Flags ========");

    if ( cf ) {
        csc_table_comment_add(t->comment, "C:       %s", cf);
    }
    if ( cxxf ) {
        csc_table_comment_add(t->comment, "C++:     %s", cxxf);
    }
    if ( ff ) {
        csc_table_comment_add(t->comment, "Fortran: %s", ff);
    }
    if ( hipf ) {
        csc_table_comment_add(t->comment, "HIP:     %s", hipf);
    }
    if ( cudaf ) {
        csc_table_comment_add(t->comment, "CUDA:    %s", cudaf);
    }

    csc_table_comment_add(t->comment, "===========================");
    csc_table_comment_add(t->comment, "");
    return;
}

void csc_table_comment_allinfo(csc_table_t * t)
{
    csc_table_comment_sysinfo(t);
    csc_table_comment_openmp_info(t);
    csc_table_comment_cmake(t);
    csc_table_comment_compilerflags(t);
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
