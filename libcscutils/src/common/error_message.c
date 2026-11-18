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

#include "cscutils/error_message.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned csc_get_term_width(void)
{
#if defined(CSC_HAVE_IOCTL) && defined(CSC_HAVE_UNISTD)
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) != 0)
    {
        return 80;
    }
    return ws.ws_col;
#else
    return 80;
#endif
}

/*-----------------------------------------------------------------------------
 *  Default Print Functions
 *-----------------------------------------------------------------------------*/
static void error_print(const char* str) { fprintf(stderr, "ERROR:   %s\n", str); }

static void warn_print(const char* str) { fprintf(stderr, "WARNING: %s\n", str); }

static void info_print(const char* str) { printf("INFO: %s\n", str); }

static void print_print(const char* str) { printf("%s\n", str); }

csc_error_print_t error_handle = error_print;
csc_error_print_t warn_handle = warn_print;
csc_error_print_t info_handle = info_print;
csc_error_print_t print_handle = print_print;

/*-----------------------------------------------------------------------------
 *  Set new Print handles
 *-----------------------------------------------------------------------------*/
void* csc_error_message_handle(csc_error_print_t fn)
{
    void* ret;
    *(void**)&ret = *(void**)&error_handle;
    if (fn == NULL)
    {
        error_handle = error_print;
    }
    else
    {
        error_handle = fn;
    }
    return ret;
}

void* csc_warn_message_handle(csc_error_print_t fn)
{
    void* ret;
    *(void**)&ret = *(void**)&warn_handle;

    if (fn == NULL)
    {
        warn_handle = warn_print;
    }
    else
    {
        warn_handle = fn;
    }
    return ret;
}

void* csc_info_message_handle(csc_error_print_t fn)
{
    void* ret;
    *(void**)&ret = *(void**)&info_handle;
    if (fn == NULL)
    {
        info_handle = info_print;
    }
    else
    {
        info_handle = fn;
    }
    return ret;
}

void* csc_print_message_handle(csc_error_print_t fn)
{
    void* ret;
    *(void**)&ret = *(void**)&print_handle;
    if (fn == NULL)
    {
        print_handle = print_print;
    }
    else
    {
        print_handle = fn;
    }
    return ret;
}

/*-----------------------------------------------------------------------------
 *  Hook malloc implementation we use, necessary if we use MATLAB
 *-----------------------------------------------------------------------------*/
typedef void* (*malloc_call)(size_t size);
typedef void* (*realloc_call)(void* ptr, size_t size);
typedef void (*free_call)(void* ptr);

static malloc_call error_malloc = malloc;
static realloc_call error_realloc = realloc;
static free_call error_free = free;

void csc_error_message_memory(void* m, void* r, void* f)
{
    *(void**)&error_malloc = m;
    *(void**)&error_realloc = r;
    *(void**)&error_free = f;
}

/*
 * Old Style loging.
 */
static char* make_message(int size, const char* fmt, va_list ap)
{
    int n = 0;
    char* p;

    if ((p = error_malloc((size_t)size)) == NULL)
        return NULL;

    /* Try to print in the allocated space. */
    n = (int)vsnprintf(p, size, fmt, ap);

    /* If that worked, return the string. */
    if (n > -1)
        return p;

    error_free(p);
    return NULL;
}

// weak symbols are not in static library by mingw compiler
#if defined(_WIN32) || defined(_WIN64)
void csc_error_message(const char* fmt, ...)
#else
__attribute__((weak)) void csc_error_message(const char* fmt, ...)
#endif
{
    char* str = NULL;
    va_list ap;
    size_t size = 0;

    /* compute size  */
    va_start(ap, fmt);
    size = vsnprintf(str, size, fmt, ap);
    va_end(ap);
    size++; /* for terminating character*/

    /* Build the message  */
    va_start(ap, fmt);
    str = make_message(size, fmt, ap);
    va_end(ap);

    error_handle(str);
    error_free(str);
}

// weak symbols are not in static library by mingw compiler
#if defined(_WIN32) || defined(_WIN64)
void csc_warn_message(const char* fmt, ...)
#else
__attribute__((weak)) void csc_warn_message(const char* fmt, ...)
#endif
{
    char* str = NULL;
    va_list ap;
    size_t size = 0;

    /* compute size  */
    va_start(ap, fmt);
    size = vsnprintf(str, size, fmt, ap);
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

// weak symbols are not in static library by mingw compiler
#if defined(_WIN32) || defined(_WIN64)
void csc_info_message(const char* fmt, ...)
#else
__attribute__((weak)) void csc_info_message(const char* fmt, ...)
#endif
{
    char* str = NULL;
    va_list ap;
    size_t size = 0;

    /* compute size  */
    va_start(ap, fmt);
    size = vsnprintf(str, size, fmt, ap);
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

// weak symbols are not in static library by mingw compiler
#if defined(_WIN32) || defined(_WIN64)
void csc_print_message(const char* fmt, ...)
#else
__attribute__((weak)) void csc_print_message(const char* fmt, ...)
#endif
{
    char* str = NULL;
    va_list ap;
    size_t size = 0;

    /* compute size  */
    va_start(ap, fmt);
    size = vsnprintf(str, size, fmt, ap);
    va_end(ap);
    size++; /* for terminating character*/

    /* Build the message  */
    va_start(ap, fmt);
    str = make_message(size, fmt, ap);
    va_end(ap);

    /* Print the message  */
    print_handle(str);

    error_free(str);
}

/*-----------------------------------------------------------------------------
 *  Logfmt-style Logging
 *-----------------------------------------------------------------------------*/
static char logfmt_prefix[129] = {0};

void csc_log_prefix(const char* prefix)
{
    strncpy(logfmt_prefix, prefix, 128);
    logfmt_prefix[128] = '\0';
    return;
}

static void logfmt_print(csc_log_level_t level, int sublevel, const char* fmt, va_list ap)
{
    char* message = NULL;
    char* hostname = NULL;
    char* buffer = NULL;
    int msg_size = 0;
    int buf_size = 0;

    /* Get hostname */
    hostname = csc_sysinfo_hostname();
    if (!hostname)
        hostname = strdup("unknown");

    /* Build the message */
    va_list ap_copy;
    va_copy(ap_copy, ap);
    msg_size = vsnprintf(NULL, 0, fmt, ap_copy);
    va_end(ap_copy);

    if (msg_size < 0)
    {
        free(hostname);
        return;
    }

    message = error_malloc(msg_size + 1);
    if (!message)
    {
        free(hostname);
        return;
    }

    vsnprintf(message, msg_size + 1, fmt, ap);

    /* Build the logfmt buffer with level, message, hostname, pid */
    buf_size = msg_size + strlen(hostname) + 2 * 128;
    buffer = error_malloc(buf_size);
    if (!buffer)
    {
        error_free(message);
        free(hostname);
        return;
    }

    /* Format: logfmt_prefix: level=info message="..." hostname="..." pid=123 */
    int written = snprintf(buffer, buf_size, "%s: level=%s message=\"%s\" hostname=\"%s\" pid=%d", logfmt_prefix,
                           level == CSC_LOG_LEVEL_DEBUG   ? "debug"
                           : level == CSC_LOG_LEVEL_INFO  ? "info"
                           : level == CSC_LOG_LEVEL_WARN  ? "warn"
                           : level == CSC_LOG_LEVEL_ERROR ? "error"
                                                          : "critical",
                           message, hostname, csc_sysinfo_getpid());

    if (sublevel > 0)
    {
        /* For debug with sublevel: level=debug:1 */
        char* tmp = error_malloc(buf_size + 32);
        if (tmp)
        {
            written = snprintf(tmp, buf_size + 32, "%s: level=debug:%d message=\"%s\" hostname=\"%s\" pid=%d", logfmt_prefix,
                               sublevel, message, hostname, csc_sysinfo_getpid());
            error_free(buffer);
            buffer = tmp;
        }
    }

    /* Print the formatted log message */
    printf("%s\n", buffer);

    error_free(message);
    error_free(buffer);
    free(hostname);
}

void csc_log_message(csc_log_level_t level, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(level, 0, fmt, ap);
    va_end(ap);
}

void csc_log_debug_message(int sublevel, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(CSC_LOG_LEVEL_DEBUG, sublevel, fmt, ap);
    va_end(ap);
}

void csc_log_info_message(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(CSC_LOG_LEVEL_INFO, 0, fmt, ap);
    va_end(ap);
}

void csc_log_warn_message(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(CSC_LOG_LEVEL_WARN, 0, fmt, ap);
    va_end(ap);
}

void csc_log_error_message(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(CSC_LOG_LEVEL_ERROR, 0, fmt, ap);
    va_end(ap);
}

void csc_log_critical_message(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    logfmt_print(CSC_LOG_LEVEL_CRITICAL, 0, fmt, ap);
    va_end(ap);
}
