/*
 * CSCUTILS - A collection of various software routines uses in CSC projects
 * Copyright (C) Martin Koehler, 2016
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
#include <unistd.h>
#include <sys/time.h>
#include <stdint.h>
#include <time.h>

#include "cscutils/counter.h"

double csc_wtime(void)
{
    /** struct timeval tv; */
    struct timespec ts;
    double ret;
    /** gettimeofday (&tv, NULL); */
    clock_gettime(CLOCK_REALTIME, &ts);
    ret = ts.tv_sec + ts.tv_nsec / 1e9;
    return ret;
}

#if 0
double csc_wtime_()
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return tv.tv_sec + tv.tv_usec / 1e6;
}
#endif

double csc_ctime(void) {
    clock_t  t = clock();
    return t / (double)(CLOCKS_PER_SEC);
}

#if 0
double csc_ctime_() {
    clock_t  t = clock();
    return t / (double)(CLOCKS_PER_SEC);
}

#endif

#if defined(__i386__)

int64_t csc_cycles(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return (uint64_t) x;
}

#if 0
int64_t csc_cycles_(void)
{
    unsigned long long int x;
    __asm__ volatile (".byte 0x0f, 0x31" : "=A" (x));
    return (int64_t) x;
}
#endif

#elif defined(__x86_64__)

int64_t csc_cycles(void)
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return (int64_t) (( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}

#if 0
int64_t csc_cycles_(void)
{
    unsigned hi, lo;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return (int64_t) (( (unsigned long long)lo)|( ((unsigned long long)hi)<<32 ));
}
#endif

#elif defined(__powerpc__)

int64_t csc_cycles(void)
{
    unsigned long long int result=0;
    unsigned long int upper, lower,tmp;
    __asm__ volatile(
            "0:                  \n"
            "\tmftbu   %0           \n"
            "\tmftb    %1           \n"
            "\tmftbu   %2           \n"
            "\tcmpw    %2,%0        \n"
            "\tbne     0b         \n"
            : "=r"(upper),"=r"(lower),"=r"(tmp)
            );
    result = upper;
    result = result<<32;
    result = result|lower;

    return (int64_t) (result);
}

#if 0
int64_t csc_cycles_(void)
{
    unsigned long long int result=0;
    unsigned long int upper, lower,tmp;
    __asm__ volatile(
            "0:                  \n"
            "\tmftbu   %0           \n"
            "\tmftb    %1           \n"
            "\tmftbu   %2           \n"
            "\tcmpw    %2,%0        \n"
            "\tbne     0b         \n"
            : "=r"(upper),"=r"(lower),"=r"(tmp)
            );
    result = upper;
    result = result<<32;
    result = result|lower;

    return (int64_t) (result);
}
#endif
#else
#warning RDTSC is not supported on this platform. DO NOT TRUST THE RESULTS
int64_t csc_cycles(void)
{
    static int64_t counter = 0;
    return counter++;
}

#if 0
int64_t csc_cycles_(void)
{
    static int64_t counter = 0;
    return counter++;
}
#endif
#endif

