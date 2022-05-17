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

#ifdef __linux__

#include <sys/sysinfo.h>
#include <unistd.h>
#include <stdlib.h>

#include "cscutils/error_message.h"
#include "cscutils/sysinfo.h"



/*-----------------------------------------------------------------------------
 *  Linux Implementation
 *-----------------------------------------------------------------------------*/

int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap)
{
    struct sysinfo sinfo;
    size_t  mem_unit;

    if ( sysinfo(&sinfo) ) {
        csc_error_message("Failed to call sysinfo.\n");
        return -1;
    }

    /*
     * The sysinfo.mem_unit member variable is not available in older 2.4 kernels.
     * If you have troubles compiling this code, set mem_unit to "1".
     */

    mem_unit = sinfo.mem_unit;
    if ( total_ram != NULL ) {
        *total_ram = sinfo.totalram * mem_unit;
    }
    if ( free_ram  != NULL ) {
        *free_ram  = sinfo.freeram * mem_unit;
    }
    if ( total_swap != NULL ) {
        *total_swap = sinfo.totalswap * mem_unit;
    }

    if ( free_swap != NULL ) {
        *free_swap = sinfo.freeswap * mem_unit;
    }
    return 0;
}


#elif defined(__FreeBSD__) || defined(__DragonFly__)
/*-----------------------------------------------------------------------------
 *  FreeBSD and DragonFlyBSD implementation
 *-----------------------------------------------------------------------------*/
#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

typedef unsigned int u_int;
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include <vm/vm_param.h>
#include <sys/vmmeter.h>

#include "cscutils/error_message.h"
#include "cscutils/sysinfo.h"


int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap)
{
    char buf[80], *used_str, *total_str;
    char *saveptr;
    /* Stuff for sysctl */
    int memory;
    size_t len;
    /* Stuff for swap display */
    int used, total, _free;
    FILE *pipe;

    len=sizeof(memory);
    sysctlbyname("hw.physmem", &memory, &len, NULL, 0);


    // total physical memory (without swap space)
    if ( total_ram != NULL ) {
        *total_ram = memory * 1024;
    }


    int free_mem = 0 ;

    len = sizeof (free_mem);
    if ((sysctlbyname("vm.stats.vm.v_free_count", &free_mem, &len, NULL, 0) == -1) || !len) {
        return -1;
    }

    if ( free_ram  != NULL ) {
        *free_ram  = free_mem *  sysconf(_SC_PAGESIZE) ;
    }


    if ((pipe = popen("/usr/sbin/pstat -ks", "r")) == NULL) {
        used = total = 1;
        return -1 ;
    }
    fgets(buf, sizeof(buf), pipe);
    fgets(buf, sizeof(buf), pipe);
    fgets(buf, sizeof(buf), pipe);
    fgets(buf, sizeof(buf), pipe);
    pclose(pipe);

    saveptr = NULL;
    strtok_r(buf, " ", &saveptr);
    total_str = (char * )strtok_r(NULL, " ", &saveptr);
    used_str =  (char * )strtok_r(NULL, " ", &saveptr);
    used = atoi(used_str);
    total = atoi(total_str);

    _free=total-used;

    if ( total_swap != NULL ) {
        *total_swap = total * 1024;
    }

    if ( free_swap != NULL ) {
        *free_swap = _free * 1024;
    }

    return 0;
}



#elif defined(__NetBSD__) || defined(__OpenBSD__)

/*-----------------------------------------------------------------------------
 *  Net and OpenBSD
 *-----------------------------------------------------------------------------*/
#include <sys/param.h>
#if __NetBSD_Version__ > 103080000
#define UVM
#endif
#if defined(__OpenBSD__)
#define UVM
#endif

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#ifdef UVM
#include <uvm/uvm_extern.h>
#else
#include <vm/vm_swap.h>
#endif

#define NO_MEMORY_INFO 0
#warning untested

int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap)
{
    int mib[2];
    size_t len;
#ifdef UVM
#if __NetBSD_Version__ > 106000000
    struct  uvmexp_sysctl uvmexp;
#else
    struct  uvmexp uvmexp;
#endif
#else
    struct swapent *swaplist;
    int64_t nswap, rnswap, totalswap, freeswap, usedswap;
#endif
#if __NetBSD_Version__ > 106170000 /* 1.6Q+ */
    quad_t memory;
#else
    int memory;
#endif

    info->buffers=0 ;
    /* memory */
#if __NetBSD_Version__ > 106170000 /* 1.6Q+ */
    mib[0] = CTL_HW;
    mib[1] = HW_PHYSMEM64;
#else
    mib[0] = CTL_HW;
    mib[1] = HW_PHYSMEM;
#endif
    len = sizeof(memory);
    if( sysctl(mib,2,&memory,&len,NULL,0)< 0 )
        info->memtotal    = NO_MEMORY_INFO;
    else
        info->memtotal    = memory;

#ifdef UVM
    mib[0] = CTL_VM;
#if __NetBSD_Version__ > 106000000
    mib[1] = VM_UVMEXP2;
#else
    mib[1] = VM_UVMEXP;
#endif
    len = sizeof(uvmexp);
    if ( sysctl(mib, 2, &uvmexp, &len, NULL, 0) < 0 ) {
        info->memfree     = NO_MEMORY_INFO;
        info->swaptotal     = NO_MEMORY_INFO;
        info->swapfree  = NO_MEMORY_INFO;
    } else {
        unsigned mess_int_t pgsz = uvmexp.pagesize;
        info->memfree     = pgsz * uvmexp.free;
        info->swaptotal    = pgsz * uvmexp.swpages;
        info->swapfree = pgsz * (uvmexp.swpages - uvmexp.swpginuse);
    }
#else
    info->memfree = NO_MEMORY_INFO;

    /* swap */
    totalswap = freeswap = usedswap = 0;
    nswap = swapctl(SWAP_NSWAP,0,0);
    if ( nswap > 0 ) {
        if ( (swaplist = (struct swapent *)malloc(nswap * sizeof(*swaplist))) ) {
            rnswap = swapctl(SWAP_STATS,swaplist,nswap);
            if ( rnswap < 0 || rnswap > nswap )
                totalswap = freeswap = -1;  /* Error */
            else {
                while ( rnswap-- > 0 ) {
                    totalswap += swaplist[rnswap].se_nblks;
                    usedswap += swaplist[rnswap].se_inuse;
                }
                freeswap = totalswap - usedswap;
            }
        } else
            totalswap = freeswap = -1;  /* Error */

        if ( totalswap == -1 ) {
            info->swaptotal = NO_MEMORY_INFO;
            info->swapfree =  NO_MEMORY_INFO;
        } else {
            info->swaptotal = totalswap;
            info->swapfree = freeswap;
        }
    }
#endif
    return 0;

    if ( total_ram != NULL ) {
        *total_ram = 0;
    }
    if ( free_ram  != NULL ) {
        *free_ram  = 0;
    }
    if ( total_swap != NULL ) {
        *total_swap = 0;
    }

    if ( free_swap != NULL ) {
        *free_swap = 0;
    }
    return 0;
}

#elif defined(_WIN32) || defined(_WIN64)

/*-----------------------------------------------------------------------------
 *  Windows
 *-----------------------------------------------------------------------------*/

#include <windows.h>

int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap)
{
    MEMORYSTATUSEX statex;
    statex.dwLength = sizeof(statex);
    GlobalMemoryStatusEx(&statex);

    if ( total_ram != NULL ) {
        *total_ram = statex.ullTotalPhys;
    }
    if ( free_ram  != NULL ) {
        *free_ram  = statex.ullAvailPhys;
    }
    if ( total_swap != NULL ) {
        *total_swap = statex.ullTotalPageFile - statex.ullTotalPhys;
    }

    if ( free_swap != NULL ) {
        *free_swap = statex.ullAvailPageFile - statex.ullAvailPhys;
    }
    return 0;
}

#else

/*-----------------------------------------------------------------------------
 *  Not available
 *-----------------------------------------------------------------------------*/
#include <stdlib.h>
#include "cscutils/error_message.h"
#include "cscutils/sysinfo.h"

#warning The memory information is not supported on the current OS.

/*-----------------------------------------------------------------------------
 *  Linux Implementation
 *-----------------------------------------------------------------------------*/

int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap)
{
    if ( total_ram != NULL ) {
        *total_ram = 0;
    }
    if ( free_ram  != NULL ) {
        *free_ram  = 0;
    }
    if ( total_swap != NULL ) {
        *total_swap = 0;
    }

    if ( free_swap != NULL ) {
        *free_swap = 0;
    }
    csc_error_message("The memory information is not supported on the current OS.\n");
    return -1;
}
#endif
