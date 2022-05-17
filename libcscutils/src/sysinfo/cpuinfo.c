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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include "cscutils/error_message.h"

#if defined(_WIN32) || defined(_WIN64)
    #include <intrin.h>
#endif

#if defined(__amd64__) || defined(__amd64) || defined(__x86_64__) || defined(__x86_64) || defined(_M_AMD64)

static inline void __cpuid_intrinsic(unsigned int* reg, unsigned int id, unsigned sub_id) {
#if defined(_WIN32) || defined(_WIN64)
    __cpuidex((int*)reg,(int)id,(int)sub_id);
#elif defined(__GNUC__) || defined(__clang__) || defined(__INTEL_COMPILER)
    unsigned int* eax = reg + 0;
    unsigned int* ebx = reg + 1;
    unsigned int* ecx = reg + 2;
    unsigned int* edx = reg + 3;
    __asm__ volatile("cpuid" : "=a"(*eax),"=b"(*ebx),"=c"(*ecx),"=d"(*edx) : "0"(id), "2"(sub_id));
#else
    reg[0] = 0;
    reg[1] = 0;
    reg[2] = 0;
    reg[3] = 0;
    (void)id;
    (void)sub_id;
#endif
}

int csc_sysinfo_cpuinfo(int *lcpu, int *pcpu, int *ht){
    uint32_t regs[4];
    // Get vendor
    uint32_t vendor[3];
    __cpuid_intrinsic(regs, 0, 0);
    vendor[0] = regs[1]; // EBX
    vendor[1] = regs[3]; // EDX
    vendor[2] = regs[2]; // ECX

    int hyper_threading = 0;
    uint32_t logical = 1;
    uint32_t physical = 1;

    if (strncmp((char*)vendor,"GenuineIntel",12) == 0) {
        {
            __cpuid_intrinsic(regs, 1, 0);
            uint32_t eax = regs[0];
            uint32_t model     = (eax >>  4) & 0xf;
            uint32_t family    = (eax >>  8) & 0xf;
            uint32_t ext_model = (eax >> 16) & 0xf;

            // Hybrid core architectures require special care
            // as the two core types report different values
            if (family == 0x6 && ext_model == 0x9 && (model == 0x7 || model == 0xa)) {
                csc_warn_message("csc_cpuinfo does not work on Alder Lake.\n");
                return 1;
            }

            uint32_t edx = regs[3];

            hyper_threading = (edx >> 28) & 0x1;
        }

        /*
         * Leaf 0x1f is an extension of 0x1b
         * and the recommended leaf if it is available
         * TODO: Use leaf 0x1f to identify logical cores on hybrid archs
         * TODO: For this we may just use the level type 5 (Die)
         */
        uint32_t x1f_exists = 0;

        {
            __cpuid_intrinsic(regs, 0x1f, 42);
            uint32_t ecx = regs[2];

            if ((ecx & 0xff) == 42) {
                x1f_exists = 1;
            }
        }

        uint32_t smt_multiplier = 1;

        uint32_t leaf = (x1f_exists) ? 0x1f : 0xb;

        /*
         * It is unclear how many levels there are, if necessary,
         * check more levels
         */
        for (int i = 0; i < 16; i++) {
            __cpuid_intrinsic(regs, leaf, i);
            uint32_t ebx = regs[1];
            uint32_t ecx = regs[2];

            uint32_t level_type = (ecx >> 8) & 0xff;

            switch (level_type) {
                case 1:
                    smt_multiplier = (ebx & 0xff);
                    break;
                case 2:
                    logical = (ebx & 0xff);
                    break;
            }
        }

        physical = logical / smt_multiplier;

    } else if (strncmp((char*)vendor,"AuthenticAMD",12) == 0) {
        {
            __cpuid_intrinsic(regs, 1, 0);
            uint32_t ebx = regs[1];
            uint32_t edx = regs[3];

            hyper_threading = (edx >> 28) & 0x1;

            if (hyper_threading) {
                logical = (ebx >> 16) & 0xff;
            }
        }

        {
            __cpuid_intrinsic(regs, 8, 0);
            uint32_t ecx = regs[2];

            physical = 1 + (ecx & 0xff);

            if (!hyper_threading) {
                logical = physical;
            }
        }
    } else {
        csc_warn_message("csc_sysinfo_cpuinfo does not work on %*.*s.\n",12,12,(char*)vendor);
        return 1;
    }

    if ( lcpu != NULL ) *lcpu = (int) logical;
    if ( pcpu != NULL ) *pcpu = (int) physical;
    if ( ht !=NULL) *ht = hyper_threading;

    return 0;
}

#else

int csc_sysinfo_cpuinfo(int *lcpu, int *pcpu, int *ht){
    (void)lcpu;
    (void)pcpu;
    (void)ht;
    return 1;
}

#endif
