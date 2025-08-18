//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
    This file is part of libcscutils, a set of helper function.
    Copyright (C) 2013-2024 Martin Koehler

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 3 of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/



#include <stdio.h>
#include <stdlib.h>
#include "cscutils/sysinfo.h"



int main(int argc, char **argv)
{
    size_t total_ram, total_swap, free_ram, free_swap;
    (void) argc;
    (void) argv;

    printf("Memory Info:\n");
    csc_sysinfo_memory(&total_ram, &free_ram, &total_swap, &free_swap);
    printf("TOTAL RAM: %zu\n", total_ram);
    printf("FREE RAM:  %zu\n", free_ram);
    printf("TOTAL SWAP: %zu\n", total_swap);
    printf("FREE SWAP:  %zu\n", free_swap);

    char *hn = csc_sysinfo_hostname();
    printf("Hostname: %s\n", hn);
    free(hn);

    char *sys = csc_sysinfo_sysname();
    char *node = csc_sysinfo_nodename();
    char *release = csc_sysinfo_release();
    char *version = csc_sysinfo_version();
    char *machine = csc_sysinfo_machine();



    printf("sysname: %s\n", sys);
    printf("nodename: %s\n", node);
    printf("release: %s\n", release);
    printf("version: %s \n",version);
    printf("machine: %s \n",machine);

    free(sys); free(node); free(release); free(version); free(machine);

    char *cpu;
    cpu = csc_sysinfo_cpuname();
    printf("CPU-Name: '%s'\n", cpu);
    free(cpu);

    int physical_cores, logical_cores, hyper_threading;

    if (csc_sysinfo_cpuinfo(&logical_cores,&physical_cores,&hyper_threading)) {
        printf("CPU Cores: Unknown\n");
    } else {
        if (hyper_threading) {
            printf("CPU Cores: %d (%d logical)\n", physical_cores, logical_cores);
        } else {
            printf("CPU Cores: %d\n", physical_cores);
        }
    }

    char *comp = csc_sysinfo_ccompiler();
    printf("Compiler: %s\n", comp);
    free(comp);

    printf("CPU is SkylakeX:       %d\n", csc_sysinfo_cpuid_is_skx());
    printf("CPU is KnightsLanding: %d\n", csc_sysinfo_cpuid_is_knl());
    printf("CPU is Haswell:        %d\n", csc_sysinfo_cpuid_is_haswell());
    printf("CPU is SandyBrigde:    %d\n", csc_sysinfo_cpuid_is_sandybridge());
    printf("CPU is Penryn:         %d\n", csc_sysinfo_cpuid_is_penryn());
    printf("CPU is ZEN4:           %d\n", csc_sysinfo_cpuid_is_zen4());
    printf("CPU is ZEN3:           %d\n", csc_sysinfo_cpuid_is_zen3());
    printf("CPU is ZEN2:           %d\n", csc_sysinfo_cpuid_is_zen2());
    printf("CPU is ZEN:            %d\n", csc_sysinfo_cpuid_is_zen());
    printf("CPU is EXCAVATOR:      %d\n", csc_sysinfo_cpuid_is_excavator());
    printf("CPU is STEAMROLLER:    %d\n", csc_sysinfo_cpuid_is_steamroller());
    printf("CPU is PILEDRIVER:     %d\n", csc_sysinfo_cpuid_is_piledriver());
    printf("CPU is BULLDOZER:      %d\n", csc_sysinfo_cpuid_is_bulldozer());

    return 0;
}
