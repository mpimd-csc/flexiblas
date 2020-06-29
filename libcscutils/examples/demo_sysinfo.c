/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) Martin Koehler, 2015
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

    char *comp = csc_sysinfo_ccompiler();
    printf("Compiler: %s\n", comp);
    free(comp);
    return 0;
}


