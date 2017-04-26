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
#include <stdio.h>
#include "cscutils/hardware.h"

int main(int argc, char **argv)
{
    size_t total_ram, total_swap, free_ram, free_swap; 

    csc_hardware_memory(&total_ram, &free_ram, &total_swap, &free_swap); 
    printf("TOTAL RAM: %lu\n", total_ram); 
    printf("FREE RAM:  %lu\n", free_ram);
    printf("TOTAL SWAP: %lu\n", total_swap);
    printf("FREE SWAP:  %lu\n", free_swap);
    return 0;
}


