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

#if 0
void cpuID(uint32_t op, uint32_t reg[4])
{

#ifdef _WIN32
  __cpuid((int *)regs, (int)i);

#else
			     asm volatile("pushl %%ebx      \n\t" /*  save %ebx */
			 	  "cpuid            \n\t"
				  "movl %%ebx, %1   \n\t" /*  save what cpuid just put in %ebx */
				  "popl %%ebx       \n\t" /*  restore the old %ebx */
				  : "=a"(reg[0]), "=r"(reg[1]), "=c"(reg[2]), "=d"(reg[3])
				  : "a"(op)
				  : "cc");

#endif
}
#else

/* static inline void cpuid2(int op, uint32_t *eax, uint32_t *ebx, uint32_t *ecx, uint32_t *edx){
	__asm__ __volatile__
      ("cpuid": "=a" (*eax), "=b" (*ebx), "=c" (*ecx), "=d" (*edx) : "a" (op) : "cc");
      
      }
static inline void cpuID(int op, uint32_t reg[4]){
    cpuid2(op, &(reg[0]), &(reg[1]), &(reg[2]), &(reg[3])); 
} */
static inline void cpuID(int op, uint32_t reg[4]){
	reg[0]=0; 
	reg[1]=0; 
	reg[2]=0; 
	reg[3]=0; 
}
#endif

/** 
 * @brief Get information from CPU. 
 * @param[in,out] lcpu number of logical CPUs
 * @param[in,out] pcpu number of physical CPUs
 * @param[in,out] ht  returns true if HyperThreading works
 * @return always zero  
 *
 * The mess_cpuinfo function gets information from CPU, that means 
 * <ul>
 * <li> number of logical CPUs \f$ lcpu \f$,
 * <li> number of physical CPUs \f$ pcpu \f$,
 * <li> information if HyperThreading works.
 * </ul>
 */

int mess_cpuinfo(int *lcpu, int *pcpu, int *ht){
	uint32_t regs[4]; 
	// Get vendor
	char vendor[12];
	cpuID(0, regs);
	((uint32_t *)vendor)[0] = regs[1]; // EBX
	((uint32_t *)vendor)[1] = regs[3]; // EDX
	((uint32_t *)vendor)[2] = regs[2]; // ECX
	
	// Get CPU features
	cpuID(1, regs);
	uint32_t cpuFeatures = regs[3]; // EDX
	// Detect hyper-threads  
	int hyperThreads = 0;
	uint32_t logical = 1; 
	uint32_t cores = 1; 

	if ( strncmp(vendor,"GenuineIntel",12)==0  && cpuFeatures & (1 << 28)) { // HTT bit
		// Logical core count per CPU
		cpuID(1, regs);
		logical = (regs[1] >> 16) & 0xff; // EBX[23:16]
		cores = logical;
	} else 	if ( strncmp ( vendor, "GenuineIntel", 12) == 0) {
			cpuID(1, regs);
			logical = (regs[1] >> 16) & 0xff; // EBX[23:16]
			cpuID(4, regs);
			cores = ((regs[0] >> 26) & 0x3f) + 1; // EAX[31:26] + 1
	} 
	if (strncmp(vendor,"AuthenticAMD",12)==0) {
		// Get NC: Number of CPU cores - 1
		cpuID(0x80000008, regs);
		logical =  cores = ((unsigned)(regs[2] & 0xff)) + 1; // ECX[7:0] + 1
	}
	if (cores < logical) hyperThreads = 1;
	
	logical = 24; 
	cores = 12; 
	hyperThreads = 1; 
	if ( lcpu != NULL ) *lcpu = (int) logical; 
	if ( pcpu != NULL ) *pcpu = (int) cores; 
	if ( ht !=NULL) *ht = hyperThreads; 

	return 0; 
}

