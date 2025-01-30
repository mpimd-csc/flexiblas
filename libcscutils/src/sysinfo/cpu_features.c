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
#include "cscutils/sysinfo.h"

int csc_sysinfo_cpuid_has_features( uint32_t have, uint32_t want )
{
    return ( have & want ) == want;
}


#if defined(__x86_64__) || defined(_M_X64) || defined(__i386) || defined(_M_IX86)

#include <cpuid.h>

enum
{
                                      // input register(s)     output register
	FEATURE_MASK_SSE3     = (1u<< 0), // cpuid[eax=1]         :ecx[0]
	FEATURE_MASK_SSSE3    = (1u<< 9), // cpuid[eax=1]         :ecx[9]
	FEATURE_MASK_SSE41    = (1u<<19), // cpuid[eax=1]         :ecx[19]
	FEATURE_MASK_SSE42    = (1u<<20), // cpuid[eax=1]         :ecx[20]
	FEATURE_MASK_AVX      = (1u<<28), // cpuid[eax=1]         :ecx[28]
	FEATURE_MASK_AVX2     = (1u<< 5), // cpuid[eax=7,ecx=0]   :ebx[5]
	FEATURE_MASK_FMA3     = (1u<<12), // cpuid[eax=1]         :ecx[12]
	FEATURE_MASK_FMA4     = (1u<<16), // cpuid[eax=0x80000001]:ecx[16]
	FEATURE_MASK_AVX512F  = (1u<<16), // cpuid[eax=7,ecx=0]   :ebx[16]
	FEATURE_MASK_AVX512DQ = (1u<<17), // cpuid[eax=7,ecx=0]   :ebx[17]
	FEATURE_MASK_AVX512PF = (1u<<26), // cpuid[eax=7,ecx=0]   :ebx[26]
	FEATURE_MASK_AVX512ER = (1u<<27), // cpuid[eax=7,ecx=0]   :ebx[27]
	FEATURE_MASK_AVX512CD = (1u<<28), // cpuid[eax=7,ecx=0]   :ebx[28]
	FEATURE_MASK_AVX512BW = (1u<<30), // cpuid[eax=7,ecx=0]   :ebx[30]
	FEATURE_MASK_AVX512VL = (1u<<31), // cpuid[eax=7,ecx=0]   :ebx[31]
  	FEATURE_MASK_AVX512VNNI = (1u<<11), // cpuid[eax=7,ecx=0]   :ecx[11]
	FEATURE_MASK_AVX512BF16 = (1u<< 5), // cpuid[eax=7,ecx=1]   :eax[5]
	FEATURE_MASK_XGETBV   = (1u<<26)|
                            (1u<<27), // cpuid[eax=1]         :ecx[27:26]
	XGETBV_MASK_XMM       = 0x02u,    // xcr0[1]
	XGETBV_MASK_YMM       = 0x04u,    // xcr0[2]
	XGETBV_MASK_ZMM       = 0xe0u     // xcr0[7:5]
};


uint32_t csc_sysinfo_cpuid_query
     (
       uint32_t* family,
       uint32_t* model,
       uint32_t* features
     )
{
	uint32_t eax, ebx, ecx, edx;

	uint32_t old_model  = 0;
	uint32_t old_family = 0;
	uint32_t ext_model  = 0;
	uint32_t ext_family = 0;

	*family   = 0;
	*model    = 0;
	*features = 0;

	uint32_t cpuid_max     = __get_cpuid_max( 0,           0 );
	uint32_t cpuid_max_ext = __get_cpuid_max( 0x80000000u, 0 );

	if ( cpuid_max < 1 ) return CSC_CPU_VENDOR_UNKNOWN;

	uint32_t vendor_string[4] = { 0, 0, 0, 0 };

	// This is actually a macro that modifies the last four operands,
	// hence why they are not passed by address.
	__cpuid( 0, eax, vendor_string[0],
	                 vendor_string[2],
	                 vendor_string[1] );

	// Check extended feature bits for post-AVX2 features.
	if ( cpuid_max >= 7 )
	{
		// This is actually a macro that modifies the last four operands,
		// hence why they are not passed by address.
		__cpuid_count( 7, 0, eax, ebx, ecx, edx );

		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX2     ) )   *features |= CSC_CPU_FEATURE_AVX2;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512F  ) )   *features |= CSC_CPU_FEATURE_AVX512F;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512DQ ) )   *features |= CSC_CPU_FEATURE_AVX512DQ;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512PF ) )   *features |= CSC_CPU_FEATURE_AVX512PF;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512ER ) )   *features |= CSC_CPU_FEATURE_AVX512ER;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512CD ) )   *features |= CSC_CPU_FEATURE_AVX512CD;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512BW ) )   *features |= CSC_CPU_FEATURE_AVX512BW;
		if ( csc_sysinfo_cpuid_has_features( ebx, FEATURE_MASK_AVX512VL ) )   *features |= CSC_CPU_FEATURE_AVX512VL;
        if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_AVX512VNNI ) ) *features |= CSC_CPU_FEATURE_AVX512VNNI;

        /* Check BFloat16 Support. */
        __cpuid_count( 7, 1, eax, ebx, ecx, edx );
		if ( csc_sysinfo_cpuid_has_features( eax, FEATURE_MASK_AVX512BF16 ) ) *features |= CSC_CPU_FEATURE_AVX512BF16;

	}

	// Check extended processor info / features bits for AMD-specific features.
	if ( cpuid_max_ext >= 0x80000001u )
	{
		// This is actually a macro that modifies the last four operands,
		// hence why they are not passed by address.
		__cpuid( 0x80000001u, eax, ebx, ecx, edx );

		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_FMA4 ) ) *features |= CSC_CPU_FEATURE_FMA4;
	}

	{
		// This is actually a macro that modifies the last four operands,
		// hence why they are not passed by address.
		__cpuid( 1, eax, ebx, ecx, edx );

		/*
		   cpuid(eax=1): eax[27:0]

			3: 0 - Stepping
			7: 4 - Model
		   11: 8 - Family
		   13:12 - Processor Type
		   19:16 - Extended Model
		   27:20 - Extended Family

		   Intel and AMD have suggested applications to display the family of a
		   CPU as the sum of the "Family" and the "Extended Family" fields shown
		   above, and the model as the sum of the "Model" and the 4-bit
		   left-shifted "Extended Model" fields. If "Family" is different than
		   6 or 15, only the "Family" and "Model" fields should be used while the
		   "Extended Family" and "Extended Model" bits are reserved. If "Family"
		   is set to 15, then "Extended Family" and the 4-bit left-shifted
		   "Extended Model" should be added to the respective base values, and if
		   "Family" is set to 6, then only the 4-bit left-shifted "Extended Model"
		   should be added to "Model".
		*/

		old_model  = ( eax >>  4 ) & ( 0xF  ); // bits 7:4
		old_family = ( eax >>  8 ) & ( 0xF  ); // bits 11:8

		ext_model  = ( eax >> 16 ) & ( 0xF  ); // bits 19:16
		ext_family = ( eax >> 20 ) & ( 0xFF ); // bits 27:20

		// Set the display model and family values based on the original family
		// value. See explanation above.
		if      ( old_family == 6 )
		{
			*model  = ( ext_model << 4 ) + old_model;
			*family =                      old_family;
		}
		else if ( old_family == 15 )
		{
			*model  = ( ext_model << 4 ) + old_model;
			*family = ( ext_family     ) + old_family;
		}
		else
		{
			*model  =                      old_model;
			*family =                      old_family;
		}

		// Check for SSE, AVX, and FMA3 features.
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_SSE3  ) ) *features |= CSC_CPU_FEATURE_SSE3;
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_SSSE3 ) ) *features |= CSC_CPU_FEATURE_SSSE3;
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_SSE41 ) ) *features |= CSC_CPU_FEATURE_SSE41;
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_SSE42 ) ) *features |= CSC_CPU_FEATURE_SSE42;
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_AVX   ) ) *features |= CSC_CPU_FEATURE_AVX;
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_FMA3  ) ) *features |= CSC_CPU_FEATURE_FMA3;

		// Check whether the hardware supports xsave/xrestor/xsetbv/xgetbv AND
		// support for these is enabled by the OS. If so, then we proceed with
		// checking that various register-state saving features are available.
		if ( csc_sysinfo_cpuid_has_features( ecx, FEATURE_MASK_XGETBV ) )
		{
			uint32_t xcr = 0;

			// Call xgetbv to get xcr0 (the extended control register) copied
			// to [edx:eax]. This encodes whether software supports various
			// register state-saving features.
			__asm__ __volatile__
			(
				".byte 0x0F, 0x01, 0xD0"
				: "=a" (eax),
				  "=d" (edx)
				: "c"  (xcr)
				: "cc"
			);

			// The OS can manage the state of 512-bit zmm (AVX-512) registers
			// only if the xcr[7:5] bits are set. If they are not set, then
			// clear all feature bits related to AVX-512.
			if ( !csc_sysinfo_cpuid_has_features( eax, XGETBV_MASK_XMM |
				                               XGETBV_MASK_YMM |
				                               XGETBV_MASK_ZMM ) )
			{
				*features &= ~( CSC_CPU_FEATURE_AVX512F  |
				                CSC_CPU_FEATURE_AVX512DQ |
				                CSC_CPU_FEATURE_AVX512PF |
				                CSC_CPU_FEATURE_AVX512ER |
				                CSC_CPU_FEATURE_AVX512CD |
				                CSC_CPU_FEATURE_AVX512BW |
				                CSC_CPU_FEATURE_AVX512VL );
			}

			// The OS can manage the state of 256-bit ymm (AVX) registers
			// only if the xcr[2] bit is set. If it is not set, then
			// clear all feature bits related to AVX.
			if ( !csc_sysinfo_cpuid_has_features( eax, XGETBV_MASK_XMM |
				                               XGETBV_MASK_YMM ) )
			{
				*features &= ~( CSC_CPU_FEATURE_AVX  |
				                CSC_CPU_FEATURE_AVX2 |
				                CSC_CPU_FEATURE_FMA3 |
				                CSC_CPU_FEATURE_FMA4 );
			}

			// The OS can manage the state of 128-bit xmm (SSE) registers
			// only if the xcr[1] bit is set. If it is not set, then
			// clear all feature bits related to SSE (which means the
			// entire bitfield is clear).
			if ( !csc_sysinfo_cpuid_has_features(eax, XGETBV_MASK_XMM ) )
			{
				*features = 0;
			}
		}
		else
		{
			// If the hardware does not support xsave/xrestor/xsetbv/xgetbv,
			// OR these features are not enabled by the OS, then we clear
			// the bitfield, because it means that not even xmm support is
			// present.

			features = 0;
		}
	}
	// Check the vendor string and return a value to indicate Intel or AMD.
	if      ( strcmp( ( char* )vendor_string, "AuthenticAMD" ) == 0 )
		return CSC_CPU_VENDOR_AMD;
	else if ( strcmp( ( char* )vendor_string, "GenuineIntel" ) == 0 )
		return CSC_CPU_VENDOR_INTEL;
	else
		return CSC_CPU_VENDOR_UNKNOWN;
}

static
void get_cpu_name( char *cpu_name )
{
	uint32_t eax, ebx, ecx, edx;

	__cpuid( 0x80000002u, eax, ebx, ecx, edx );
    //printf("%x %x %x %x\n", eax, ebx, ecx, edx);

	*( uint32_t* )&cpu_name[0 + 0] = eax;
	*( uint32_t* )&cpu_name[0 + 4] = ebx;
	*( uint32_t* )&cpu_name[0 + 8] = ecx;
	*( uint32_t* )&cpu_name[0 +12] = edx;

	__cpuid( 0x80000003u, eax, ebx, ecx, edx );
	//printf("%x %x %x %x\n", eax, ebx, ecx, edx);

	*( uint32_t* )&cpu_name[16+ 0] = eax;
	*( uint32_t* )&cpu_name[16+ 4] = ebx;
	*( uint32_t* )&cpu_name[16+ 8] = ecx;
	*( uint32_t* )&cpu_name[16+12] = edx;

	__cpuid( 0x80000004u, eax, ebx, ecx, edx );
	//printf("%x %x %x %x\n", eax, ebx, ecx, edx);

	*( uint32_t* )&cpu_name[32+ 0] = eax;
	*( uint32_t* )&cpu_name[32+ 4] = ebx;
	*( uint32_t* )&cpu_name[32+ 8] = ecx;
	*( uint32_t* )&cpu_name[32+12] = edx;
}

// Return the number of FMA units _assuming avx512 is supported_.
// This needs updating for new processor types, sigh.
// See https://ark.intel.com/content/www/us/en/ark.html#@Processors
// and also https://github.com/jeffhammond/vpu-count
static int vpu_count( void )
{
	char  cpu_name[48];
	char* loc;
	char  model_num[5];
	int   sku;

    memset(cpu_name, 0, 48);

	get_cpu_name( cpu_name );

	if ( strstr( cpu_name, "Intel(R) Xeon(R)" ) != NULL )
	{
		if (( loc = strstr( cpu_name, "Platinum" ) ))
			return 2;
		if ( loc == NULL )
			loc = strstr( cpu_name, "Gold" ); // 1 or 2, tested below
		if ( loc == NULL )
			if (( loc = strstr( cpu_name, "Silver" ) ))
				return 1;
		if ( loc == NULL )
			if (( loc = strstr( cpu_name, "Bronze" ) ))
				return 1;
		if ( loc == NULL )
			loc = strstr( cpu_name, "W" );
		if ( loc == NULL )
			if (( loc = strstr( cpu_name, "D" ) ))
				// Fixme:  May be wrong
				// <https://github.com/jeffhammond/vpu-count/issues/3#issuecomment-542044651>
				return 1;
		if ( loc == NULL )
			return -1;

		// We may have W-nnnn rather than, say, Gold nnnn
		if ( 'W' == *loc && '-' == *(loc+1) )
			loc++;
		else
			loc = strstr( loc+1, " " );
		if ( loc == NULL )
			return -1;

		strncpy( model_num, loc+1, 4 );
		model_num[4] = '\0'; // Things like i9-10900X matched above

		sku = atoi( model_num );

		// These were derived from ARK listings as of 2019-10-09, but
		// may not be complete, especially as the ARK Skylake listing
		// seems to be limited.
		if      ( 8199 >= sku && sku >= 8100 ) return 2;
		else if ( 6199 >= sku && sku >= 6100 ) return 2;
		else if (                sku == 5122 ) return 2;
		else if ( 6299 >= sku && sku >= 6200 ) return 2; // Cascade Lake Gold
		else if ( 5299 >= sku && sku >= 5200 ) return 1; // Cascade Lake Gold
		else if ( 5199 >= sku && sku >= 5100 ) return 1;
		else if ( 4199 >= sku && sku >= 4100 ) return 1;
		else if ( 3199 >= sku && sku >= 3100 ) return 1;
		else if ( 3299 >= sku && sku >= 3200 ) return 2; // Cascade Lake W
		else if ( 2299 >= sku && sku >= 2200 ) return 2; // Cascade Lake W
		else if ( 2199 >= sku && sku >= 2120 ) return 2;
		else if ( 2102 == sku || sku == 2104 ) return 2; // Gold exceptions
		else if ( 2119 >= sku && sku >= 2100 ) return 1;
		else return -1;
	}
	else if ( strstr( cpu_name, "Intel(R) Core(TM)" ) != NULL )
		return 2; // All i7/i9 with avx512?
	else
	{
		return -1;
	}
}

// Intel CPUs
static int __csc_sysinfo_cpuid_is_skx( uint32_t family, uint32_t model, uint32_t features )
{
    const uint32_t expected = CSC_CPU_FEATURE_AVX      |
	                          CSC_CPU_FEATURE_FMA3     |
	                          CSC_CPU_FEATURE_AVX2     |
	                          CSC_CPU_FEATURE_AVX512F  |
	                          CSC_CPU_FEATURE_AVX512DQ |
	                          CSC_CPU_FEATURE_AVX512BW |
	                          CSC_CPU_FEATURE_AVX512VL ;


	int nvpu = vpu_count();

	if ( csc_sysinfo_cpuid_has_features(features, expected ) )
	{
		switch ( nvpu )
		{
		case 1:
			/* Only one FMA unit, not a full - features Skylake + */ 
            return 0;
		case 2:
			/* 2 FMA units, proper SKX */
            return 1;
		default:
            /* Unkown, assuming Haswell */
			return 0;
		}
	}
	else
		return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_knl( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX     |
	                          CSC_CPU_FEATURE_FMA3    |
	                          CSC_CPU_FEATURE_AVX2    |
	                          CSC_CPU_FEATURE_AVX512F |
	                          CSC_CPU_FEATURE_AVX512PF;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0; 

	return 1;
}


static int __csc_sysinfo_cpuid_is_haswell( uint32_t family, uint32_t model, uint32_t features )
{
    const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_AVX2;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_sandybridge( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX;
	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0; 

	return 1;

}

static int __csc_sysinfo_cpuid_is_penryn( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_SSE3 |
	                          CSC_CPU_FEATURE_SSSE3;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0; 

	return 1;
}

// AMD
static int __csc_sysinfo_cpuid_is_zen4( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_SSE3       |
	                          CSC_CPU_FEATURE_SSSE3      |
	                          CSC_CPU_FEATURE_SSE41      |
	                          CSC_CPU_FEATURE_SSE42      |
	                          CSC_CPU_FEATURE_AVX        |
	                          CSC_CPU_FEATURE_FMA3       |
	                          CSC_CPU_FEATURE_AVX2       |
	                          CSC_CPU_FEATURE_AVX512F    |
	                          CSC_CPU_FEATURE_AVX512DQ   |
	                          CSC_CPU_FEATURE_AVX512CD   |
	                          CSC_CPU_FEATURE_AVX512BW   |
	                          CSC_CPU_FEATURE_AVX512VL   |
	                          CSC_CPU_FEATURE_AVX512VNNI |
	                          CSC_CPU_FEATURE_AVX512BF16;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// zen4 => family == 0x19
	if ( family != 0x19 ) return 0;

	return 0;
}

static int __csc_sysinfo_cpuid_is_zen3( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_AVX2;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0; 

	// zen3 => family == 0x19
	if ( family != 0x19 ) return 0;

	// Finally, check for specific models:
	// Zen 3 maps to couple of different model number ranges
	// we check for all of them.
	const int is_zen3 = (
		( model <= 0x0f ) ||                  // EPYC and ThreadRipper
		( 0x20 <= model && model <= 0x2f ) || // Ryzen 5000 Desktop
		( 0x30 <= model && model <= 0x3f ) || // Trento
		( 0x40 <= model && model <= 0x4f ) || // RMB
		( 0x50 <= model && model <= 0x5f )    // Ryzen 5000 APU
	);

	if ( !is_zen3 ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_zen2( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_AVX2;

	if ( ! csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// zen2 => family == 0x17.
	if ( family != 0x17 ) return 0;

	int is_zen2 = ( 0x30 <= model && model <= 0xff );

	if ( !is_zen2 ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_zen( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_AVX2;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// All Zen cores have a family of 0x17.
	if ( family != 0x17 ) return 0;

	// Finally, check for specific models:
	int is_zen = (model <= 0x30 );

	if ( !is_zen ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_excavator( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_AVX2;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// All Excavator cores have a family of 0x15.
	if ( family != 0x15 ) return 0;

	// Finally, check for specific models:
	// - 0x60-0x7f
	int is_ex = ( 0x60 <= model && model <= 0x7f );

	if ( !is_ex ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_steamroller( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_FMA4;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// All Steamroller cores have a family of 0x15.
	if ( family != 0x15 ) return 0;

	// Finally, check for specific models:
	// - 0x30-0x3f
	const int is_sr =( 0x30 <= model && model <= 0x3f );

	if ( !is_sr ) return 0;

    return 1; 

}

static int __csc_sysinfo_cpuid_is_piledriver( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX  |
	                          CSC_CPU_FEATURE_FMA3 |
	                          CSC_CPU_FEATURE_FMA4;

	if ( ! csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// All Piledriver cores have a family of 0x15.
	if ( family != 0x15 ) return 0;

	// Finally, check for specific models:
	// - 0x02
	// - 0x10-0x1f
	const int is_pd = 	model == 0x02 || ( 0x10 <= model && model <= 0x1f );

	if ( !is_pd ) return 0;

	return 1;
}

static int __csc_sysinfo_cpuid_is_bulldozer( uint32_t family, uint32_t model, uint32_t features )
{
	const uint32_t expected = CSC_CPU_FEATURE_AVX |
	                          CSC_CPU_FEATURE_FMA4;

	if ( !csc_sysinfo_cpuid_has_features( features, expected ) ) return 0;

	// All Bulldozer cores have a family of 0x15.
	if ( family != 0x15 ) return 0;

	// Finally, check for specific models:
	// - 0x00
	// - 0x01
	const int is_bd = ( model == 0x00 || model == 0x01 );

	if ( !is_bd ) return 0;

	return 1;
}

// Intel CPUs
int csc_sysinfo_cpuid_is_skx(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_INTEL)) return 0; 
    return __csc_sysinfo_cpuid_is_skx(family, model, features); 
}

int csc_sysinfo_cpuid_is_knl(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_INTEL)) return 0; 
    return __csc_sysinfo_cpuid_is_knl(family, model, features); 

}

int csc_sysinfo_cpuid_is_haswell(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_INTEL)) return 0; 
    return __csc_sysinfo_cpuid_is_haswell(family, model, features); 
}

int csc_sysinfo_cpuid_is_sandybridge(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_INTEL)) return 0; 
    return __csc_sysinfo_cpuid_is_sandybridge(family, model, features); 
}

int csc_sysinfo_cpuid_is_penryn(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_INTEL)) return 0; 
    return __csc_sysinfo_cpuid_is_penryn(family, model, features); 
}

// AMD
int csc_sysinfo_cpuid_is_zen4(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_zen4(family, model, features); 
}

int csc_sysinfo_cpuid_is_zen3(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_zen3(family, model, features); 
}

int csc_sysinfo_cpuid_is_zen2(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_zen2(family, model, features); 
}

int csc_sysinfo_cpuid_is_zen(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_zen(family, model, features); 
}

int csc_sysinfo_cpuid_is_excavator(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_excavator(family, model, features); 
}

int csc_sysinfo_cpuid_is_steamroller(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_steamroller(family, model, features); 
}

int csc_sysinfo_cpuid_is_piledriver(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_piledriver(family, model, features); 
}

int csc_sysinfo_cpuid_is_bulldozer(  )
{
    uint32_t family; 
    uint32_t model; 
    uint32_t features; 
    uint32_t vendor; 

    vendor = csc_sysinfo_cpuid_query(&family, &model, &features); 

    if (!(vendor == CSC_CPU_VENDOR_AMD)) return 0; 
    return __csc_sysinfo_cpuid_is_bulldozer(family, model, features); 
}



#else

uint32_t csc_sysinfo_cpuid_query
     (
       uint32_t* family,
       uint32_t* model,
       uint32_t* features
     )
{
	*family = 0;
    *model = 0;
    *features = 0;
    return 0;
}

// Intel CPUs
int csc_sysinfo_cpuid_is_skx(  )
{
    return 0;
}

int csc_sysinfo_cpuid_is_knl(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_haswell(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_sandybridge(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_penryn(  )
{
    return 0;
}

// AMD
int csc_sysinfo_cpuid_is_zen4(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_zen3(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_zen2(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_zen(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_excavator(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_steamroller(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_piledriver(  )
{
    return 0;
}
int csc_sysinfo_cpuid_is_bulldozer(  )
{
    return 0;
}




#endif
