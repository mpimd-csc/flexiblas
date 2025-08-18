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

#ifndef CSC_HARDWARE_H
#define CSC_HARDWARE_H
#include "cscutils/cscutils_config.h"
#include <stdint.h>

#ifdef  __cplusplus
extern "C" {
#endif

    /**
     @file include/cscutils/sysinfo.h
      @defgroup sysinfo  System Information: Hardware and Operating System Related Information

      This part of the library contains routines to retrieve information about the hardware
      and the used operating system like the size of the memory and similar stuff.

      @remark The module depends on the \ref error_message module.

      @addtogroup sysinfo
      @{
    */

#define CSC_CPUFREQ_CONSERVATIVE "conservative"
#define CSC_CPUFREQ_USERSPACE "userspace"
#define CSC_CPUFREQ_POWERSACE "powersave"
#define CSC_CPUFREQ_SCHEDUTIL "schedutil"
#define CSC_CPUFREQ_ONDEMAND "ondemand"
#define CSC_CPUFREQ_PERFORMANCE "performance"

    /**
     * @brief Retrieve information about the installed memory.
     * @param[out]  total_ram   Returns the total (physical) size of the memory.
     * @param[out]  free_ram    Returns the size of the free memory.
     * @param[out]  total_swap  Returns the size of the swap space.
     * @param[out]  free_swap   Returns the size of the free swap space.
     * @return zero on success or a non zero error code otherwise
     *
     * The csc_sysinfo_memory function retrieve information about the installed
     * memory and the swap space. The sizes are returned in bytes. If a value
     * is not wanted, set it to zero during the function call.
     */
    int csc_sysinfo_memory(size_t *total_ram, size_t *free_ram, size_t *total_swap, size_t *free_swap);

    /**
     * @brief Return the current hostname.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_hostname function returns the current hostname
     * as malloced string. The result needs to be freed afterwards.
     *
     */
    char *csc_sysinfo_hostname(void);

    /**
     * @brief Return the system name.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_sysname function returns the system name
     * as reported by \b uname as malloced string. The result needs to be freed afterwards.
     *
     */
    char *csc_sysinfo_sysname(void);


    /**
     * @brief Return the node name.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_nodename function returns the node name
     * as reported by \b uname as malloced string. The result needs to be freed afterwards.
     *
     * @sa csc_sysinfo_hostname
     */
    char *csc_sysinfo_nodename(void);


     /**
     * @brief Return the kernel release.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_release function returns the kernel release
     * as reported by \b uname as malloced string. The result needs to be freed afterwards.
     *
     */
    char *csc_sysinfo_release(void);

    /**
     * @brief Return the kernel version.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_version function returns the kernel version
     * as reported by \b uname as malloced string. The result needs to be freed afterwards.
     *
     */
    char *csc_sysinfo_version(void);

    /**
     * @brief Return the machine architecture.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_machine  function returns the machine's architecture
     * as reported by \b uname as malloced string. The result needs to be freed afterwards.
     *
     */
    char *csc_sysinfo_machine(void);

    /**
     * @brief Return the CPU's name.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_cpuname  function returns the CPU's name as reported by
     * the kernel in /proc/cpuinfo. The string needs to be freed afterwards.
     */
    char *csc_sysinfo_cpuname(void);

    /**
     * @brief Return the compiler name and version as human-readable string.
     * @return A malloced string or NULL on failure.
     *
     * The csc_sysinfo_ccompiler function returns the name and the version of the
     * use C compiler as human-readable string. The string needs to be freed afterwards.
     */
    char *csc_sysinfo_ccompiler(void);

    /**
     * @brief Return the number of CPU cores
     * @return The number of available CPU cores.
     *
     * The csc_sysinfo_cpu_count function returns the number of
     * available CPU cores. In case of an error one is returned.
     *
     */
    unsigned int csc_sysinfo_cpu_count(void);


    /**
     * @brief Check if cpufreq is available.
     * @return one if cpufreq is available, zero otherwise
     *
     * The csc_cpufreq_enabled function checks if the cpufreq support is available
     * and enabled.
     *
     */
    int csc_cpufreq_enabled(void);

    /**
     * @brief Set the frequency of a CPU core.
     * @param[in]   cpu     The number of the core to set.
     * @param[in]   freq    Frequency to set.
     * @return zero on success, non-zero otherwise.
     *
     * The csc_cpufreq_set function set the frequency of a CPU core. The
     * frequency is given in kilo-Hertz.
     *
     */
    int csc_cpufreq_set(unsigned int cpu, unsigned long freq);

    /**
     * @brief Return the current frequency of a CPU core.
     * @param[in] cpu   The number of the CPU core to query.
     * @return The frequency of the CPU core in kilo-Hertz, zero in case of an error.
     *
     * The csc_cpufreq_freq function returns the current clock frequency of the given
     * CPU core.
     *
     */
    unsigned long csc_cpufreq_freq(unsigned int cpu);

    /**
     * @brief Gather the available frequencies for a CPU core.
     * @param[in] cpu   The CPU core to gather.
     * @return An array of the available frequencies.
     *
     * The csc_cpufreq_available_freq function returns an array of available
     * frequencies for a CPU core. The array is allocated using malloc and needs
     * to be freed afterwards. The last entry in the array is always zero. The
     * frequencies are given in kilo-Hertz.
     */
    unsigned long *csc_cpufreq_available_freq(unsigned int cpu);

    /**
     * @brief Check if a cpufreq governor is available.
     * @param[in]   cpu         CPU core to use
     * @param[in]   governor    Name of the governor.
     * @return one if the governor is available, zero otherwise.
     *
     * The csc_cpufreq_governor_available function checks if a cpufreq governor
     * is available for a desired CPU core.
     *
     */
    int csc_cpufreq_governor_available(unsigned int cpu, const char *governor);

    /**
     * @brief Return an iterator over all available cpufreq governors.
     * @param[out]  buffer      Buffer containing the current governors.
     * @param[in]   maxlen      Maximum length of the string to be written to buffer.
     * @param[in]   cpu         The number of the CPU core to use
     * @param[inout] state      State of the iteration.
     * @return The pointer to buffer containing the name of the governor. NULL if nothing is available.
     *
     * The csc_cpufreq_available_governors functions iterates over the cpufreq governors available on
     * a given CPU core. The function returns the name of the next governor. Thereby the name of the
     * governors is allowed to be at most maxlen-1 bytes long. At the beginning state needs to be intializes
     * as NULL.
     *
     * \b Example:
     * \code
     *   void * state = NULL:
     *   char buffer[256];
     *   while ( csc_cpufreq_available_governors(buffer, 256, 0, &state) != NULL) {
     *      printf("Governor: %s available.\n", buffer);
     *   }
     * \endcode
     *
     * @attention The function is not thread-safe / reentrant.
     *
     */
    char * csc_cpufreq_available_governors(char*buffer, size_t maxlen, unsigned int cpu, void **state);

    /**
     * @brief Set a cpufreq governor on a CPU core.
     * @param[in]   cpu         CPU core to use
     * @param[in]   governor    Name of the governor to set.
     * @return zero on sucess, non-zero otherwise
     *
     * The csc_cpufreq_set_governor function set the cpufreq governor to
     * be used by a CPU core.
     *
     */
    int csc_cpufreq_set_governor(unsigned int cpu, char * governor);


    /**
     * @brief Check if a cpufreq governor is set for a CPU core.
     * @param[in]   cpu         The number of the CPU to check.
     * @param[in]   governor    The name of the governor to be checked.
     * @return one if the governor is set for the CPU core, zero otherwise.
     *
     * The csc_cpufreq_check_governor function checks if a CPU freq governor is
     * set of a given CPU core.
     *
     */
    int csc_cpufreq_check_governor(unsigned int cpu, char *governor);

    /**
     * @brief Get information from CPU.
     * @param[in,out] lcpu number of logical CPUs
     * @param[in,out] pcpu number of physical CPUs
     * @param[in,out] ht  returns true if HyperThreading works
     * @return zero if output values were written
     *
     * The csc_sysinfo_cpuinfo function gets information from CPU, that means
     * <ul>
     * <li> number of logical CPUs \f$ lcpu \f$,
     * <li> number of physical CPUs \f$ pcpu \f$,
     * <li> information if HyperThreading works.
     * </ul>
     * Note that these are CPU properties, that is, they are invariant
     * under software and platform hardware configuration.
     */

    int csc_sysinfo_cpuinfo(int *lcpu, int *pcpu, int *ht);


    /**
     * @brief Get the CMake command line from compile time.
     * @return String containing the CMake command line.
     *
     * The csc_sysinfo_cmake_args function returns a static string containing
     * the CMake command line parameters from compile-time. This is only possible
     * on Linux due to restrictions in CMake. On all other platforms, NULL is returned.
     */
    const char *csc_sysinfo_cmake_args(void);

    /**
     * @brief Return the C flags used in compilation.
     * @return String containing the used C flags.
     *
     * The csc_sysinfo_c_flags function returns a static string containing
     * the C flags used during compilation. If C is not supported or enabled
     * NULL is returned.
     */
    const char *csc_sysinfo_c_flags(void);

    /**
     * @brief Return the C++ flags used in compilation.
     * @return String containing the used C++ flags.
     *
     * The csc_sysinfo_cxx_flags function returns a static string containing
     * the C++ flags used during compilation. If C++ is not supported or enabled
     * NULL is returned.
     */
    const char *csc_sysinfo_cxx_flags(void);

    /**
     * @brief Return the Fortran flags used in compilation.
     * @return String containing the used Fortran flags.
     *
     * The csc_sysinfo_fortran_flags function returns a static string containing
     * the Fortran flags used during compilation. If Fortran is not supported or enabled
     * NULL is returned.
     */
    const char *csc_sysinfo_fortran_flags(void);

    /**
     * @brief Return the HIP flags used in compilation.
     * @return String containing the used HIP flags.
     *
     * The csc_sysinfo_hip_flags function returns a static string containing
     * the HIP flags used during compilation. If HIP is not supported or enabled
     * NULL is returned.
     */
    const char *csc_sysinfo_hip_flags(void);

    /**
     * @brief Return the CUDA flags used in compilation.
     * @return String containing the used CUDA flags.
     *
     * The csc_sysinfo_cuda_flags function returns a static string containing
     * the CUDA flags used during compilation. If CUDA is not supported or enabled
     * NULL is returned.
     */
    const char *csc_sysinfo_cuda_flags(void);


#if defined(__x86_64__) || defined(_M_X64) || defined(__i386) || defined(_M_IX86)
    enum
    {
    	CSC_CPU_VENDOR_INTEL = 0,
    	CSC_CPU_VENDOR_AMD,
    	CSC_CPU_VENDOR_UNKNOWN
    };
    enum
    {
    	CSC_CPU_FEATURE_SSE3     = 0x0001,
    	CSC_CPU_FEATURE_SSSE3    = 0x0002,
    	CSC_CPU_FEATURE_SSE41    = 0x0004,
    	CSC_CPU_FEATURE_SSE42    = 0x0008,
    	CSC_CPU_FEATURE_AVX      = 0x0010,
    	CSC_CPU_FEATURE_AVX2     = 0x0020,
    	CSC_CPU_FEATURE_FMA3     = 0x0040,
    	CSC_CPU_FEATURE_FMA4     = 0x0080,
    	CSC_CPU_FEATURE_AVX512F  = 0x0100,
    	CSC_CPU_FEATURE_AVX512DQ = 0x0200,
    	CSC_CPU_FEATURE_AVX512PF = 0x0400,
    	CSC_CPU_FEATURE_AVX512ER = 0x0800,
    	CSC_CPU_FEATURE_AVX512CD = 0x1000,
    	CSC_CPU_FEATURE_AVX512BW = 0x2000,
    	CSC_CPU_FEATURE_AVX512VL = 0x4000,
        CSC_CPU_FEATURE_AVX512VNNI = 0x8000,
        CSC_CPU_FEATURE_AVX512BF16 = 0x10000
    };
#endif

    // Intel CPUs
    int csc_sysinfo_cpuid_is_skx( void );
    int csc_sysinfo_cpuid_is_knl( void );
    int csc_sysinfo_cpuid_is_haswell( void );
    int csc_sysinfo_cpuid_is_sandybridge( void );
    int csc_sysinfo_cpuid_is_penryn( void );

    // AMD
    int csc_sysinfo_cpuid_is_zen4( void );
    int csc_sysinfo_cpuid_is_zen3( void );
    int csc_sysinfo_cpuid_is_zen2( void );
    int csc_sysinfo_cpuid_is_zen( void );
    int csc_sysinfo_cpuid_is_excavator( void );
    int csc_sysinfo_cpuid_is_steamroller( void );
    int csc_sysinfo_cpuid_is_piledriver( void );
    int csc_sysinfo_cpuid_is_bulldozer( void );


    /** @} */
#ifdef  __cplusplus
}
#endif
#endif /* end of include guard: ERROR_MESSAGE_H */
