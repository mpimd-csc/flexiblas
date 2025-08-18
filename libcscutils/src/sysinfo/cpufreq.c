/*
 * LIBCSCUTILS -- Helper for CSC developed software
 * Copyright (C) Martin Koehler, 2017
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
#include <math.h>
#include <string.h>
#include "cscutils/sysinfo.h"
#include "cscutils/strutils.h"
#include "cscutils/error_message.h"

#ifdef CSC_HAVE_CPUFREQ
#include <cpufreq.h>
int csc_cpufreq_enabled(void)
{
    return 1;
}

unsigned long csc_cpufreq_freq(unsigned int cpu) {
    unsigned long hf;

    hf = cpufreq_get_freq_hardware(cpu);
    if (!hf) {
        return cpufreq_get_freq_kernel(cpu);
    } else {
        return hf;
    }
}

int csc_cpufreq_set(unsigned int cpu, unsigned long freq)
{
    int ret =  cpufreq_set_frequency(cpu, freq );
    if ( ret  ) {
        csc_error_message("Failed to set the frequency of CPU %u to %lukHz (code %d)\n", cpu, freq, ret );
        return -1;
    }
    return 0;
}

unsigned long *csc_cpufreq_available_freq(unsigned int cpu)
{
    struct cpufreq_available_frequencies *first = NULL;
    struct cpufreq_available_frequencies *current = NULL;
    unsigned long *freq;
    size_t count = 0;
    if (!(first = cpufreq_get_available_frequencies(cpu))) {
        csc_error_message("Failed to get available CPU frequencies.\n");
        return NULL;
    }

    current = first;
    count = 0;
    while ( current ) {
        count ++;
        current=current->next;
    }

    freq = (unsigned long *) malloc(sizeof(unsigned long) * (count+1));
    if (!freq) {
        csc_error_message("Failed to allocate the results.\n");
        cpufreq_put_available_frequencies(first);
        return NULL;
    }

    count = 0;
    current = first;
    while (current) {
        freq[count] = current->frequency;
        current = current->next;
        count++;
    }
    freq[count] = 0;
    cpufreq_put_available_frequencies(first);
    return freq;
}

int csc_cpufreq_governor_available(unsigned int cpu, const char *governor)
{
    struct cpufreq_available_governors *first, *current;

    first = cpufreq_get_available_governors(cpu);
    if (!first) {
        return 0;
    }

    current = first;
    while (current) {
        if ( csc_strcasecmp(governor, current->governor) == 0 ) {
            cpufreq_put_available_governors(first);
            return -1;
        }
        current = current -> next;
    }
    cpufreq_put_available_governors(first);
    return 0;
}

char * csc_cpufreq_available_governors(char*buffer, size_t maxlen, unsigned int cpu, void **state)
{
    struct cpufreq_available_governors * first;

    if ( *state == NULL ) {
        first = cpufreq_get_available_governors(cpu);
        if (!first) return NULL;
        strncpy(buffer, first->governor, maxlen);
        *state = (void*) first;
        return buffer;
    } else {
        first = (struct cpufreq_available_governors *) *state;
        if ( first -> next == NULL ) {
            cpufreq_put_available_governors(first->first);
            state = NULL;
            return NULL;
        }
        first = first->next;
        strncpy(buffer, first->governor, maxlen);
        *state = (void*) first;
        return buffer;
    }
}

int csc_cpufreq_set_governor(unsigned int cpu, char * governor)
{
    if (csc_cpufreq_governor_available(cpu, governor)) {
        return cpufreq_modify_policy_governor(cpu, governor);
    } else {
        csc_error_message("Governor %s on CPU %u not available.", governor, cpu);
        return -1;
    }

}

int csc_cpufreq_check_governor(unsigned int cpu, char *governor) {
    struct cpufreq_policy *policy;

    policy = cpufreq_get_policy(cpu);
    if ( !policy ) return 0;
    if ( strcmp(policy->governor, governor) == 0 ) {
        cpufreq_put_policy(policy);
        return 1;
    }
    cpufreq_put_policy(policy);
    return 0;
}

#else
int csc_cpufreq_enabled(void) { return 0; }
unsigned long csc_cpufreq_freq(unsigned int cpu) { (void) cpu; return 0;}
int csc_cpufreq_set(unsigned int cpu, unsigned long freq) { (void) cpu; (void) freq; return 0; }
unsigned long *csc_cpufreq_available_freq(unsigned int cpu) { (void) cpu; return NULL; }
int csc_cpufreq_governor_available(unsigned int cpu, const char *governor) { (void) cpu; (void) governor;  return 0; }
char * csc_cpufreq_available_governors(char*buffer, size_t maxlen, unsigned int cpu, void **state){ (void) buffer; (void) maxlen; (void) cpu; (void) state; return NULL;}
int csc_cpufreq_set_governor(unsigned int cpu, char * governor) { (void) cpu; (void) governor; return 0; }
int csc_cpufreq_check_governor(unsigned int cpu, char *governor) { (void) cpu; (void) governor; return 0; }
#endif


