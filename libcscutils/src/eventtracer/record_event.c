/*
 * LIBCSCUTILS - Event Tracer
 * Copyright (C) Martin Koehler, 2018
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
#include <pthread.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#include "cscutils/eventtracer.h"
#include "cscutils/counter.h"

#define CHUNK 1024

size_t csc_event_tracer_event_begin(const char *type, const char *name)
{
    size_t return_id;

    CSC_EVENT_TRACER_LOCK;
    if ( csc_event_tracer.current_id >= csc_event_tracer.len) {
        csc_event_tracer.len += CHUNK;
        csc_event_tracer.events = realloc(csc_event_tracer.events, sizeof(csc_event_tracer_evt_t) * csc_event_tracer.len);
        if ( csc_event_tracer.events == NULL) {
            fprintf(stderr, "Failed to allocate memory for the event trace. Abort the program.\n");
            abort();
        }
    }
    return_id = csc_event_tracer.current_id;
    csc_event_tracer.current_id++;
    csc_event_tracer.events[return_id].id = return_id;
#ifdef _OPENMP
    csc_event_tracer.events[return_id].tid = omp_get_thread_num();
#else
    csc_event_tracer.events[return_id].tid = 0;
#endif
    csc_event_tracer.events[return_id].ts = csc_wtime() - csc_event_tracer.toffset;
    csc_event_tracer.events[return_id].te = csc_event_tracer.events[return_id].ts - csc_event_tracer.toffset;

    snprintf(csc_event_tracer.events[return_id].event_type, CSC_EVENT_BUFFER_LEN, "%s", type);
    snprintf(csc_event_tracer.events[return_id].event_name, CSC_EVENT_BUFFER_LEN, "%s", name);
    CSC_EVENT_TRACER_UNLOCK;
    return return_id;
}

void csc_event_tracer_event_end(size_t id)
{
    CSC_EVENT_TRACER_LOCK;
    if ( id > csc_event_tracer.current_id) {
        CSC_EVENT_TRACER_UNLOCK;
        return;
    }
    csc_event_tracer.events[id].te = csc_wtime()- csc_event_tracer.toffset;
    CSC_EVENT_TRACER_UNLOCK;
    return;
}

