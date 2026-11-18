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

/**
 * @file record_event.c
 * @brief Event recording functionality
 *
 * This file contains the implementation of functions for recording timing events:
 * csc_event_tracer_event_begin() and csc_event_tracer_event_end().
 */

#include <math.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _OPENMP
#include <omp.h>
#endif

#include "cscutils/counter.h"
#include "cscutils/eventtracer.h"

#define CHUNK 1024

/**
 * @brief Mark the beginning of a timing event
 *
 * This function records the start of a timing event. It allocates space in the
 * events array if needed, records the current time as the start time, and stores
 * the event type and name. The function returns an event ID that should be used
 * with csc_event_tracer_event_end() to mark the end of the event.
 *
 * The function is thread-safe and uses the tracer's mutex for synchronization.
 * If memory allocation fails, the program is aborted.
 *
 * @param type The type or category of the event (e.g., "computation", "io")
 * @param name The name or description of the event
 * @return The event ID that should be used to end the event
 */
size_t csc_event_tracer_event_begin(const char* type, const char* name)
{
    size_t return_id;

    CSC_EVENT_TRACER_LOCK;
    if (csc_event_tracer.current_id >= csc_event_tracer.len)
    {
        csc_event_tracer.len += CHUNK;
        csc_event_tracer.events = realloc(csc_event_tracer.events, sizeof(csc_event_tracer_evt_t) * csc_event_tracer.len);
        if (csc_event_tracer.events == NULL)
        {
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

/**
 * @brief Mark the end of a timing event
 *
 * This function records the end of a timing event that was started with
 * csc_event_tracer_event_begin(). It updates the end time of the event
 * with the specified ID.
 *
 * The function is thread-safe and uses the tracer's mutex for synchronization.
 * If the provided ID is invalid (greater than current_id), the function returns
 * without making any changes.
 *
 * @param id The event ID returned by csc_event_tracer_event_begin()
 */
void csc_event_tracer_event_end(size_t id)
{
    CSC_EVENT_TRACER_LOCK;
    if (id > csc_event_tracer.current_id)
    {
        CSC_EVENT_TRACER_UNLOCK;
        return;
    }
    csc_event_tracer.events[id].te = csc_wtime() - csc_event_tracer.toffset;
    CSC_EVENT_TRACER_UNLOCK;
    return;
}
