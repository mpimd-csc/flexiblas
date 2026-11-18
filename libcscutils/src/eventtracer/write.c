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
 * @file write.c
 * @brief Event tracer file writing functionality
 *
 * This file contains the implementation of the csc_event_tracer_write() function
 * which writes the collected timing events to a CSV file for analysis.
 */

#include <errno.h>
#include <math.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscutils/eventtracer.h"

/**
 * @brief Write all recorded events to a CSV file
 *
 * This function writes the collected timing events to the specified file
 * in CSV format. The file format is:
 * id,tid,start_time,end_time,type,name
 *
 * Where:
 * - id: Unique event identifier
 * - tid: Thread identifier
 * - start_time: Event start timestamp (relative to tracer initialization)
 * - end_time: Event end timestamp (relative to tracer initialization)
 * - type: Event type/category
 * - name: Event name/description
 *
 * The function is thread-safe and uses the tracer's mutex for synchronization.
 * If the file cannot be opened, an error message is printed to stderr.
 *
 * @param filename The name of the file to write the events to
 */
void csc_event_tracer_write(const char* filename)
{
    FILE* fp;
    size_t i;
    CSC_EVENT_TRACER_LOCK;

    fp = fopen(filename, "w");
    if (!fp)
    {
        int err = errno;
        fprintf(stderr, "Failed to open %s for writing ( error %d = %s ). \n", filename, err, strerror(err));
        fflush(stderr);
        CSC_EVENT_TRACER_UNLOCK;
        return;
    }

    for (i = 0; i < csc_event_tracer.current_id; i++)
    {
        csc_event_tracer_evt_t* evt = &(csc_event_tracer.events[i]);
        fprintf(fp, "%lu,%lu,%32.16g,%32.16g,%s,%s\n", (unsigned long)evt->id, (unsigned long)evt->tid, evt->ts, evt->te,
                evt->event_type, evt->event_name);
    }
    fclose(fp);
    CSC_EVENT_TRACER_UNLOCK;
    return;
}
