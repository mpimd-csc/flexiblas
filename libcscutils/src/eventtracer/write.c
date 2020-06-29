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
#include <errno.h>

#include "cscutils/eventtracer.h"

void csc_event_tracer_write(const char *filename)
{
    FILE *fp;
    size_t i;
    CSC_EVENT_TRACER_LOCK;

    fp = fopen (filename, "w");
    if (!fp) {
        int err = errno;
        fprintf(stderr, "Failed to open %s for writing ( error %d = %s ). \n", filename, err, strerror(err));
        fflush(stderr);
        CSC_EVENT_TRACER_UNLOCK;
        return;
    }

    for (i = 0; i < csc_event_tracer.current_id; i++) {
        csc_event_tracer_evt_t * evt = &(csc_event_tracer.events[i]);
        fprintf(fp, "%lu,%lu,%32.16g,%32.16g,%s,%s\n", (unsigned long) evt->id, (unsigned long) evt->tid, evt->ts, evt->te, evt->event_type, evt->event_name);
    }
    fclose(fp);
    CSC_EVENT_TRACER_UNLOCK;
    return;

}

