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

#include "cscutils/eventtracer.h"
#include "cscutils/counter.h"

csc_event_tracer_t csc_event_tracer = {
    .mutex = PTHREAD_MUTEX_INITIALIZER,
    .toffset = 0.0,
    .current_id = 0,
    .len = 0,
    .events = NULL
};

void csc_event_tracer_reset()
{
    CSC_EVENT_TRACER_LOCK;
    csc_event_tracer.current_id = 0;
    csc_event_tracer.len = 0;
    if ( csc_event_tracer.events != NULL){
        free(csc_event_tracer.events);
    }
    csc_event_tracer.events = NULL;
    csc_event_tracer.toffset = csc_wtime();
    CSC_EVENT_TRACER_UNLOCK;
    return;
}

