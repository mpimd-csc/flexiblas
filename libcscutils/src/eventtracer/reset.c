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
 * @file reset.c
 * @brief Event tracer reset functionality
 *
 * This file contains the implementation of the csc_event_tracer_reset() function
 * which initializes or clears the event tracer state.
 */

#include <math.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cscutils/counter.h"
#include "cscutils/eventtracer.h"

csc_event_tracer_t csc_event_tracer = {.mutex = PTHREAD_MUTEX_INITIALIZER, .toffset = 0.0, .current_id = 0, .len = 0, .events = NULL};

/**
 * @brief Global event tracer instance
 *
 * This is the main event tracer instance that is used throughout the library.
 * It is initialized with default values and can be reset using csc_event_tracer_reset().
 */

/**
 * @brief Reset the event tracer to its initial state
 *
 * This function resets the event tracer by:
 * 1. Setting current_id and len to 0
 * 2. Freeing any allocated events array
 * 3. Setting the time offset to the current time
 *
 * The function is thread-safe and uses the tracer's mutex for synchronization.
 */
void csc_event_tracer_reset()
{
    CSC_EVENT_TRACER_LOCK;
    csc_event_tracer.current_id = 0;
    csc_event_tracer.len = 0;
    if (csc_event_tracer.events != NULL)
    {
        free(csc_event_tracer.events);
    }
    csc_event_tracer.events = NULL;
    csc_event_tracer.toffset = csc_wtime();
    CSC_EVENT_TRACER_UNLOCK;
    return;
}
