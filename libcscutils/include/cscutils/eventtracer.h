/*
 * Event Tracing Helper for LIBCSCUTILS
 * Copyright (C) Martin Koeherl, 2018
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



#ifndef CSC_EVENT_TRACER

#define CSC_EVENT_TRACER

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#include "cscutils/cscutils_config.h"

#ifdef __cplusplus
extern "C" {
#endif
    #define CSC_EVENT_BUFFER_LEN 128
    #define CSC_EVENT_TRACER_LOCK do { pthread_mutex_lock(&(csc_event_tracer.mutex));  } while(0)
    #define CSC_EVENT_TRACER_UNLOCK do { pthread_mutex_unlock(&(csc_event_tracer.mutex));  } while(0)

    typedef struct _csc_event_tracer_evt_t {
        size_t id;
        size_t tid;
        double ts;
        double te;
        char event_type[CSC_EVENT_BUFFER_LEN];
        char event_name[CSC_EVENT_BUFFER_LEN];
    } csc_event_tracer_evt_t;

    typedef struct _csc_event_tracer_t {
        pthread_mutex_t mutex;
        double toffset;
        size_t current_id;
        size_t len;
        csc_event_tracer_evt_t *events;
    } csc_event_tracer_t;

    extern csc_event_tracer_t csc_event_tracer;

    void csc_event_tracer_reset();
    void csc_event_tracer_write(const char *filename);
    size_t csc_event_tracer_event_begin(const char *type, const char *name);
    void csc_event_tracer_event_end(size_t id);

#ifdef __cplusplus
};
#endif


#endif /* end of include guard: CSC_EVENT_TRACER */
