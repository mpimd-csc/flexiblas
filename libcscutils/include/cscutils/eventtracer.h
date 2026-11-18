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

/**
 * @file eventtracer.h
 * @brief Event Tracing API for performance profiling and debugging
 *
 * The event tracer provides a simple API for recording timing events in code.
 * It allows developers to mark the beginning and end of specific operations
 * and then write the collected timing data to a file for analysis.
 *
 * Typical usage:
 * 1. Call csc_event_tracer_reset() to initialize the tracer
 * 2. Use csc_event_tracer_event_begin() and csc_event_tracer_event_end()
 *    to mark events during execution
 * 3. Call csc_event_tracer_write() to save the collected data to a file
 *
 * The tracer is thread-safe and can be used in multi-threaded applications.
 */

#ifndef CSC_EVENT_TRACER

#define CSC_EVENT_TRACER

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#include "cscutils/cscutils_config.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C"
{
#endif
#define CSC_EVENT_BUFFER_LEN 128
#define CSC_EVENT_TRACER_LOCK                                                                                                        \
    do                                                                                                                               \
    {                                                                                                                                \
        pthread_mutex_lock(&(csc_event_tracer.mutex));                                                                               \
    } while (0)
#define CSC_EVENT_TRACER_UNLOCK                                                                                                      \
    do                                                                                                                               \
    {                                                                                                                                \
        pthread_mutex_unlock(&(csc_event_tracer.mutex));                                                                             \
    } while (0)

    /**
     * @brief Represents a single traced event
     *
     * This structure holds information about a single timing event including
     * its unique identifier, thread ID, timestamps, and descriptive information.
     */
    typedef struct _csc_event_tracer_evt_t
    {
        size_t id;                             /**< Unique event identifier */
        size_t tid;                            /**< Thread identifier */
        double ts;                             /**< Start timestamp (seconds) */
        double te;                             /**< End timestamp (seconds) */
        char event_type[CSC_EVENT_BUFFER_LEN]; /**< Event type/category */
        char event_name[CSC_EVENT_BUFFER_LEN]; /**< Event name/description */
    } csc_event_tracer_evt_t;

    /**
     * @brief Main event tracer structure
     *
     * This structure maintains the state of the event tracer including
     * the mutex for thread safety, time offset, event counter, buffer size,
     * and the array of recorded events.
     */
    typedef struct _csc_event_tracer_t
    {
        pthread_mutex_t mutex;          /**< Mutex for thread-safe access */
        double toffset;                 /**< Time offset for relative timing */
        size_t current_id;              /**< Next available event ID */
        size_t len;                     /**< Current buffer size */
        csc_event_tracer_evt_t* events; /**< Array of recorded events */
    } csc_event_tracer_t;

    extern csc_event_tracer_t csc_event_tracer;

    /**
     * @brief Reset the event tracer to its initial state
     *
     * This function clears all recorded events and resets the internal state.
     * It should be called before starting a new tracing session.
     */
    void csc_event_tracer_reset();

    /**
     * @brief Write all recorded events to a file
     *
     * This function writes the collected timing data to the specified file
     * in CSV format. Each line contains: id,tid,start_time,end_time,type,name
     *
     * @param filename The name of the file to write the events to
     */
    void csc_event_tracer_write(const char* filename);

    /**
     * @brief Mark the beginning of an event
     *
     * This function records the start of a timing event with the given type
     * and name. It returns an event ID that should be used with
     * csc_event_tracer_event_end() to mark the end of the event.
     *
     * @param type The type or category of the event
     * @param name The name or description of the event
     * @return The event ID that should be used to end the event
     */
    size_t csc_event_tracer_event_begin(const char* type, const char* name);

    /**
     * @brief Mark the end of an event
     *
     * This function records the end of a timing event that was started with
     * csc_event_tracer_event_begin(). The event ID must match the one returned
     * by the begin function.
     *
     * @param id The event ID returned by csc_event_tracer_event_begin()
     */
    void csc_event_tracer_event_end(size_t id);

#ifdef __cplusplus
};
#endif

#endif /* end of include guard: CSC_EVENT_TRACER */
