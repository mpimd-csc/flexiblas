//    SPDX-License-Identifier: LGPL-3.0-or-later
/*
   This file is part of FlexiBLAS, a BLAS/LAPACK interface wrapper library.
   Copyright (C) 2013-2025 Martin Koehler

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

   SPDX-FileCopyrightText: Portions Copyright 2024 Siemens and/or its affiliates

   November 2024 modified by Siemens and/or its affiliates by adding Windows support
*/


#ifndef WINDOWS_FIXES_H
#define WINDOWS_FIXES_H 1

#if defined(_WIN32) || defined(_WIN64)

#ifndef strtok_r
#define strtok_r(s,d,p) strtok_s(s,d,p)
#endif

inline char * dirname( char const * pathname)
{
  // Parameter Validation
  if( pathname == NULL)
  {
    return NULL;
  }

  char * base_dir = NULL;

  // Returns a pointer to the last occurrence of \ in pathname or NULL if it is not found
  char const * p_end_of_path = strrchr( pathname, '\\' );
  if( p_end_of_path != NULL )
  {
    size_t path_length = (size_t)( p_end_of_path - pathname );
    if (path_length > 0)
    {
      base_dir = (char*) malloc(sizeof(char) * path_length + 1);
      if (base_dir)
      {
        // Copy the base path into the out variable
        if( strncpy( base_dir, pathname, path_length ) == NULL )
        {
          free(base_dir);
          return NULL;
        }

        base_dir[path_length] = '\0';
      }
    }
  }

  return base_dir;
}

#include <windows.h>

/* pthread mutex mocking */
typedef CRITICAL_SECTION pthread_mutex_t;
typedef void* pthread_mutexattr_t;

inline int pthread_mutex_init(pthread_mutex_t * mutex, const pthread_mutexattr_t * attr)
{
  (void) attr;
  InitializeCriticalSection(mutex);
  return 0;
}

inline int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
  DeleteCriticalSection(mutex);
  return 0;
}

inline int pthread_mutex_lock(pthread_mutex_t *mutex)
{
  EnterCriticalSection(mutex);
  return 0;
}

inline int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
  LeaveCriticalSection(mutex);
  return 0;
}

#endif /* defined(_WIN32) || defined(_WIN64) */

#endif /* WINDOWS_FIXES_H  */
