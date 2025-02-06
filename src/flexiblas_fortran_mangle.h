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
   */





#ifndef FLEXIBLAS_FORTRAN_MANGLE_HEADER_INCLUDED
#define FLEXIBLAS_FORTRAN_MANGLE_HEADER_INCLUDED

#include "flexiblas_config.h"

#if defined(FLEXIBLAS_MANGLE_DEFAULT)
#define FC_GLOBAL(name,NAME) name##_
#define FC_GLOBAL_(name,NAME) name##_

#define FC_GLOBAL2(name,NAME) name
#define FC_GLOBAL2_(name,NAME) name

#define FC_GLOBAL3(name,NAME)  NAME
#define FC_GLOBAL3_(name,NAME) NAME

#elif defined(FLEXIBLAS_MANGLE_NO_UNDERSCORE)

#define FC_GLOBAL(name,NAME) name
#define FC_GLOBAL_(name,NAME) name

#define FC_GLOBAL2(name,NAME) name##_
#define FC_GLOBAL2_(name,NAME) name##_

#define FC_GLOBAL3(name,NAME)  NAME
#define FC_GLOBAL3_(name,NAME) NAME

#elif defined(FLEXIBLAS_MANGLE_UPPERCASE)
#define FC_GLOBAL(name,NAME) NAME
#define FC_GLOBAL_(name,NAME) NAME

#define FC_GLOBAL2(name,NAME) name##_
#define FC_GLOBAL2_(name,NAME) name##_

#define FC_GLOBAL3(name,NAME)  name
#define FC_GLOBAL3_(name,NAME) name

#else
#error No Fortran Name Mangling set.
#endif

#endif
