!   SPDX-License-Identifier: LGPL-3.0-or-later
!
! This file is part of libcscutils, a set of helper function.
! Copyright (C) 2013-2024 Martin Koehler
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3 of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this program; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!



MODULE CSC_EVENT_TRACER
    USE ISO_C_BINDING
    IMPLICIT NONE

    INTEGER, PARAMETER :: CSC_HDF5_T = C_INT
    INTEGER(C_INT), PARAMETER :: CSC_HDF5_FIELD_REAL = 0

    INTERFACE
        ! csc_event_tracer_reset
        SUBROUTINE CSC_EVENT_TRACER_RESET() BIND(C, name="csc_event_tracer_reset")
            IMPORT
        END SUBROUTINE CSC_EVENT_TRACER_RESET


        ! csc_event_tracer_write
        SUBROUTINE CSC_EVENT_TRACER_WRITE_C(FILENAME) BIND(C, name="csc_event_tracer_write")
            IMPORT
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: FILENAME
        END SUBROUTINE CSC_EVENT_TRACER_WRITE_C

        INTEGER(KIND=C_SIZE_T) FUNCTION CSC_EVENT_TRACER_EVENT_BEGIN_C(EVENT_TYPE, EVENT_NAME) &
                & BIND(C, name="csc_event_tracer_event_begin")
            IMPORT
            CHARACTER(KIND=C_CHAR), INTENT(IN)     :: EVENT_TYPE, EVENT_NAME
        END FUNCTION CSC_EVENT_TRACER_EVENT_BEGIN_C

        SUBROUTINE CSC_EVENT_TRACER_EVENT_END_C(ID) BIND(C, name="csc_event_tracer_event_end")
            IMPORT
            INTEGER(KIND = C_SIZE_T), INTENT(IN), VALUE :: ID
        END SUBROUTINE CSC_EVENT_TRACER_EVENT_END_C
    END INTERFACE

CONTAINS


    ! WRITE WRAPPER
    SUBROUTINE CSC_EVENT_TRACER_WRITE(FILENAME)
        CHARACTER(LEN=*), INTENT(IN) :: FILENAME

        CALL CSC_EVENT_TRACER_WRITE_C(TRIM(FILENAME)//C_NULL_CHAR)
        RETURN
    END SUBROUTINE CSC_EVENT_TRACER_WRITE

    ! Close Wrapper
    INTEGER FUNCTION CSC_EVENT_TRACER_EVENT_BEGIN(EVENT_TYPE, EVENT_NAME)
        CHARACTER(LEN=*), INTENT(IN) :: EVENT_NAME, EVENT_TYPE

        INTEGER(KIND=C_SIZE_T) :: RET


        RET = CSC_EVENT_TRACER_EVENT_BEGIN_C(TRIM(EVENT_TYPE)//C_NULL_CHAR, TRIM(EVENT_NAME) // C_NULL_CHAR)
        CSC_EVENT_TRACER_EVENT_BEGIN = INT(RET)
        RETURN
    END FUNCTION CSC_EVENT_TRACER_EVENT_BEGIN

    SUBROUTINE CSC_EVENT_TRACER_EVENT_END(ID)
        INTEGER, INTENT(IN) :: ID

        INTEGER(KIND=C_SIZE_T) :: IDPASS


        IDPASS = INT(ID, KIND = C_SIZE_T)
        CALL CSC_EVENT_TRACER_EVENT_END_C(IDPASS)
        RETURN
    END SUBROUTINE CSC_EVENT_TRACER_EVENT_END

END MODULE

