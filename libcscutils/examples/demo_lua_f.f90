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




PROGRAM MAIN
    USE CSC_LUA
    USE ISO_C_BINDING
    IMPLICIT NONE

    CHARACTER(LEN=1024) :: FILENAME
    TYPE(C_PTR) :: LUA
    INTEGER :: INFO, RETVAL1, RETVAL2

    IF ( COMMAND_ARGUMENT_COUNT() .NE. 1) THEN
        WRITE(*,*) "Need a lua script as command line argument"
        STOP 1
    END IF

    CALL GET_COMMAND_ARGUMENT(1, FILENAME)
    WRITE(*,*) "Load:", TRIM(FILENAME)

    CALL CSC_LUA_INIT(LUA, INFO)

    IF (INFO.NE.0 ) WRITE(*,*) "LUA INIT FAILED INFO = ", INFO

    CALL CSC_LUA_LOADFILE(LUA, TRIM(FILENAME), INFO)

    IF (INFO.NE.0 ) WRITE(*,*) "LUA LOADFILE FAILED INFO = ", INFO


    CALL CSC_LUA_GLOBAL(LUA, "testint", 1245, INFO)
    CALL CSC_LUA_GLOBAL(LUA, "testnumber", 1.2D1, INFO)
    CALL CSC_LUA_GLOBAL(LUA, "teststring", "Hello", INFO)

    CALL CSC_LUA_RUN(LUA, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA RUN FAILED INFO = ", INFO

    CALL CSC_LUA_CALL_RETURN_NULL(LUA, "arg0ret0", INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO

    CALL CSC_LUA_CALL_RETURN_INT(LUA, "arg0reti",  RETVAL1, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1

    CALL CSC_LUA_CALL_RETURN_INT(LUA, "argireti", 2, RETVAL1, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1

    CALL CSC_LUA_CALL_RETURN_INT(LUA, "argiireti", 2, 8, RETVAL1, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1

    CALL CSC_LUA_CALL_RETURN_INT(LUA, "argiiireti", 2, 8, 16, RETVAL1, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1

    CALL CSC_LUA_CALL_RETURN_INT(LUA, "argsiireti", "huhu", 8, 16, RETVAL1, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1

    CALL CSC_LUA_CALL_RETURN_2INT(LUA, "arg0retii", RETVAL1, RETVAL2, INFO)
    IF (INFO.NE.0 ) WRITE(*,*) "LUA CALL FAILED INFO = ", INFO
    WRITE(*,*) "RETVAL1 = ", RETVAL1
    WRITE(*,*) "RETVAL2 = ", RETVAL2



    CALL CSC_LUA_FINALIZE(LUA, INFO)

END PROGRAM MAIN
