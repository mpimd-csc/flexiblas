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
    USE CSC_TABLE
    IMPLICIT NONE

    TYPE(C_PTR) :: TAB
    INTEGER :: COL1, COL2, COL3, COL4

    TAB = CSC_TABLE_NEW(0)

    COL1 = CSC_TABLE_ADD_COLUMN(TAB, "M", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT)
    COL2 = CSC_TABLE_ADD_COLUMN(TAB, "N", CSC_TABLE_INTEGER, CSC_TABLE_RIGHT)
    COL3 = CSC_TABLE_ADD_COLUMN(TAB, "Time", CSC_TABLE_FLOAT, CSC_TABLE_RIGHT)
    COL4 = CSC_TABLE_ADD_COLUMN(TAB, "Comment", CSC_TABLE_STRING, CSC_TABLE_LEFT)

    CALL CSC_TABLE_NEW_ROW(TAB)
    CALL CSC_TABLE_SET_ENTRY_INTEGER(TAB, COL1, 10)
    CALL CSC_TABLE_SET_ENTRY_INTEGER(TAB, COL2, 12)
    CALL CSC_TABLE_SET_ENTRY_FLOAT(TAB, COL3, 123.0D-1)
    CALL CSC_TABLE_SET_ENTRY_STRING(TAB, COL4, "Test")

    CALL CSC_TABLE_COMMENT_SIGN(TAB, "%% ")
    CALL CSC_TABLE_COMMENT_TEXT(TAB, "BLA BLA")
    CALL CSC_TABLE_COMMENT_TEXT(TAB, "BLA BLA 2")
    CALL CSC_TABLE_COMMENT_DATE(TAB)
    CALL CSC_TABLE_COMMENT_CMD(TAB)
    CALL CSC_TABLE_COMMENT_SYSINFO(TAB)


    CALL CSC_TABLE_PRINT(TAB, "  ")
    CALL CSC_TABLE_SAVE_ASCII("Table.txt", TAB, "   ")
    CALL CSC_TABLE_SAVE_LATEX("Table.tex", TAB, 1)
    CALL CSC_TABLE_DESTROY(TAB)
END PROGRAM MAIN
