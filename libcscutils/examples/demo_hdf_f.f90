!    SPDX-License-Identifier: LGPL-3.0-or-later
!
!  This file is part of libcscutils, a set of helper function.
!  Copyright (C) 2013-2024 Martin Koehler
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation; either
!  version 3 of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public License
!  along with this program; if not, write to the Free Software Foundation,
!  Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!




PROGRAM MAIN
    USE CSC_HDF5
    IMPLICIT NONE

    DOUBLE PRECISION A(3,4)

    INTEGER(CSC_HDF5_T) :: HDF_FILE, GID

    A = RESHAPE((/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12/), (/3,4/))

    CALL CSC_HDF5_SET_COMPRESSION_F(9)

    ! Open HDF5 File
    CALL CSC_HDF5_OPEN_F(HDF_FILE, "test.h5", "rw")

    ! Create a group
    CALL CSC_HDF5_GROUP_OPEN_F(GID, HDF_FILE, "testgroup")

    ! Store matrix
    CALL CSC_HDF5_MATRIX_WRITE_REAL_F(GID, "A", 3, 4, A, 3)

    ! close the group
    CALL CSC_HDF5_GROUP_CLOSE_F(GID)
    ! Close HDF5 File
    CALL CSC_HDF5_CLOSE_F(HDF_FILE)

END PROGRAM MAIN
