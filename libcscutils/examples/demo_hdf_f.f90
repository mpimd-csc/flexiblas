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
