# Install the CMake Modules
#SET(DEST "${CMAKE_ROOT}/share/cmake-${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION}/Modules/")
IF(INSTMODULES STREQUAL ON)
    SET(DEST ${CMAKE_ROOT}/Modules/)
    MESSAGE(STATUS "Install CMAKE Modules to: ${DEST}")
    INSTALL(FILES   FindAMD.cmake FindCOLAMD.cmake FindGfortran.cmake FindSLICOT.cmake
                    FindUMFPACK.cmake FindCHOLMOD.cmake FindCSPARSE.cmake
                    FindRRQR.cmake FindUFCONFIG.cmake FindMESS.cmake
                    DESTINATION ${DEST})
ENDIF()


