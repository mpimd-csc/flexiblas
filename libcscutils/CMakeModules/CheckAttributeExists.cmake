# - Check if a C compiler attribute exists
# CHECK_ATTRIBUTE_EXISTS(<attribute> <variable>)
#
# Check that the <attribute> is supported by the compiler and
# store the result in a <variable>.
#
#=============================================================================
# Copyright 2012 Martin Köhler, MPI-Magdeburg
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)
INCLUDE(CheckCSourceCompiles)
MACRO(CHECK_ATTRIBUTE_EXISTS ATTRIBUTE VARIABLE)
    	SET(_FAIL_REGEX "attribute directive ignored")

	MESSAGE(STATUS "Check for __attribute__((${ATTRIBUTE}))")
	FILE(WRITE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.c"
		"void  bar (int bar) __attribute__((${ATTRIBUTE})); 
	        void  foo (int bar) { return; }
		int main(){}")
	TRY_COMPILE(COMPILES
	    ${CMAKE_BINARY_DIR}
	    ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeTmp/src.c
	    OUTPUT_VARIABLE OUTPUT)  
	FOREACH(_regex ${_FAIL_REGEX})
		IF("${OUTPUT}" MATCHES "${_regex}")
			SET(COMPILES FALSE)
		ENDIF()
	ENDFOREACH()                    
	IF(COMPILES)
		  MESSAGE(STATUS "Looking for attribute:  ${ATTRIBUTE}  - works")
		  SET (${VARIABLE} 1 CACHE INTERNAL "Have attribute ${ATTRIBUTE}")
		  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log 
		  		 "Determining if the attribute ${ATTRIBUTE} exists passed with the following output:\n"
		  	 	 "${OUTPUT}\n\n" )
	  
	ELSE(COMPILES)
		  MESSAGE(STATUS "Looking for attribute ${ATTRIBUTE} - not found")
		  SET (${VARIABLE} "" CACHE INTERNAL "Have attribute ${ATTRIBUTE}")
		  FILE(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log 
		  		  "Determining if the attribute ${ATTRIBUTE} exists failed with the following output:\n"
		  	  "${OUTPUT}\n\n")
	ENDIF(COMPILES)
ENDMACRO(CHECK_ATTRIBUTE_EXISTS)
