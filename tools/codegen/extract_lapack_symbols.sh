#!/usr/bin/env sh
LAPACK_PATH=$(realpath "$1")
LAPACK_VERSION=$2


DEST=$(pwd)/lapack/inputs/${LAPACK_VERSION}
DEST2=$(pwd)/lapack/inputs/${LAPACK_VERSION}-wodprc


FC=${FC:=gfortran}

mkdir -p "${DEST}"
mkdir -p "${DEST2}"

for f in dlamch.f slamch.f droundup_lwork.f sroundup_lwork.f
do
    cp -v "${LAPACK_PATH}/INSTALL/${f}" "${LAPACK_PATH}/SRC/${f}"
done

cp -v "${LAPACK_PATH}/INSTALL/dsecnd_NONE.f" "${LAPACK_PATH}/SRC/dsecnd.f"
cp -v "${LAPACK_PATH}/INSTALL/second_NONE.f" "${LAPACK_PATH}/SRC/second.f"

P=$(pwd)
cd "${LAPACK_PATH}/SRC"
rm -f xerbla*
rm -f lsamen.*
rm -f ilaver.*

if [ -e la_constants.f90 ];
then
    ${FC} -c la_constants.f90
fi

if [ -e la_xisnan.F90 ];
then
    ${FC} -c la_xisnan.F90
fi

ALL=$(ls *.[fF]* | wc -l)
c=0
for i in *.[fF]*; do
    NF=$(echo $i | cut -d. -f1)
    ${FC} -fsyntax-only -fc-prototypes-external -fallow-argument-mismatch $i > ${DEST2}/$NF.h 2>> warnings-${LAPACK_VERSION}.txt
    c=$((c+1))
    echo $c
done  | tqdm --total $ALL > /dev/null

cd "${LAPACK_PATH}/SRC/DEPRECATED"

ALL=$(ls *.[fF]* | wc -l)
c=0
for i in *.[fF]*; do
    NF=$(echo $i | cut -d. -f1)
    ${FC} -fsyntax-only -fc-prototypes-external -fallow-argument-mismatch $i > ${DEST}/$NF.h 2>> warnings-${LAPACK_VERSION}.txt
    c=$((c+1))
    echo $c
done  | tqdm --total $ALL > /dev/null


cp -av "${DEST2}"/*.h "${DEST}"
rm -f "${DEST}/la_constants.h" "${DEST2}/la_constants.h"
rm -f "${DEST}/la_xisnan.h" "${DEST2}/la_xisnan.h"
