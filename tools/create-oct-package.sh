#!/bin/bash
#
# This script create a tar.gz archive of the FlexiBLAS-Octave interface.
# This archive can be installed afterwards using
#
#     pkg install flexiblas-octave.tar.gz
#
# inside Octave. The Octave addon requires FlexiBLAS to be installed before.
#
OUTPUT=flexiblas-octave.tar.gz

P=$(pwd)
cd "${P}/flexiblas-octave/src"
make clean cleanall
cd "${P}"

rm -f "${OUTPUT}"
tar czvf "${OUTPUT}" flexiblas-octave/*
