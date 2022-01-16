#!/bin/bash
PYVER=$( python3 -c "import sys; print (str(sys.version_info.major)+ \".\" + str(sys.version_info.minor));" )
export PYTHONPATH=${HOME}/python-packages/lib/python${PYVER}/site-packages
S=$(pwd)

echo "Run with OpenBLAS"

FLEXIBLAS=OpenBLASPTHREAD python3 numpy-scipy-benchmark.py

echo ""
echo ""
echo "Run with Netlib"
FLEXIBLAS=Netlib python3 numpy-scipy-benchmark.py

