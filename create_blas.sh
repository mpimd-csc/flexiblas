#!/bin/bash 
echo -n  "Name of the interface: " 
read INAME
echo -n "Link Line of the BLAS Library: "
read LINKLINE
echo -n "Is Intel Interface [N/y]: "
read INTEL 
echo -n "Has SCABS1 [Y/n]: "
read SCABS 

ADDDEF=""
if [ "$INTEL" = "Y" -o "$INTEL" = "y" ]; then 
	ADDDEF="$ADDDEF -DZDOTC_MKL"
fi

if [ "$SCABS1" = "N" -o "$SCABS1" = "n" ]; then 
	ADDDEF="$ADDDEF -DSCABS_MISSING"
fi

echo "Link Command: gcc -shared -o libblas_${INAME}.so -Isrc src/dummy_lib.c $ADDDEF $LINKLINE" 
gcc -shared -fPIC -o libblas_${INAME}.so -Isrc src/dummy_lib.c $ADDDEF $LINKLINE
if [ $? -ne 0 ]; then 
	echo "Failed"
else 
	echo "Success"
fi 

