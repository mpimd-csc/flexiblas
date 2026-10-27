#!/bin/bash
if [ $# -ne 1 ]; then
    echo "Usage: $0 DOI"
    exit 1
fi

export DOI=$1
sed -i -e "s,^id:.*\$,id: ${DOI},g" CODE
sed -i -e "s,^doi:.*\$,doi: ${DOI},g" CITATION.cff
sed -i -e "s,DOI:.*\$,DOI: ${DOI},g" README.md


