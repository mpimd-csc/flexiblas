#!/bin/bash
set -x
YEAR=$(date +"%Y")
grep -lRn "[[:space:]]*Copyright (C)" ../examples/ | xargs -n 1 sed -i -e "s/^\([[:space:]]* Copyright (C)\).*\$/\1 2013-${YEAR} Martin Koehler/g"
grep -lRn "[[:space:]]*Copyright (C)" ../include/ | xargs -n 1 sed -i -e "s/^\([[:space:]]* Copyright (C)\).*\$/\1 2013-${YEAR} Martin Koehler/g"
grep -lRn "[[:space:]]*Copyright (C)" ../src/ | xargs -n 1 sed -i -e "s/^\([[:space:]]* Copyright (C)\).*\$/\1 2013-${YEAR} Martin Koehler/g"
grep -lRn "[[:space:]]*Copyright (C)" ./code_generators/ | xargs -n 1 sed -i -e "s/^\([[:space:]]* Copyright (C)\).*\$/\1 2013-${YEAR} Martin Koehler/g"
grep -lRn "[[:space:]]*Copyright (C)" ./flexiblas-octave/ | xargs -n 1 sed -i -e "s/^\([[:space:]]* Copyright (C)\).*\$/\1 2013-${YEAR} Martin Koehler/g"
sed -i -r -e  "s/^Copyright [0-9\-]{1,} (.*)/Copyright 2013-${YEAR} \1/g" ../README.md
sed -i -r -e  "s/2013-[0-9]{4}/2013-${YEAR}/g" ../CMakeLists.txt
grep -lREn "2013-[0-9]{4}" ../doc/ | xargs -n 1 sed -i -E -e "s/2013-[0-9]{4}/2013-${YEAR}/g"
