#!/bin/bash
MAJOR=$(echo $1 | cut -d. -f 1)
MINOR=$(echo $1 | cut -d. -f 2)
PATCH=$(echo $1 | cut -d. -f 3)

sed -i -e "s/^version.*\$/version: $1/g" CODE
sed -i -e "s/^version.*\$/version: $1/g" CITATION.cff
sed -i -e "s/^Version.*\$/Version: $1/g" tools/flexiblas-octave/DESCRIPTION
sed -i -e "s/^VERSION=.*\$/VERSION=$1/g" tools/create_tar_ball.sh
sed -i -e "/^PROJECT/ s/VERSION [0-9]\+\.[0-9]\+\.[0-9]\+/VERSION $1/g" CMakeLists.txt
sed -i -e "s/Version [0-9]\+\.[0-9]\+\.[0-9]\+/Version $1/g" README.md

cat > ./.version << EOF
MAJOR=$MAJOR
MINOR=$MINOR
PATCH=$PATCH
EOF


