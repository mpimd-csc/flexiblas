#!/bin/sh
VERSION=""
if [ -e ./.version ]; then
    . ./.version
    VERSION="$MAJOR.$MINOR.$PATCH"
fi

if [ -d .git -a -n "$(which git)" ]; then
    VERSION=$(git describe --match 'v[0-9]*.[0-9]*.[0-9]*' --long | sed 's/-g[0-9a-f]\+$//' | sed 's/^v//g' )
fi

if [ "$1" = "deb" ]; then
    VERSION=$(echo $VERSION | sed 's/-/+/g' )
fi
echo $VERSION

