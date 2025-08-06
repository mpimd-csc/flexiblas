#!/usr/bin/env sh
export OS_DIST=$(grep ^VERSION_CODENAME /etc/os-release | cut -d'=' -f2 | sed 's/\"//gI')
cd debian
if [ -f control.${OS_DIST} ]; then
    ln -sf control.${OS_DIST} control
else
    ln -sf control.default control
fi
