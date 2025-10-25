#!/usr/bin/env sh
export OS_ID=$(grep "^ID=" /etc/os-release | cut -d'=' -f2 | sed 's/\"//gI')

echo $OS_ID
cd debian
if [ "${OS_ID}" = "ubuntu" ]; then
    ubuntu_version=$(grep ^VERSION_ID /etc/os-release | cut -d'=' -f 2 | sed 's/\"//gI' )
    ubuntu_version_major=$(echo $ubuntu_version | cut -d. -f 1)
    ubuntu_version_minor=$(echo $ubuntu_version | cut -d. -f 2)
    ubuntu_version_int=$(($ubuntu_version_major*100 + $ubuntu_version_minor))
    if [ ${ubuntu_version_int} -gt 2504 ]; then
        ln -sf control.no_atlas control
    else
        ln -sf control.default control
    fi
else
    export OS_DIST=$(grep ^VERSION_CODENAME /etc/os-release | cut -d'=' -f2 | sed 's/\"//gI')
    debian_version=$(grep ^VERSION_ID /etc/os-release | cut -d'=' -f 2 | sed 's/\"//gI' )
    echo "Setup for Debian ${debian_version}"
    if [ ${debian_version} -gt 12 ]; then
        ln -sf control.no_atlas control
    else
        ln -sf control.default control
    fi
fi
