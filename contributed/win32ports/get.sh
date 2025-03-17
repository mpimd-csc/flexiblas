#!/bin/sh
mkdir -p sys
rm -f *.h
rm -f sys/*.h

wget -O sys/time.h https://raw.githubusercontent.com/win32ports/sys_time_h/refs/heads/master/sys/time.h
wget https://raw.githubusercontent.com/win32ports/dirent_h/refs/heads/master/dirent.h
wget https://raw.githubusercontent.com/win32ports/getopt_h/refs/heads/master/getopt.h
wget https://raw.githubusercontent.com/win32ports/strings_h/refs/heads/master/strings.h
wget https://github.com/win32ports/unistd_h/raw/refs/heads/master/unistd.h
