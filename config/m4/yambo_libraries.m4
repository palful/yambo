#
#        Copyright (C) 2000-2020 the YAMBO team
#              http://www.yambo-code.org
#
# Authors (see AUTHORS file for details): AM
#
# This file is distributed under the terms of the GNU
# General Public License. You can redistribute it and/or
# modify it under the terms of the GNU General Public
# License as published by the Free Software Foundation;
# either version 2, or (at your option) any later version.
#
# This program is distributed in the hope that it will
# be useful, but WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public
# License along with this program; if not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,
# MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
#
#
AC_DEFUN([AC_YAMBO_LIBRARIES],[

DRIVER_INCS="-I$PWD/lib/yambo/driver/include/ -I$PWD/include/driver"
AC_SUBST(DRIVER_INCS)

AC_MSG_CHECKING([driver lib])

if test -d "src/real_time_el-ph/"; then
  #
  # develop procedure
  #
  cd lib/
  if ! test -d "yambo/driver/src"; then
    git clone git@github.com:yambo-code/yambo-libraries.git yambo
  else
    cd yambo
    git pull
    cd ../
  fi
  cd ../
  #
else
  #
  # gpl procedure
  #
  if ! test -d "yambo/driver/src"; then
    cd lib/yambo/
    rm $TARBALL
    URL="https://github.com/yambo-code/yambo-libraries/archive/0.0.1.tar.gz"
    TARBALL="0.0.1.tar.gz"
    if test -x $(command -v wget) ; then
      wget --no-check-certificate -O ${TARBALL} ${URL} ;
    elif test -x $(command -v curl) ; then
      curl -L --progress-bar -o ${TARBALL} ${URL} ;
    fi
    if test ! -s $TARBALL ; then
      echo "*** Unable to download ${TARBALL}. Test whether curl or wget is installed and working," ;
      echo "*** if you have direct access to the internet." ;
      echo "*** If not, download ${URL}${TARBALL} into folder lib and extract it" ;
    else
      echo "Extracting yambo internal library ${TARBALL}" ;
      tar -xzf $TARBALL
    fi
    cd ../../
  fi
  #
fi

m4_include([lib/yambo/driver/config/version.m4])

AC_MSG_RESULT([@ version $YDRI_VERSION.$YDRI_SUBVERSION.$YDRI_PATCHLEVEL])

])