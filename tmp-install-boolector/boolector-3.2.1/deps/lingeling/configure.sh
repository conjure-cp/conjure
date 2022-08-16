#!/bin/sh

dema=no
check=undefined
chksol=undefined
coverage=undefined
debug=no
log=undefined
lto=no
olevel=none
other=none
druplig=undefined
profile=undefined
static=no
classify=no
aiger=undefined
yalsat=undefined
files=no

##########################################################################

die () {
  echo "*** configure.sh: $*" 1>&2
  exit 1
}

##########################################################################

while [ $# -gt 0 ]
do
  case $1 in
    -h|--help) 
       echo "usage: configure.sh [<option> ...]"
       echo
       echo "where <option> is one of the following"
       echo
       echo "-h | --help"
       echo "-g | --debug    include debugging code and symbols"
       echo "-l | --log      include logging code (default with '-g')"
       echo "-c | --check    include checking code (default with '-g')"
       echo "-p | --profile  compile with '-pg' for profiling"
       echo "--coverage      compile with coverage options"
       echo "--no-check      no checking code (overwrite default for '-g')"
       echo "--chksol        always check solution (default for '-c')"
       echo "--no-chksol     do not check solution"
       echo "--softfloats    use software floats"
       echo "--no-log        no logging code (overwrite default for '-g')"
       echo "--dema"
       echo "--no-dema"
       echo "-O[0-4]         set optimization level unless '-g' specified"
       echo "-flto           enable link time optimization"
       echo "-f...|-m...     add other compiler options"
       echo "--aiger=<dir>   specify AIGER directory (default '../aiger')"
       echo "--no-aiger      no targets requiring AIGER library"
       echo "--yalsat=<dir>  specify YalSAT directory (default '../yalsat')"
       echo "--no-yalsat     do not include YalSAT code"
       echo "--druplig       specify Druplig directory (default '../druplig')"
       echo "--no-druplig    do not include Druplig code"
       echo "--files         generate statistics files"
       echo
       echo "--classify      use classifier for automatic parameter setting"
       exit 0
       ;;
    -g|--debug) debug=yes;;
    -l|--log) log=yes;;
    -c|--check) check=yes;;
    --chksol) chksol=yes;;
    --no-chksol|--nchksol) chksol=no;;
    --no-check) check=no;;
    --no-log) log=no;;
    -p|--profile) profile=yes;;
    --coverage) coverage=yes;;
    -O) debug=no;;
    --dema) dema=yes;;
    --no-dema) dema=no;;
    -O0|-O1|-O2|-O3|-O4) olevel=$1;;
    -lto|-flto|--lto|--flto) lto=yes;;
    -static|--static|-s) static=yes;;
    --aiger=*) aiger=`echo "$1"|sed -e 's,^--aiger=,,'`;;
    --no-aiger) aiger=no;;
    --yalsat=*) yalsat=`echo "$1"|sed -e 's,^--yalsat=,,'`;;
    --no-yalsat) yalsat=no;;
    --druplig) druplig=`echo "$1"|sed -e 's,^--druplig=,,'`;;
    --no-druplig) druplig=no;;
    --files) files=yes;;
    --classify) classify=yes;;
    -f*|-m*) if [ $other = none ]; then other=$1; else other="$other $1"; fi;;
    *) echo "*** configure.sh: invalid command line option '$1'"; exit 1;;
  esac
  shift
done

##########################################################################

if [ x"$aiger" = xundefined ]
then
  if [ -d ../aiger ]
  then
    aiger="../aiger"
    echo "found and using $aiger"
  fi
fi

if [ x"$aiger" = xundefined ]
then
  aiger=no
elif [ ! x"$aiger" = xno ]
then
  if [ ! -d "$aiger" ]
  then
    die "'$aiger' not a directory (use '--aiger' or '--no-aiger')"
  elif [ ! -f "$aiger/aiger.h" ]
  then
    die "can not find '$aiger/aiger.h'"
  elif [ ! -f "$aiger/aiger.o" ]
  then
    die "can not find '$aiger/aiger.o'"
  fi
fi

##########################################################################

if [ x"$yalsat" = xundefined ]
then
  if [ -d ../yalsat ]
  then
    yalsat="../yalsat"
    echo "found and using $yalsat"
  fi
fi

if [ x"$yalsat" = xundefined ]
then
  yalsat=no
elif [ ! x"$yalsat" = xno ]
then
  if [ ! -d "$yalsat" ]
  then
    die "'$yalsat' not a directory (use '--yalsat' or '--no-yalsat')"
  elif [ ! -f "$yalsat/yals.h" ]
  then
    die "can not find '$yalssat/yals.h'"
  elif [ ! -f "$yalsat/libyals.a" ]
  then
    die "can not find '$yalsat/libyals.a'"
  fi
fi

##########################################################################

if [ x"$druplig" = xundefined ]
then
  if [ -d ../druplig ]
  then
    druplig="../druplig"
    echo "found and using $druplig"
  fi
fi

if [ x"$druplig" = xundefined ]
then
  druplig=no
elif [ ! x"$druplig" = xno ]
then
  if [ ! -d "$druplig" ]
  then
    die "'$druplig' not a directory (use '--druplig' or '--no-druplig')"
  elif [ ! -f "$druplig/druplig.h" ]
  then
    die "can not find '$yalssat/druplig.h'"
  elif [ ! -f "$druplig/libdruplig.a" ]
  then
    die "can not find '$druplig/libdruplig.a'"
  fi
fi

##########################################################################

[ $log = undefined ] && log=$debug
[ $check = undefined ] && check=$debug

##########################################################################

[ x"$CC" = x ] && CC=gcc

CFLAGS="-Wall"
if [ $debug = yes ]
then
  CFLAGS="$CFLAGS -g3"
  [ $olevel = none ] || CFLAGS="$CFLAGS $olevel"
else
  [ $olevel = none ] && olevel=-O3
  CFLAGS="$CFLAGS $olevel"
  [ $lto = yes ] && CFLAGS="$CFLAGS -flto -fwhole-program"
fi

LIBS="-lm"
HDEPS=""
LDEPS=""

if [ "$aiger" = no ]
then
  AIGERTARGETS=""
  AIGER=""
else
  AIGERTARGETS="blimc"
  AIGER="$aiger"
fi

if [ ! "$yalsat" = no ]
then
  [ x"$HDEPS" = x ] || HDEPS="${HDEPS} "
  HDEPS="${HDEPS}$yalsat/yals.h"
  [ x"$LDEPS" = x ] || LDEPS="${LDEPS} "
  LDEPS="${LDEPS}$yalsat/libyals.a"
  [ x"$LIBS" = x ] || LIBS="${LIBS} "
  LIBS="${LIBS}-L$yalsat -lyals"
  [ x"$CFLAGS" = x ] || CFLAGS="${CFLAGS} "
  CFLAGS="${CFLAGS}-I$yalsat"
fi

if [ ! "$druplig" = no ]
then
  [ x"$HDEPS" = x ] || HDEPS="${HDEPS} "
  HDEPS="${HDEPS}$druplig/druplig.h"
  [ x"$LDEPS" = x ] || LDEPS="${LDEPS} "
  LDEPS="${LDEPS}$druplig/libdruplig.a"
  [ x"$LIBS" = x ] || LIBS="${LIBS} "
  LIBS="${LIBS}-L$druplig -ldruplig"
  [ x"$CFLAGS" = x ] || CFLAGS="${CFLAGS} "
  CFLAGS="${CFLAGS}-I$druplig"
fi

[ $chksol = undefined ] && chksol=$check
[ $static = yes ] && CFLAGS="$CFLAGS -static"
[ $profile = yes ] && CFLAGS="$CFLAGS -pg"
[ $coverage = yes ] && CFLAGS="$CFLAGS -ftest-coverage -fprofile-arcs"
[ $other = none ] || CFLAGS="$CFLAGS $other"
[ $log = no ] && CFLAGS="$CFLAGS -DNLGLOG"
[ $check = no ] && CFLAGS="$CFLAGS -DNDEBUG"
[ $chksol = no ] && CFLAGS="$CFLAGS -DNCHKSOL"
[ $druplig = no ] && CFLAGS="$CFLAGS -DNLGLDRUPLIG"
[ $yalsat = no ] && CFLAGS="$CFLAGS -DNLGLYALSAT"
[ $files = no ] && CFLAGS="$CFLAGS -DNLGLFILES"
[ $dema = no ] && CFLAGS="$CFLAGS -DNLGLDEMA"

if [ $classify = yes -a -d sc14classify ]
then
  cd sc14classify
  if [ $debug = yes ]
  then
    echo "calling 'configure.sh -g' in sub-directory 'sc14classify'"
    ./configure.sh -g || exit 1
  else
    echo "calling 'configure.sh' in sub-directory 'sc14classify'"
    ./configure.sh || exit 1
  fi
  echo "calling 'make sc14classify.o' in sub-directory 'sc14classify'"
  make sc14classify.o || exit 1
  cd ..
  [ x"$CFLAGS" = x ] || CFLAGS="${CFLAGS} "
  CFLAGS="$CFLAGS -DLGLSC14CLASSIFY"
  [ x"$EXTRAOBJS" = x ] || EXTRAOBJS="${EXTRAOBJS} "
  EXTRAOBJS="sc14classify/sc14classify.o"
fi

echo "$CC $CFLAGS"

##########################################################################

rm -f makefile
sed \
  -e "s,@CC@,$CC," \
  -e "s,@CFLAGS@,$CFLAGS," \
  -e "s,@HDEPS@,$HDEPS," \
  -e "s,@LDEPS@,$LDEPS," \
  -e "s,@EXTRAOBJS@,$EXTRAOBJS," \
  -e "s,@AIGERTARGETS@,$AIGERTARGETS," \
  -e "s,@AIGER@,$AIGER," \
  -e "s,@LIBS@,$LIBS," \
  makefile.in > makefile
