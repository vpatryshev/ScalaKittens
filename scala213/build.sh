#!/bin/zsh
clear && printf '\e[3J'
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk use java 22.3.4.r11-nik
dir0=$(dirname $0)
dir=$dir0
if [ "$dir" = '.' ]; then
 dir=`pwd`
fi
name=$(basename "$dir")
echo "************************************"
echo "**     building $name          **"
echo "************************************"

if [ "$dir0" != '.' ]; then
  pushd $name
fi

sbt clean test package
res=$?

if [ "$dir0" != '.' ]; then
  popd
fi
