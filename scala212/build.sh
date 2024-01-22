#!/bin/zsh
clear && printf '\e[3J'
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk use java 11.0.21-ms
dir=$(dirname $0)
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

exit $res
