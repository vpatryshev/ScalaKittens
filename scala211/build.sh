#!/bin/zsh
clear && printf '\e[3J'
# Use the following to make sdkman available
#source "$HOME/.sdkman/bin/sdkman-init.sh"
# use the following and choose the jdk if the one you have does not work for you
#sdk use java 17.0.9-amzn # 11.0.21-ms # 8.0.392-amzn
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
