dir=$(dirname $0)
if [ "$dir" = '.' ]; then
 dir=`pwd`
fi
name=$(basename "$dir")
echo "************************************"
echo "**     building $name          **"
echo "************************************"
pushd $name
echo "Version 2.10 has reached EOL. Bye."
#sbt clean test package
popd
