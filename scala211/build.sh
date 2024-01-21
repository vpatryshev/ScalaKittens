#!/bin/zsh
clear && printf '\e[3J'
# Use the following to make sdkman available
#source "$HOME/.sdkman/bin/sdkman-init.sh"
# use the following and choose the jdk if the one you have does not work for you
#sdk use java 17.0.9-amzn # 11.0.21-ms # 8.0.392-amzn

sbt clean test package
