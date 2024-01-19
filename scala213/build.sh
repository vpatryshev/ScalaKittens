#!/bin/zsh
clear && printf '\e[3J'
sbt clean test package
