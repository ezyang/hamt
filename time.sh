#!/bin/sh
#export TIME="%E real\n%U user\n%S sys"
export TIME="+ %E"
time ./HAMTTest $1
time ./IntMapTest $1
echo "----"
