#!/bin/bash

CWD=$(pwd)
ELMFIRE_CLEAN=$HOME/elmfire_clean

rm -f -r $CWD/temp
mkdir -p $CWD/temp/elmfire

cd $ELMFIRE_CLEAN
git pull
cp -f -r * $CWD/temp/elmfire/

cd $CWD
docker build -t elmfire .

rm -f -r $CWD/temp

exit 0
