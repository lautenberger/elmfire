#!/bin/bash

CWD=$(pwd)
ELMFIRE_CLEAN=$HOME/elmfire_clean
CLOUDFIRE_CLEAN=$HOME/cloudfire_clean

rm -f -r $CWD/temp
mkdir -p $CWD/temp/elmfire $CWD/temp/cloudfire

cd $ELMFIRE_CLEAN
git pull
cp -f -r * $CWD/temp/elmfire/

cd $CLOUDFIRE_CLEAN
git pull
cp -f -r * $CWD/temp/cloudfire/

cd $CWD
docker build -t elmfire .

rm -f -r $CWD/temp

exit 0
