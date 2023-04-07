#!/bin/bash

CWD=$(pwd)
ELMFIRE_CLEAN=$HOME/elmfire_clean

cd $ELMFIRE_CLEAN
git pull
cd $CWD

mkdir temp
cp -f -r $ELMFIRE_CLEAN/* ./temp/

docker build -t elmfire .

rm -f -r temp

exit 0
