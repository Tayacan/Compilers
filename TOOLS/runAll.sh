#!/bin/sh

DIR=../DATA
PAL='../BIN/Paladim -ti '

for f in $DIR/*.pal
do
    echo $f;
    $PAL $f;
done
