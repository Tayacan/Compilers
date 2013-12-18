#!/bin/sh

DIR=../DATA
PAL='../BIN/Paladim'

for f in $DIR/*.pal
do
    echo $f;
    $PAL $f;
done
