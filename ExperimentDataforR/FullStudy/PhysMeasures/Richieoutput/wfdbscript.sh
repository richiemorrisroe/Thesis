#!/bin/bash
files=`ls |grep .txt`
outfiles=`echo $files|sed 's/.txt//g' `

for i in $files
do 
    echo $i
     wrsamp -z -F 10000 -i $i  -o $(echo $i |sed 's/.txt//g') 1
done
