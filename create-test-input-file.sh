#!/bin/bash

seq -f "%.0f" 0 100000000 > test.txt
gshuf test.txt > test0.txt
sed -i '' -n -e 'H;${x;s/\n/,/g;s/^,//;p;}' test0.txt
sed -i '' -e 's/12345678/12223334/g' test0.txt
mv test0.txt test.txt
