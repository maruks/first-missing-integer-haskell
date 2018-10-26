#!/bin/bash

seq -f "%.0f" 0 100000000 > test.txt
gshuf test.txt > test0.txt
sed -i '' '1 s/^/100000001\'$'\n/' test0.txt
sed -i '' -e 's/87654321/10000000/g' test0.txt
mv test0.txt test.txt
