#!/bin/sh

FUN=$1

echo $0

if [ $# -ne 1 ]; then
    echo "!!! exutable not specified"
    exit 1
fi
if [ ! -x ${FUN} ]; then
    echo "!!! ${FUN} is not executable"
    exit 1
fi

CNT=0

for x in `find . -name "*.fun" -print`; do
    TEST=${x%.*}
    CNT=$((CNT+1))
    echo "[${CNT}] testing " ${TEST} 
    time ${FUN} ${TEST}.fun > ${TEST}.result
    diff ${TEST}.result ${TEST}.expected
    if [ $? -eq 0 ]; then
        echo "=> OK"
    else
        exit 1
    fi
done

echo "Total ${CNT} tests (*^-^)v"

exit 0
