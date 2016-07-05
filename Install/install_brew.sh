#!/bin/bash

line_no=1;

while read LINE
do
    count='echo "$LINE" | wc -c';
    echo "$line_no : $LINE";
    line_no=`expr $line_no + 1`;
done < $1

exit 0
