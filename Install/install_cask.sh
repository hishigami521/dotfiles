#!/bin/bash
if [ `uname` = "Darwin" ]
then
    brew install cask;
else
    curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python;
fi
