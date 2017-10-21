#!/bin/sh

set -e

ghc $1 -o $1.exe > /dev/null
./$1.exe
