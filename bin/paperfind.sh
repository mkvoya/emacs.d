#!/bin/bash

cd $1
shift

find . -not \( -wholename "*/.*" -prune \) $@
