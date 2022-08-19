#!/bin/bash

if (( $# > 0 )); then
    swipl -g run_tests -t halt $1.plt
else
    for f in src/*.plt; do
        swipl -g run_tests -t halt $f
    done
fi