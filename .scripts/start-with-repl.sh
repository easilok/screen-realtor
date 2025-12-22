#!/usr/bin/env bash

[[ ! -z $GUIX_ENVIRONMENT ]] && export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GUIX_ENVIRONMENT/lib/

rlwrap sbcl --eval '(load "displayer.asd")' \
    --eval '(ql:quickload :lp-displayer)' \
    --eval '(in-package #:lp-displayer)' \
    --eval '(main (list "--repl"))'
