#!/usr/bin/env bash

[[ ! -z $GUIX_ENVIRONMENT ]] && export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$GUIX_ENVIRONMENT/lib/

rlwrap sbcl --eval '(load "realtor.asd")' \
    --eval '(ql:quickload :screen-realtor)' \
    --eval '(in-package #:screen-realtor)' \
    --eval '(main (list "--repl"))'
