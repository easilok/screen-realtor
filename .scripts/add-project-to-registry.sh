#!/usr/bin/env bash

REGISTRY_PATH=$HOME/.config/common-lisp/source-registry.conf.d

mkdir -p "$REGISTRY_PATH"

echo '(:tree (:home "projects/commonlisp/"))' >> "$REGISTRY_PATH/projects.conf"
