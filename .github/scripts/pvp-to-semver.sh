#!/usr/bin/env bash

PVP=$(yq '.version' package.yaml)
SEMVER=$(python -c "print('.'.join('\\$PVP'.split('.')[1:]))")
echo "${PVP} -> ${SEMVER}"
yq -i e ".version |= \"$SEMVER\"" package.yaml
