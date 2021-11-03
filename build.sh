#!/bin/bash

mkdir -p ./release

stack sdist --tar-dir ./release

docker build ./ -t static-compile-env

docker run -it -d --name="build_container" static-compile-env:latest
docker cp build_container:/usr/jun/jun-0.1.0.0/jun-release ./release

docker stop build_container