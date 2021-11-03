#!/bin/sh

IMAGE_NAME=jun-image
echo "Saving JUN image to ${IMAGE_NAME}"

docker save -o ./../release/${IMAGE_NAME}.tar static-compile-env:latest