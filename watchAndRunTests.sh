#!/bin/sh
dir1=./
while inotifywait -qqre modify "$dir1"; do
  sh build.sh test
done
