#!/bin/bash

release=3.0.1
release_dir="zmim-r$release"
base="$release_dir/zmim"

mkdir -p $base
mkdir -p $base/resources
mkdir -p $base/saves
mkdir -p $base/scripts
mkdir -p $base/scripts/minizork
mkdir -p $base/src
mkdir -p $base/stories

cp ChangeLog $base
cp LICENSE $base
cp README.md $base
cp zmim.bas $base
cp zmim_cm1.bas $base
cp -R resources/* $base/resources
cp -R src/* $base/src
cp scripts/minizork/bench.scr $base/scripts/minizork
cp stories/minizork.z3 $base/stories
cp stories/sampler1.z3 $base/stories
cp stories/sampler2.z3 $base/stories
cp stories/tutorial.z3 $base/stories
cp docs/zmim.pdf $base

cd $release_dir
zip -r ../$release_dir.zip zmim
cd .
