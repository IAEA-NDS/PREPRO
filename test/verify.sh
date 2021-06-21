#!/bin/sh
PATH="$(pwd)/../bin:$PATH"

mkdir -p output
cp input/* output
cd output

endf2c
linear
recent
rm LINEAR.OUT
sigma1
rm RECENT.OUT
activate
rm SIGMA1.OUT
legend
rm ACTIVATE.OUT
fixup
rm LEGEND.OUT
dictin
rm FIXUP.OUT
groupie
mixer
virgin
evalplot
complot
evalhard1
mv PLOT0001.ps evalhard1.ps
comhard1
mv PLOT0001.ps comhard1.ps
