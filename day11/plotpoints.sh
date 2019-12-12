#!/bin/bash

gnuplot -p <<EOF
set style line 1 lc rgb '#0060ad' pt 7 ps 1.5 lt 1 lw 2
set yrange[-5:0]
set xrange[0:40]
set size 1,1
plot 'points.txt' with points linestyle 1
pause 100
EOF
