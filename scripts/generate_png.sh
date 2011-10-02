#!/bin/sh

files=$(find $1 -name '*.dat'|sort)

echo "Files to process: $(echo $files|wc -w)"

echo "set terminal png" > draw.gp
# echo "set yrange [1.e-15:1.]" >> draw.gp
# echo "set logscale y" >> draw.gp

for f in $files; do
    echo "set output \"$f.png\"" >> draw.gp
    echo "plot \"$f\" u 1:(abs(\$$2)) w lp" >> draw.gp
    # echo "plot \"$f\" u 0:1 w lp" >> draw.gp
done

gnuplot draw.gp
