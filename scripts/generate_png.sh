#!/bin/sh

echo "set terminal png" > draw.gp
echo "set yrange [-2:2]" >> draw.gp

for f in $(find $1 -name '*.dat'|sort); do
    echo "$cmd set output \"$f.png\"" >> draw.gp
    echo "$cmd plot \"$f\" u 1:2 w l" >> draw.gp
done

gnuplot draw.gp
