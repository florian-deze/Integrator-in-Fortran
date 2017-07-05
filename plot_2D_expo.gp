set term png
set output 'courbe_exponentielle.png'
set xlabel "x"
set ylabel "y"
#set xrange [0:1]
plot 'exponentielle.txt' using 1:2 with lines
