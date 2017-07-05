set term png
set output 'courbe_oscillateur2.png'
set xlabel "x"
set ylabel "y"
#set xrange [-15:15]
plot 'oscillateur.txt' using 1:2 with lines
