set term png
set output 'courbe_hodgkin.png'
set xlabel "x"
set ylabel "y"
#set xrange [0:60]
plot 'hodgkin.txt' using 1:2 with lines
