set term png
set output 'courbe_Arenstorf.png'
set xlabel "y2"
set ylabel "y3"
#set xrange [-15:15]
plot 'Arenstorf.txt' using 2:3 with lines
