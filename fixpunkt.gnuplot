set style line 1 linecolor rgb '#000000' linetype 1 linewidth 2
set style line 2 linecolor rgb '#9600ff' linetype 1 linewidth 4
set style line 3 linecolor rgb '#4444ff' linetype 1 linewidth 4

unset border
set grid
set xtics ("0" 0, "1" 1)
set ytics ("0" 0, "1" 1)
set xrange [ 0 : 1.9 ]
set yrange [ 0 : 3 ]
set xzeroaxis ls 1 lw 3
set yzeroaxis ls 1 lw 3
set samples 10000

set terminal pdf size 10cm, 4cm dashed
set output "fixpunkt.pdf"

plot sqrt(x) w l ls 2, x w l ls 3
