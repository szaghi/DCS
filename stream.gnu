set size square
set title 'stream function contour levels'
set pm3d map
set contour base
set xrange [0:1]
set yrange [0:1]
set format x ""
set format y ""
set cntrparam levels 21
set pal gray
unset key
unset clabel
unset grid
splot 'DCS_out.gnu' using 1:2:3 notitle lc rgb "black"
