PRO blank_india_map

set_plot,'ps'
device,file='/home/ss901165/idl/temporary/blank_india_map.ps',set_font='Hershey'
map_set,0,0,limit=[-10,50,35,100],/cylindrical
map_continents,/hires,/countries,/coasts
map_grid,box_axes=1,/horizon

device,/close

END
