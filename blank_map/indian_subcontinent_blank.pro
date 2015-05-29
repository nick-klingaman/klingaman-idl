PRO indian_subcontinent_blank

; Draw a blank map of the Indian Ocean and the Asian summer monsoon
; domain.

!p.position=[0.05,0.05,0.80,0.95]

set_plot,'ps'
device,file='/home/ss901165/idl/blank_map/indian_subcontinent_blank.ps',/color,bits_per_pixel=24,set_font='Hershey'
map_set,0,0,limit=[5,60,45,100],/cylindrical
map_continents,/hires,thick=5
map_grid,/box_axes,/horizon,label=1,/no_grid,charsize=1.25,charthick=5,xthick=5,ythick=5,thick=5,$
  latdel=5,londel=10

device,/close

STOP
END
