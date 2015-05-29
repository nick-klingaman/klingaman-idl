PRO colorbar_only

; Minimum and maximum values for anomalies
max_val = 40.
min_val = -40.
intervals = 20
; Define and load color table
color_table = 33
bottom = 5
LOADCT,color_table,NColors=intervals,bottom=bottom
mycolors = indgen(intervals)+bottom
scale = ABS(max_val-min_val)/FLOAT(intervals)
mylevs = indgen(intervals)*scale+min_val
colorlevs = indgen(intervals/2+1)*scale*2+min_val

set_plot,'ps'
device,file='/home/ss901165/idl/blank_map/colorbar_only.ps',/color,$
  bits_per_pixel=24,set_font='Hershey'

colorbar = obj_new("COLORBAR",ncolors=intervals,range=[min_val,max_val],$
                   charsize=0.5,bottom=bottom,position=[0.10,0.93,0.88,0.98],$
                   tickv=colorlevs, major=N_ELEMENTS(colorlevs))
colorbar->draw

device,/close

STOP
END
