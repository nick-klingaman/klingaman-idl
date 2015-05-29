PRO kpp_forced_weight_test

; Demonstrate that our simple cosine-like coupling weights work
weight_file='/home/ss901165/datasets/NCOF_OCEAN/kpp_cplwght_n144.nc'

; Get longitude, latitude, weights
longitude = OPEN_AND_EXTRACT(weight_file,'longitude')
latitude = OPEN_AND_EXTRACT(weight_file,'latitude')
n_lat = N_ELEMENTS(latitude)
n_lon = N_ELEMENTS(longitude)
alpha = OPEN_AND_EXTRACT(weight_file,'alpha')

; Plot
set_plot,'ps'
device,file='/home/ss901165/kpp_ocean/forced_weight_test/plot_weights.ps',/color,bits_per_pixel=24,/times,/cmyk
min_val=0.
max_val=1.
mylevs=[min_val,indgen(9)*0.1+0.1,max_val]
intervals=N_ELEMENTS(mylevs)-1
bottom=1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),1,4)," "]
!p.position=[0.05,0.05,0.95,0.80]

map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,alpha,longitude,latitude,/overplot,/fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,thick=3,charthick=3,charsize=1.25,ythick=3,xthick=3

colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Coupling weight for KPP forced_weight_test',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charthick=3,charsize=1.25

device,/close

; Take difference between mean weighted SSTs and mean unweighted SSTs
; Use SSTs from simulation with 50-day relaxation to climatology for
; unweighted SSTs.

kpp_weighted = '/home/ss901165/kpp_ocean/forced_weight_test/KPPocean.T.nc'
kpp_unweighted = '/home/ss901165/kpp_ocean/forced_lsmask_relax/KPPocean.T.nc'

latitude = OPEN_AND_EXTRACT(kpp_weighted,'latitude')
longitude = OPEN_AND_EXTRACT(kpp_weighted,'longitude')
n_lat = N_ELEMENTS(latitude)
n_lon = N_ELEMENTS(longitude)
weighted_ssts = REFORM(OPEN_AND_EXTRACT(kpp_weighted,'T',offset=[0,0,0,0],count=[n_lon,n_lat,1,1200]))
unweighted_ssts = REFORM(OPEN_AND_EXTRACT(kpp_unweighted,'T',offset=[0,0,0,0],count=[n_lon,n_lat,1,1200]))

weighted_mean = fltarr(n_lon,n_lat)
unweighted_mean = fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
    FOR j=0,n_lat-1 DO BEGIN
        weighted_mean(i,j) = MEAN(weighted_ssts(i,j,*))
        unweighted_mean(i,j) = MEAN(unweighted_ssts(i,j,*))
    ENDFOR
ENDFOR

device,file='/home/ss901165/idl/kpp_forced/forced_weight_test/kpp_forced_weight_test_mean_diff_weight_noweight.ps',$
  /color,bits_per_pixel=24,/times,/cmyk

min_val=-100.
max_val=100.
mylevs=[min_val,indgen(21)-10,max_val]
intervals=N_ELEMENTS(mylevs)-1
bottom=1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),2,4)," "]
!p.position=[0.05,0.05,0.95,0.80]

map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,weighted_mean-unweighted_mean,longitude,latitude,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in mean MJJAS SSTs for WEIGHT - NO_WEIGHT',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3

device,/close

STOP

END
