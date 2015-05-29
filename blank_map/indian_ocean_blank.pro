PRO indian_ocean_blank

; Draw a blank map of the Indian Ocean and the Asian summer monsoon
; domain.  Overlay orography.

;set_plot,'ps'
;device,file='/home/ss901165/idl/blank_map/indian_ocean_blank.ps',/color,bits_per_pixel=24,set_font='Hershey'
;map_set,0,0,limit=[-25,30,35,120],/cylindrical
;map_continents,/hires
;map_grid,/box_axes,/horizon,label=1,/no_grid,charsize=1.25,charthick=3,xthick=3,ythick=3,thick=3

orography_file='/home/ss901165/datasets/UM/qrparm.orog.n216.nc' ; UM orography at N216 resolution
box=[-20,10,50,200]
latitude=OPEN_AND_EXTRACT(orography_file,'latitude')
longitude=OPEN_AND_EXTRACT(orography_file,'longitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
orography=REFORM(OPEN_AND_EXTRACT(orography_file,'ht',offset=[box_tx(1),box_tx(0),0,0],count=[n_lon,n_lat,1,1]))
orography[where(orography eq 0)] = !Values.F_NaN
PSOPEN,font=2,file='/home/ss901165/idl/blank_map/indian_ocean_blank.ps',charsize=150,margin=1500,xoffset=400
CS,SCALE=13,NCOLS=25
MAP,latmin=-20,latmax=30,lonmin=30,lonmax=195
LEVS,MANUAL=[1,2,3,4,5,6,7,8,9,10,12,14,16,18,21,24,27,30,35,40,45,50,60]
temp=fltarr(n_lon,n_lat)
FOR i=0,n_lat-1 DO $
  FOR j=0,n_lon-1 DO $
  temp(j,i) = !Values.F_NaN
CON,FIELD=temp,X=longitude,Y=latitude,/nolinelabels,/nolines
;CON,FIELD=orography/100.,X=longitude,Y=latitude,/nolinelabels,/nolines
PSCLOSE

;device,/close

STOP
END
