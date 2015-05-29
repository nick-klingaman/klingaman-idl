PRO kpp_compare_lsmask_relax_sst

; Compare surface temperatures in the forced KPP runs with and without
; relaxation to OSTIA monthly mean SSTs for 2005.

norelax_output='/home/ss901165/kpp_ocean/forced_lsmask_test/KPPocean.T.dmean.nc'
relax_output='/home/ss901165/kpp_ocean/forced_lsmask_relax/KPPocean.T.dmean.nc'
reference_file='/home/ss901165/datasets/GHRSST/FOR_KPP/May-Sep_2005_n144_for_kpp_mmean.nc'

points=[[21.666,6.666],[143.75,102.5]]

kpp_latitudes = OPEN_AND_EXTRACT(norelax_output,'latitude')
kpp_longitudes = OPEN_AND_EXTRACT(norelax_output,'longitude')
n_kpplat = N_ELEMENTS(kpp_latitudes)
n_kpplon = N_ELEMENTS(kpp_longitudes)
ostia_latitudes = OPEN_AND_EXTRACT(reference_file,'latitude')
ostia_longitudes = OPEN_AND_EXTRACT(reference_file,'longitude')
n_ostialat = N_ELEMENTS(ostia_latitudes)
n_ostialon = N_ELEMENTS(ostia_longitudes)

; Timeseries
FOR i=0,N_ELEMENTS(points(*,0))-1 DO BEGIN
    this_lat_kpp = NEAREST(kpp_latitudes,points(i,0))
    this_lon_kpp = NEAREST(kpp_longitudes,points(i,1))
    this_lat_ostia = NEAREST(ostia_latitudes,points(i,0))
    this_lon_ostia = NEAREST(ostia_longitudes,points(i,1))
    norelax_sst = OPEN_AND_EXTRACT(norelax_output,'T',offset=[this_lon_kpp,this_lat_kpp,0,0],count=[1,1,1,150])
    relax_sst = OPEN_AND_EXTRACT(relax_output,'T',offset=[this_lon_kpp,this_lat_kpp,0,0],count=[1,1,1,150])
    reference_sst = OPEN_AND_EXTRACT(reference_file,'sst',offset=[this_lon_ostia,this_lat_ostia,0],count=[1,1,150])
    reference_sst = reference_sst-273.15

    diff = relax_sst-norelax_sst
    n_days = N_ELEMENTS(reference_sst)
    reference_sst_3hr = fltarr(8*n_days)
    FOR j=0,n_days-1 DO $
      reference_sst_3hr(j*8:(j+1)*8-1) = REPLICATE(reference_sst(j),8)
    
    set_plot,'ps'
    device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_point'+STRTRIM(STRING(i),1)+'.ps',bits_per_pixel=24,color=1,/times,/cmyk
    LoadCT,33,ncolors=16,bottom=3
    time = findgen(n_days)
    plot,time,relax_sst,yrange=[25.0,47.0],xrange=[0,150],ystyle=9,xstyle=1,$
      xtitle='Days since 1 May',ytitle='Temperature (degC)',$
      title='Surface temperatures for point ('+STRTRIM(STRING(points(i,0)),1)+','+$
      STRTRIM(STRING(points(i,1)),1)+')',color=5,linestyle=2
    oplot,time,norelax_sst,color=8,linestyle=3
    oplot,time,reference_sst_3hr,color=10,linestyle=4,thick=2
    AXIS,YAXIS=1,YRANGE=[-10,2],YTITLE='Effect of relaxation (degC)',/SAVE
    oplot,time,diff,color=15,linestyle=5
    items=['RELAX','NO_RELAX','REFERENCE','(RELAX-NO_RELAX)']
    legend,items,linestyle=[2,3,4,5],color=[5,8,10,15],/NORMAL,/BOTTOM,/RIGHT
    device,/close
ENDFOR

; Get only those OSTIA SSTs corresponding to the KPP domain
start_kpp_lat = MIN([NEAREST(ostia_latitudes,kpp_latitudes(0)),NEAREST(ostia_latitudes,kpp_latitudes(n_kpplat-1))])
stop_kpp_lat = MAX([NEAREST(ostia_latitudes,kpp_latitudes(0)),NEAREST(ostia_latitudes,kpp_latitudes(n_kpplat-1))])
start_kpp_lon = MIN([NEAREST(ostia_longitudes,kpp_longitudes(0)),NEAREST(ostia_longitudes,kpp_longitudes(n_kpplon-1))])
stop_kpp_lon = MAX([NEAREST(ostia_longitudes,kpp_longitudes(0)),NEAREST(ostia_longitudes,kpp_longitudes(n_kpplon-1))])

; Maps of mean SSTS
min_val=0.
max_val=100.
mylevs=[min_val,indgen(16)+20,max_val]
intervals=N_ELEMENTS(mylevs)-1
bottom=1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),2,4)," "]
!p.position=[0.05,0.05,0.95,0.80]

; Get mean SSTs
relax_ssts = REFORM(OPEN_AND_EXTRACT(relax_output,'T',offset=[0,0,0,0],count=[n_kpplon,n_kpplat,1,150]))
norelax_ssts = REFORM(OPEN_AND_EXTRACT(norelax_output,'T',offset=[0,0,0,0],count=[n_kpplon,n_kpplat,1,150]))
reference_ssts = OPEN_AND_EXTRACT(reference_file,'sst')
mean_relax = fltarr(n_kpplon,n_kpplat)
mean_norelax = fltarr(n_kpplon,n_kpplat)
mean_reference = fltarr(n_ostialon,n_ostialat)
FOR i=0,n_kpplon-1 DO BEGIN
    FOR j=0,n_kpplat-1 DO BEGIN
        mean_relax(i,j) = MEAN(relax_ssts(i,j,*))
        mean_norelax(i,j) = MEAN(norelax_ssts(i,j,*))        
    ENDFOR
ENDFOR
FOR i=0,n_ostialon-1 DO BEGIN
    FOR j=0,n_ostialat-1 DO BEGIN
        mean_reference(i,j) = MEAN(reference_ssts(i,j,*))-273.15
    ENDFOR
ENDFOR

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_relax.ps',bits_per_pixel=8,color=1,/times,/cmyk,$
  /ENCAPSULATED,LANGUAGE_LEVEL=2
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,mean_relax,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
map_continents,/hires
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Mean SSTs for MJJAS - RELAX (50 DAY)',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_norelax.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,mean_norelax,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Mean SSTs for MJJAS - NO_RELAX',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

mask=OPEN_AND_EXTRACT('/home/ss901165/um_output/mask_n144.nc','lsm')
mean_reference[where(mask eq 1)] = !Values.F_NaN
device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,mean_reference,ostia_longitudes,ostia_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Mean SSTs for MJJAS - Reference (OSTIA)',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; Maps of the last day of the integration
mylevs=[min_val,indgen(20)+20,max_val]
intervals=N_ELEMENTS(mylevs)-1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),2,4)," "]

lastday_norelax = OPEN_AND_EXTRACT(norelax_output,'T',offset=[0,0,0,149],count=[n_kpplon,n_kpplat,1,1])
lastday_relax = OPEN_AND_EXTRACT(relax_output,'T',offset=[0,0,0,149],count=[n_kpplon,n_kpplat,1,1])
lastday_reference = OPEN_AND_EXTRACT(reference_file,'sst',offset=[0,0,149],count=[n_ostialon,n_ostialat,1])

lastday_norelax_mean = fltarr(n_kpplon,n_kpplat)
lastday_relax_mean = fltarr(n_kpplon,n_kpplat)
FOR i=0,n_kpplon-1 DO BEGIN
    FOR j=0,n_kpplat-1 DO BEGIN
        lastday_norelax_mean(i,j) = MEAN(lastday_norelax(i,j,*))
        lastday_relax_mean(i,j) = MEAN(lastday_relax(i,j,*))
    ENDFOR
ENDFOR

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_norelax.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,lastday_norelax_mean,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Daily mean of last day of simulation (30 Sep) for no_relax',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; For RELAX (50 day) simulation

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_relax.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,lastday_relax_mean,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Daily mean of last day of simulation (30 Sep) for relax (50 day)',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; For reference SSTs (GHRSST)

lastday_reference[where(mask eq 1)] = !Values.F_NaN

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,lastday_reference-273.15,ostia_longitudes,ostia_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Daily mean of last day of simulation (30 Sep) for relax (50 day)',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; For difference in last day SSTs (RELAX - NO RELAX)
min_val = -100.
mylevs = [min_val,indgen(17)-8,max_val]
intervals=N_ELEMENTS(mylevs)-1
bottom=1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals+4,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),2,4)," "]
white = FSC_COLOR("white",bottom+8)
white = FSC_COLOR("white",bottom+9)

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_diff_relax_norelax.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
contour,lastday_relax_mean-lastday_norelax_mean,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in daily mean (30 Sep) for RELAX - NO_RELAX',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; For difference in last day SSTs (RELAX - REFERENCE)

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_diff_relax_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
diff = lastday_relax_mean-lastday_reference(start_kpp_lon:stop_kpp_lon,start_kpp_lat:stop_kpp_lat)+273.15
contour,diff,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in daily mean (30 Sep) for RELAX - REFERENCE',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_lastday_diff_norelax_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
diff = lastday_norelax_mean-lastday_reference(start_kpp_lon:stop_kpp_lon,start_kpp_lat:stop_kpp_lat)+273.15
contour,diff,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in daily mean (30 Sep) for NO_RELAX - REFERENCE',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

; Maps of differences in means

mylevs = [min_val,indgen(13)-6,max_val]
intervals=N_ELEMENTS(mylevs)-1
bottom=1
mycolors=indgen(intervals)+bottom
LoadCT,33,ncolors=intervals,bottom=bottom
ticknames=[" ",LEVS2STRING(mylevs(1:intervals-1),1,4)," "]
white = FSC_COLOR("white",bottom+6)
white = FSC_COLOR("white",bottom+7)

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_diff_norelax_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
diff = mean_norelax-mean_reference(start_kpp_lon:stop_kpp_lon,start_kpp_lat:stop_kpp_lat)
contour,diff,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in mean MJJAS SSTs for NO_RELAX - REFERENCE',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_diff_relax_ref.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
diff = mean_relax-mean_reference(start_kpp_lon:stop_kpp_lon,start_kpp_lat:stop_kpp_lat)
contour,diff,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in mean MJJAS SSTs for RELAX - REFERENCE',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close

device,file='/home/ss901165/idl/kpp_forced/forced_lsmask_relax/kpp_compare_lsmask_relax_map_mean_diff_relax_norelax.ps',bits_per_pixel=24,color=1,/times,/cmyk
map_set,0,0,limit=[-30,20,30,180],/cylindrical
diff = mean_relax-mean_norelax
contour,diff,kpp_longitudes,kpp_latitudes,/overplot,/cell_fill,c_colors=mycolors,$
  levels=mylevs,color=1,min_val=min_val,max_val=max_val,xthick=3,ythick=3,thick=3,c_thick=3
map_continents,/hires
map_grid,/box_axes,/horizon,/no_grid,latdel=10,londel=10,ythick=3,xthick=3,thick=3,charthick=3,charsize=1.25
colorbar,ncolors=intervals,bottom=bottom,divisions=intervals,$
  title='Diff in mean MJJAS SSTs for RELAX - NO_RELAX',$
  position=[0.05,0.90,0.95,0.93],ticknames=ticknames,thick=3,charsize=1.25,charthick=3
device,/close


STOP

END
