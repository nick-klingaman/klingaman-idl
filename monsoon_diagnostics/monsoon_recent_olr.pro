PRO monsoon_recent_olr,daily_files,clim_file

; This procedure plots recent daily OLR anomalies from ECMWF analyses, 
; taken from a daily climatology (NOAA-CIRES)

n_daily = N_ELEMENTS(daily_files)
!p.position=[0.05,0.05,0.95,0.75]

FOR i=0,n_daily-1 DO BEGIN
    id = NCDF_OPEN(daily_files(i))
    olrid = NCDF_VARID(id,'TTR')
    latid = NCDF_VARID(id,'latitude')
    lonid = NCDF_VARID(id,'longitude')
    NCDF_VARGET,id,olrid,daily
    NCDF_VARGET,id,latid,ecmwf_lat
    NCDF_VARGET,id,lonid,ecmwf_lon

    temp = STRSPLIT(daily_files(i),'/',/EXTRACT)
    n_temp = N_ELEMENTS(temp)
    filename = temp(n_temp-1)
    year = fix(strmid(filename,4,4))
    month = fix(strmid(filename,8,2))
    date = fix(strmid(filename,10,2))

    julian = GREGORIAN_TO_JULIAN(date,month,year)
    
;    read_noaa_olr_climatology,clim_file,[julian,julian],clim,noaa_lat,noaa_lon,time
    n_noaa_lon = N_ELEMENTS(noaa_lon)
    n_noaa_lat = N_ELEMENTS(noaa_lat)
    climid = NCDF_OPEN(clim_file)
    climvarid = NCDF_VARID(climid,'TTR')
    NCDF_VARGET,climid,climvarid,clim
    anomalies = daily-clim
    anomalies = -anomalies/86400.
    ; Interpolate ECMWF to NOAA grid and calculate anomalies
;    anomalies = fltarr(n_noaa_lon,n_noaa_lat)
;    FOR j=0,n_noaa_lon-1 DO BEGIN
;        FOR k=0,n_noaa_lat-1 DO BEGIN
;            anomalies(j,k) = daily(NEAREST(ecmwf_lon,noaa_lon(j)),$
;                                   NEAREST(ecmwf_lat,noaa_lon(k))) - $   
;          clim(j,k)
;        ENDFOR
;    ENDFOR

    max_val = 75.
    min_val = -75.
    print,max(clim),min(clim)
    print,max(daily),min(daily)
    print,max(anomalies),min(anomalies)
    set_plot,'ps'
    device,file='monsoon_recent_olr_'+strmid(filename,4,4)+strmid(filename,8,2)+strmid(filename,10,2)+'.ps',/color,bits_per_pixel=24,set_font='Hershey'
    intervals = 25
    LoadCT,33,NColors=intervals,bottom=5
    mycolors=indgen(intervals)+5
    scale = (max_val+ABS(min_val))/FLOAT(intervals)
    mylevs = indgen(intervals)*scale+min_val
    map_set,0,0,limit=[-25,40,35,120]
    contour,anomalies,ecmwf_lon,ecmwf_lat,min_val=min_val,max_val=max_val,levels=mylevs,$
      c_colors=mycolors,/overplot,color=1,/cell_fill
    map_continents,/hires
    map_grid,/horizon,box_axes=1

    colorbar=obj_new("COLORBAR",ncolors=intervals,range=[min_val,max_val],$
                     title='OLR anomalies (W m^-2) for '+strmid(filename,4,4)+strmid(filename,8,2)+strmid(filename,10,2),$
                     bottom=5,position=[0.10,0.90,0.88,0.95])
    colorbar->draw
    device,/close
    NCDF_CLOSE,climid


ENDFOR

clim = -clim/86400.
max_val = 220.
min_val = 120.
print,clim
device,file='monsoon_recent_olr_clim.ps'
intervals = 40
LoadCT,33,NColors=intervals,bottom=5
mycolors=indgen(intervals)+5
scale = (max_val-(min_val))/FLOAT(intervals)
mylevs = indgen(intervals)*scale+min_val
map_set,0,0,limit=[-25,40,35,120]
contour,clim,ecmwf_lon,ecmwf_lat,min_val=min_val,max_val=max_val,levels=mylevs,$
  c_colors=mycolors,/overplot,color=1,/cell_fill
map_continents,/hires
map_grid,/horizon,box_axes=1

colorbar=obj_new("COLORBAR",ncolors=intervals,range=[min_val,max_val],$
                 title='OLR (W m^-2) for Aug 29 - Sep 5',$
                 bottom=5,position=[0.10,0.90,0.88,0.95])
colorbar->draw
device,/close

END
