PRO qld_wet_days_percentiles_plot_climpct

; Plot a climatological percentile of daily rainfall for Queensland.
  
; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
input_file='/home/ss901165/datasets_mango/SILO/t62/SILO.may-apr_dmeans.1958-2000.precip_percentiles.t62.nc'
;input_file='/home/ss901165/datasets_mango/SILO/era40_resolution/SILO.may-apr_dmeans.1958-2000.precip_percentiles.era40.nc'
;input_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.nc'
;input_file='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.t62_gauss.nc'
;input_file='/home/ss901165/datasets/ERA40/PRECIP/era40.may-apr_dmeans.1958-2000.precip_percentiles.aus_domain.nc'


; Read latitude and longitude from the file
longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
; Get the number of latitude and longitude points
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Read the percentiles available in the file
percentiles=OPEN_AND_EXTRACT(input_file,'percentile')
; Find the percentile that you want to plot
percentile_to_plot=99
offset_percentile=NEAREST(percentiles,percentile_to_plot)

; Read the climatological percentile of daily rainfall
clim_percentile=REFORM(OPEN_AND_EXTRACT(input_file,'amount_at_percentile',$
                                        offset=[0,0,offset_percentile],$
                                        count=[n_lon,n_lat,1]))
clim_percentile[where(clim_percentile eq 2e20)]=!Values.F_NaN

; Contour levels
mylevs=['6','8','10','12','14','16','18','20','22','24','26']

; Set the filesystem path to the PostScript file
psfile='/home/ss901165/idl/queensland/qld_wet_days_percentiles_plot_climpct.ERA40'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=100,MARGIN=4000,SPACE2=200,XOFFSET=500,YOFFSET=1500,TFONT=6,$
       TCHARSIZE=80,CB_WIDTH=100,/PORTRAIT
CS,SCALE=21,NCOLS=N_ELEMENTS(mylevs)+1,/REV

; To use the high-resolution map of Queensland, add the /SET option to MAP and 
; un-comment the line of code as indicated below.
MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude),/hires;,/SET
LEVS,MANUAL=mylevs
CON,FIELD=clim_percentile,X=longitude,Y=latitude,TITLE='Rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - ERA-40 REANALYSIS 0.25 data - 1958-2000',/BLOCK,/NOLINES,$
    CB_TITLE='Precipitation (mm day!U-1!N)'
AXES

; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE

STOP
END



