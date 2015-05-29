PRO qld_wet_days_percentiles_plot_climpct

; Plot a climatological percentile of daily rainfall for Queensland.
  
; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
input_file='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip_percentiles.may-apr_dmeans.1900-2008.0.25x0.25.nc'

; Read latitude and longitude from the file
longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
; Get the number of latitude and longitude points
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Read the percentiles available in the file
percentiles=OPEN_AND_EXTRACT(input_file,'percentile')
; Find the percentile that you want to plot
percentile_to_plot=95
offset_percentile=NEAREST(percentiles,percentile_to_plot)

; Read the climatological percentile of daily rainfall
clim_percentile=REFORM(OPEN_AND_EXTRACT(input_file,'amount_at_percentile',$
                                        offset=[0,0,offset_percentile],$
                                        count=[n_lon,n_lat,1]))

; Contour levels
mylevs=['15','18','21','24','27','30','33','36','39','42','45','48','51','54']

; Set the filesystem path to the PostScript file
psfile='/home/ss901165/idl/queensland/wet_days/percentiles/qld_wet_days_percentiles_plot_climpct.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=1500,TFONT=2,$
       TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV

; To use the high-resolution map of Queensland, add the /SET option to MAP and 
; un-comment the line of code as indicated below.
MAP,LONMIN=MIN(longitude),LONMAX=MAX(longitude),LATMIN=MIN(latitude),LATMAX=MAX(latitude),/hires;,/SET
LEVS,MANUAL=mylevs
CON,FIELD=clim_percentile,X=longitude,Y=latitude,TITLE='Rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - SILO 0.25 data - 1900-2008',/BLOCK,/NOLINES,$
    CB_TITLE='Precipitation (mm day!U-1!N)'
AXES

; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE

STOP
END

