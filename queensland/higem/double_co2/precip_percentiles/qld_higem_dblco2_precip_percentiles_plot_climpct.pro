PRO qld_higem_dblco2_precip_percentiles_plot_climpct

; Plot a climatological percentile of daily rainfall for Queensland.
  
; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
silo_input_file='/home/ss901165/datasets_mango/SILO/n144/SILO.may-apr_dmeans.1900-2010.precip_percentiles.n144.nc'
;higem_ctl_file='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
higem_ctl_file='/net/mango/export/mango/data-10/ss901165/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
higem_2xco2_file='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip_percentiles.aus_domain.nc'

box=[-10,112,-40,154]

; Read SILO latitude and longitude
silo_longitude=OPEN_AND_EXTRACT(silo_input_file,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_input_file,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
; Get the number of latitude and longitude points
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read HiGEM latitude and longitude
higem_longitude=OPEN_AND_EXTRACT(higem_ctl_file,'longitude')
higem_latitude=OPEN_AND_EXTRACT(higem_ctl_file,'latitude')
DEFINE_BOUNDARIES,box,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
higem_nlon=N_ELEMENTS(higem_longitude)
higem_nlat=N_ELEMENTS(higem_latitude)

; Read the percentiles available in the file
percentiles=OPEN_AND_EXTRACT(silo_input_file,'percentile')
; Find the percentile that you want to plot
percentile_to_plot=90
offset_percentile=NEAREST(percentiles,percentile_to_plot)

; Read the SILO climatological percentile of daily rainfall
silo_clim_percentile=REFORM(OPEN_AND_EXTRACT(silo_input_file,'amount_at_percentile',$
                                        offset=[silo_box_tx(1),silo_box_tx(0),offset_percentile],$
                                        count=[silo_nlon,silo_nlat,1]))
IF TOTAL(where(silo_clim_percentile gt 2e19)) ge 0 THEN $
   silo_clim_percentile[where(silo_clim_percentile gt 2e19)]=!Values.F_NaN

; Reverse the SILO latitude dimension
silo_clim_percentile_revlat=fltarr(silo_nlon,silo_nlat)
FOR i=0,silo_nlat-1 DO $
   silo_clim_percentile_revlat(*,i)=silo_clim_percentile(*,silo_nlat-i-1)

; Read the HiGEM control climatological percentile of daily rainfall
higem_ctl_clim_percentile=REFORM(OPEN_AND_EXTRACT(higem_ctl_file,'amount_at_percentile',$
                                                  offset=[higem_box_tx(1),higem_box_tx(0),offset_percentile],$
                                                  count=[higem_nlon,higem_nlat,1]))
;higem_ctl_clim_percentile[where(higem_ctl_clim_percentile gt 2e19)]=!Values.F_NaN

; Read the HiGEM 2x CO2 climatological percentile of daily rainfall
higem_2xco2_clim_percentile=REFORM(OPEN_AND_EXTRACT(higem_2xco2_file,'amount_at_percentile',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),offset_percentile],$
                                                    count=[higem_nlon,higem_nlat,1]))

; Contour levels
mylevs=['6','8','10','12','14','16','18','21','24','27','30','35','40','45','50','60']
;mylevs=['12','15','18','21','24','27','30','35','40','45','50','60','70','80','90']
;mylevs=['24','30','36','42','48','54','60','70','80','90','100','120','140','160','180']
mylevs_ratio=['0.49','0.55','0.61','0.67','0.73','0.79','0.85','0.91','0.97','1.03','1.10','1.18','1.27','1.37','1.49','1.63','1.82','2.02']
;mylevs_ratio=['0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.85','4.00']
mylevs_ratio_2xco2=['0.74','0.78','0.82','0.86','0.90','0.94','0.98','1.02','1.06','1.11','1.16','1.22','1.28','1.35']
;mylevs_ratio_2xco2=['0.49','0.55','0.61','0.67','0.73','0.79','0.85','0.91','0.97','1.03','1.10','1.18','1.27','1.37','1.49','1.63','1.82','2.02']

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_plot_climpct.higem_ctl.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=1000,XOFFSET=500,YOFFSET=4500,TFONT=2,$
       TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=MIN(higem_longitude),LONMAX=MAX(higem_longitude),LATMIN=MIN(higem_latitude),LATMAX=MAX(higem_latitude),/hires;,/SET
LEVS,MANUAL=mylevs,NDECS=1
CON,FIELD=higem_ctl_clim_percentile,X=higem_longitude,Y=higem_latitude,TITLE='Ratio of rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - HiGEM ctl (150 years)',/BLOCK,/NOLINES,$
    CB_TITLE='Ratio of amount of rainfall at climatological '+STRTRIM(STRING(percentile_to_plot),1)+'th percentile'
AXES
; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_plot_climpct.higem_2xco2.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=1000,XOFFSET=500,YOFFSET=4500,TFONT=2,$
       TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=MIN(higem_longitude),LONMAX=MAX(higem_longitude),LATMIN=MIN(higem_latitude),LATMAX=MAX(higem_latitude),/hires;,/SET
LEVS,MANUAL=mylevs,NDECS=1
CON,FIELD=higem_2xco2_clim_percentile,X=higem_longitude,Y=higem_latitude,TITLE='Ratio of rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - HiGEM 2x CO2 (30 years)',/BLOCK,/NOLINES,$
    CB_TITLE='Ratio of amount of rainfall at climatological '+STRTRIM(STRING(percentile_to_plot),1)+'th percentile'
AXES
; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_plot_climpct.silo_n144.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=1000,XOFFSET=500,YOFFSET=4500,TFONT=2,$
       TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=MIN(silo_longitude),LONMAX=MAX(silo_longitude),LATMIN=MIN(silo_latitude),LATMAX=MAX(silo_latitude),/hires;,/SET
LEVS,MANUAL=mylevs,NDECS=1
CON,FIELD=silo_clim_percentile,X=silo_longitude,Y=silo_latitude,TITLE='Ratio of rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - SILO N144 (1900-2010)',/BLOCK,/NOLINES,$
    CB_TITLE='Ratio of amount of rainfall at climatological '+STRTRIM(STRING(percentile_to_plot),1)+'th percentile'
AXES
; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE,/NOVIEW

; Set the filesystem path to the PostScript file
psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_plot_climpct.higem_ctl-ratio-silo.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=1000,XOFFSET=500,YOFFSET=4500,TFONT=2,$
       TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV
MAP,LONMIN=MIN(higem_longitude),LONMAX=MAX(higem_longitude),LATMIN=MIN(higem_latitude),LATMAX=MAX(higem_latitude),/hires;,/SET
LEVS,MANUAL=mylevs_ratio,NDECS=2
CON,FIELD=higem_ctl_clim_percentile/silo_clim_percentile_revlat,X=higem_longitude,Y=higem_latitude,TITLE='Ratio of rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - HiGEM ctl divided by SILO',/BLOCK,/NOLINES,$
    CB_TITLE='Ratio of amount of rainfall at climatological '+STRTRIM(STRING(percentile_to_plot),1)+'th percentile'
AXES
; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE,/NOVIEW

; Set the filesystem path to the PostScript file
psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_plot_climpct.higem_2xco2-ratio-higem_ctl.'+$
       STRTRIM(STRING(percentile_to_plot),1)+'pct.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=1000,XOFFSET=500,YOFFSET=4500,TFONT=2,$
       TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio_2xco2)+1,/REV
MAP,LONMIN=MIN(higem_longitude),LONMAX=MAX(higem_longitude),LATMIN=MIN(higem_latitude),LATMAX=MAX(higem_latitude),/hires;,/SET
LEVS,MANUAL=mylevs_ratio_2xco2,NDECS=2
CON,FIELD=higem_2xco2_clim_percentile/higem_ctl_clim_percentile,X=higem_longitude,Y=higem_latitude,TITLE='Ratio of rainfall at climatological '+$
    STRTRIM(STRING(percentile_to_plot),1)+'th percentile - HiGEM 2x CO2 divided by HiGEM control',/BLOCK,/NOLINES,$
    CB_TITLE='Ratio of amount of rainfall at climatological '+STRTRIM(STRING(percentile_to_plot),1)+'th percentile'
AXES
; Use the following line of code to get a high-resolution map of Queensland 
; (this takes a long time to draw, though, so best to use it only for the final version of the plot)
;DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',$
;           ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
PSCLOSE


STOP
END



