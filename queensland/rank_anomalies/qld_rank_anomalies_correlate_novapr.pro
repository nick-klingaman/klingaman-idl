PRO qld_rank_anomalies_correlate_novapr

; File containing timeseries of Nov-Apr mean rainfall
silo_seasmean_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smeans.1900-2008.0.25x0.25.nc'
; File containing mean Nov-Apr rainfall
silo_seasmean_clim_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.nov-apr_smean_clim.1900-2008.0.25x0.25.nc'

silo_nyears=108

; Box to consider for area-averaging
box_aavg=[-10,140,-30,154]
; Box to consider for plotting
box_plot=[-10,140,-30,154]

; Read latitude and longitude, get number of points
silo_longitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_seasmean_infile,'latitude')
DEFINE_BOUNDARIES,box_aavg,silo_latitude,silo_longitude,silo_box_aavg_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Read climatological mean rainfall
silo_clim_precip=REFORM(OPEN_AND_EXTRACT(silo_seasmean_clim_infile,'rain',$
                                         offset=[silo_box_aavg_tx(1),silo_box_aavg_tx(0)],$
                                         count=[silo_nlon,silo_nlat]))
silo_clim_precip[where(silo_clim_precip ge 1E20)]=!Values.F_NaN
silo_clim_precip_aavg=MEAN(silo_clim_precip,/NaN)

; Read timeseries of mean rainfall, take area-average
silo_precip=REFORM(OPEN_AND_EXTRACT(silo_seasmean_infile,'rain',$
                                    offset=[silo_box_aavg_tx(1),silo_box_aavg_tx(0),0],$
                                    count=[silo_nlon,silo_nlat,silo_nyears]))
silo_precip[where(silo_precip ge 1E20)]=!Values.F_NaN
silo_precip_aavg=fltarr(silo_nyears)
silo_precip_aavg_anomaly=fltarr(silo_nyears)

FOR i=0,silo_nyears-1 DO BEGIN
   silo_precip_aavg(i)=MEAN(silo_precip(*,*,i),/NaN)
   silo_precip_aavg_anomaly(i)=silo_precip_aavg(i)-silo_clim_precip_aavg
ENDFOR

silo_aavg_sorted_indices=SORT(silo_precip_aavg_anomaly)
silo_aavg_ranks=fltarr(silo_nyears)
silo_aavg_ranks[silo_aavg_sorted_indices]=indgen(silo_nyears)
silo_rank_correlation=fltarr(silo_nlon,silo_nlat)
silo_thispt_ranks=fltarr(silo_nyears)
FOR j=0,silo_nlon-1 DO BEGIN
   FOR k=0,silo_nlat-1 DO BEGIN
      IF silo_precip(j,k,0) le 1E20 THEN BEGIN
         silo_thispt_anomalies=silo_precip(j,k,*)-silo_clim_precip(j,k)
         silo_thispt_sorted_indices=SORT(silo_thispt_anomalies)
         silo_thispt_ranks[silo_thispt_sorted_indices]=indgen(silo_nyears)
         silo_rank_correlation(j,k)=CORRELATE(silo_thispt_ranks,silo_aavg_ranks)
      ENDIF ELSE $
         silo_rank_correlation(j,k)=!Values.F_NaN
   ENDFOR
ENDFOR

mylevs_corr=['0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70','0.75','0.80','0.85']
psfile='/home/ss901165/idl/queensland/rank_anomalies/qld_rank_anomalies_correlate_novapr.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_corr)+1,/REV;,white=[7]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(2),LATMAX=box_plot(0),/HIRES
LEVS,MANUAL=mylevs_corr
CON,FIELD=silo_rank_correlation,X=silo_longitude,Y=silo_latitude,$
    TITLE="Rank corr between pt and aavg rain anomaly ("+$
    STRTRIM(STRING(ABS(box_aavg(0))),1)+"-"+STRTRIM(STRING(ABS(box_aavg(2))),1)+"S, "+$
    STRTRIM(STRING(box_aavg(1)),1)+"-"+STRTRIM(STRING(box_aavg(3)),1)+"E) - Nov-Apr - SILO 0.25x0.25",$
    /BLOCK,/CB_RIGHT,/NOLINES

PSCLOSE

STOP

END

