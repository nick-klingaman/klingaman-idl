PRO qld_iacorr_bypoint_silo025_acorr

; SILO 0.25x0.25 annual-mean rainfall timeseries
silo_ameans_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.may-apr_ameans.1900-2008.0.25x0.25.nc'
silo_nyears=108
; Region masks
silo_regions_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_qld_region_mask.0.25x0.25.may-apr.nc'
silo_nregions=N_ELEMENTS(OPEN_AND_EXTRACT(silo_regions_infile,'region'))

; Read regions latitude and longitude
silo_regions_latitude=OPEN_AND_EXTRACT(silo_regions_infile,'latitude')
silo_regions_longitude=OPEN_AND_EXTRACT(silo_regions_infile,'longitude')
silo_regions_nlon=N_ELEMENTS(silo_regions_longitude)
silo_regions_nlat=N_ELEMENTS(silo_regions_latitude)

; Read precip latitude and longitude, restrict to QLD box
silo_ameans_latitude=OPEN_AND_EXTRACT(silo_ameans_infile,'latitude')
silo_ameans_longitude=OPEN_AND_EXTRACT(silo_ameans_infile,'longitude')
DEFINE_BOUNDARIES,[silo_regions_latitude(0),silo_regions_longitude(0),$
                   silo_regions_latitude(silo_regions_nlat-1),silo_regions_longitude(silo_regions_nlon-1)],$
                  silo_ameans_latitude,silo_ameans_longitude,qld_box_tx,/LIMIT



; Read precipitation data
silo_ameans_precip=REFORM(OPEN_AND_EXTRACT(silo_ameans_infile,'rain',$
                                           offset=[qld_box_tx(1),qld_box_tx(0),0],$
                                           count=[silo_regions_nlon,silo_regions_nlat,silo_nyears]))

silo_aavg_weights=fltarr(silo_regions_nlat)
FOR i=0,silo_regions_nlat-1 DO $
   silo_aavg_weights(i)=COS(3.14159*silo_regions_latitude(i)/180.)
silo_aavg_weights=silo_aavg_weights/TOTAL(silo_aavg_weights)

n_lags=40
lags=indgen(n_lags)+1
silo_ameans_precip_region_aavg=fltarr(silo_nregions,silo_nyears)
silo_ameans_precip_region_aavg_acorr=fltarr(silo_nregions,n_lags)
silo_ameans_precip_region_acorr_stddev=fltarr(silo_nregions,n_lags)
silo_ameans_precip_region_acorr_mean=fltarr(silo_nregions,n_lags)
FOR i=0,silo_nregions-1 DO BEGIN
   ; Read region mask for this region
   this_region_mask=OPEN_AND_EXTRACT(silo_regions_infile,'mask_region'+STRTRIM(STRING(i+1),1))
   ; Mask annual-mean rainfall timeseries
   silo_ameans_precip_masked=fltarr(silo_regions_nlon,silo_regions_nlat,silo_nyears)
   silo_ameans_precip_masked_clim=fltarr(silo_nyears)
   silo_ameans_precip_masked_acorr=fltarr(silo_regions_nlon,silo_regions_nlat,n_lags)
   FOR j=0,silo_nyears-1 DO BEGIN
      temp=REFORM(silo_ameans_precip(*,*,j))
      temp[where(this_region_mask eq 0)]=!Values.F_NaN
      silo_ameans_precip_masked(*,*,j)=temp
      FOR k=0,silo_regions_nlat-1 DO $
         silo_ameans_precip_region_aavg(i,j)=silo_ameans_precip_region_aavg(i,j)+$
         TOTAL(silo_aavg_weights*REFORM(temp(*,k)),/NaN)*1./FLOAT(silo_regions_nlon)
   ENDFOR
   silo_ameans_precip_region_aavg_acorr(i,*)=A_CORRELATE(silo_ameans_precip_region_aavg(i,*),lags)
   FOR j=0,silo_regions_nlon-1 DO BEGIN
      FOR k=0,silo_regions_nlat-1 DO BEGIN
;         silo_ameans_precip_masked_clim(*)=MEAN(silo_ameans_precip_masked(j,k,*),/NaN)*365.
         silo_ameans_precip_masked_acorr(j,k,*)=A_CORRELATE(REFORM(silo_ameans_precip_masked(j,k,*)),lags)
      ENDFOR
   ENDFOR
   FOR j=0,n_lags-1 DO BEGIN
      silo_ameans_precip_region_acorr_stddev(i,j)=STDDEV(silo_ameans_precip_masked_acorr(*,*,j),/NaN)      
      silo_ameans_precip_region_acorr_mean(i,j)=MEAN(silo_ameans_precip_masked_acorr(*,*,j),/NaN)
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silo025_statistics.aavg_acorr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=120
GSET,XMIN=MIN(lags),XMAX=MAX(lags)+1,YMIN=-0.3,YMAX=0.3
CS,SCALE=26,NCOLS=silo_nregions+1,/REV
FOR i=0,silo_nregions-1 DO BEGIN
   GPLOT,X=lags,Y=REFORM(silo_ameans_precip_region_aavg_acorr(i,*)),COL=i+2
ENDFOR
black=FSC_COLOR("black",10)
GPLOT,X=lags,Y=REPLICATE(0,n_lags),COL=10,STYLE=2
GPLOT,X=lags,Y=findgen(n_lags)*0.002+0.195,COL=10,STYLE=1
GPLOT,X=lags,Y=findgen(n_lags)*(-0.002)-0.195,COL=10,STYLE=1
AXES,XSTEP=1,YSTEP=0.1,NDECS=2
PSCLOSE,/NOVIEW

windows=[10,20,30,40,50,60,70,80]
signif_levels=[0.602,0.433,0.355,0.304,0.273,0.25,0.232,0.217]
n_windows=N_ELEMENTS(windows)
items=strarr(n_windows)
silo_ameans_precip_region_aavg_acorr_window=fltarr(silo_nregions,n_windows,silo_nyears)
silo_ameans_precip_region_aavg_acorr_window(*,*,*)=!Values.F_NaN
FOR i=0,n_windows-1 DO BEGIN
   items(i)=STRTRIM(STRING(windows(i)+1),1)+'-year window'
   FOR j=0,silo_nregions-1 DO BEGIN
      FOR k=windows(i)/2,silo_nyears-windows(i)/2-1 DO BEGIN
         silo_ameans_precip_region_aavg_acorr_window(j,i,k)=$
            A_CORRELATE(silo_ameans_precip_region_aavg(j,k-windows(i)/2:k+windows(i)/2),1)
      ENDFOR
   ENDFOR
ENDFOR

FOR j=0,silo_nregions-1 DO BEGIN
   psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silo025_statistics.aavg_acorr_windows_region'+STRTRIM(STRING(j+1),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=300,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=120
   GSET,XMIN=1900,XMAX=2008,YMIN=-0.8,YMAX=0.8
   CS,SCALE=26,NCOLS=n_windows,/REV
   FOR i=0,n_windows-1 DO BEGIN
      GPLOT,X=indgen(silo_nyears)+1900,Y=REFORM(silo_ameans_precip_region_aavg_acorr_window(j,i,*)),COL=i+2
      GPLOT,X=indgen(silo_nyears)+1900,Y=REPLICATE(signif_levels(i),silo_nyears-windows(i)/2+1900),COL=i+2,STYLE=1,THICK=150
      GPLOT,X=indgen(silo_nyears)+1900,Y=REPLICATE(-signif_levels(i),silo_nyears-windows(i)/2+1900),COL=i+2,STYLE=1,THICK=150
   ENDFOR
   black=FSC_COLOR("black",10)
   GPLOT,X=indgen(silo_nyears)+1900,Y=REPLICATE(0,silo_nyears),COL=10,STYLE=2
   LEGEND,labels=REVERSE(items),COL=REVERSE(indgen(n_windows)+2),LEGPOS=9
;GPLOT,X=lags,Y=findgen(n_lags)*0.002+0.195,COL=10,STYLE=1
;GPLOT,X=lags,Y=findgen(n_lags)*(-0.002)-0.195,COL=10,STYLE=1
   AXES,XSTEP=10,YSTEP=0.1,NDECS=2,XTITLE='Year at centre of winow',YTITLE='Correlation across window'
   PSCLOSE
ENDFOR

STOP
END
