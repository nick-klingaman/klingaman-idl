PRO twentyc_everymember_compare_silo_boxavg
  
; Give the file containing the 20th Century Reanalysis every-member data
twentyc_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/every_member/precip/20thc_reanalysis_allmembers.may-apr_ameans.1900-2007.precip.nc'

; Give the file containing the SILO precipitation on the 20CR grid
silo_infile='/home/ss901165/datasets_mango/SILO/t62/SILO_precip.may-apr_ameans.1900-2007.t62.nc'

; Land/sea mask for the 20CR
twentyc_mask_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/mask_t62_revlat.nc'

; Define the box over which to area-average
box=[-30,138,-10,154]

; Note that we need to read both the SILO and 20CR grids, because the 20CR every-member files are
; ordered south -> north, while the 20CR ensemble-mean files (on which the SILO files are based) are
; ordered north -> south.  Don't ask me why ...

; Read SILO longitude and latitude
silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

; Now read the 20CR every-member grid
twentyc_longitude=OPEN_AND_EXTRACT(twentyc_infile,'lon')
twentyc_latitude=OPEN_AND_EXTRACT(twentyc_infile,'lat')
DEFINE_BOUNDARIES,box,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
twentyc_nlon=N_ELEMENTS(twentyc_longitude)
twentyc_nlat=N_ELEMENTS(twentyc_latitude)

; Number of ensemble members in 20CR
twentyc_nmembers=56
; Start year and total number of years
start_year=1900
n_years=108

; Read SILO precipitation
silo_precip=OPEN_AND_EXTRACT(silo_infile,'rain',$
                             offset=[silo_box_tx(1),silo_box_tx(0),0],$
                             count=[silo_nlon,silo_nlat,n_years])

; Read 20CR every-member precipitation
twentyc_precip=OPEN_AND_EXTRACT(twentyc_infile,'prate',$
                                offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                count=[twentyc_nlon,twentyc_nlat,twentyc_nmembers,n_years])*86400.

; Read 20CR land/sea mask
twentyc_mask=OPEN_AND_EXTRACT(twentyc_mask_file,'lsm',$
                              offset=[twentyc_box_tx(1),twentyc_box_tx(0)],$
                              count=[twentyc_nlon,twentyc_nlat])

; Construct area averages
silo_precip_aavg=fltarr(n_years)
twentyc_precip_aavg=fltarr(twentyc_nmembers,n_years)

FOR i=0,n_years-1 DO BEGIN
   temp=REFORM(silo_precip(*,*,i))
   temp[where(twentyc_mask eq 0)]=!Values.F_NaN
   temp[where(temp gt 2e19)]=!Values.F_NaN
   silo_precip_aavg(i)=MEAN(temp,/NaN)
   FOR j=0,twentyc_nmembers-1 DO BEGIN
      temp=REFORM(twentyc_precip(*,*,j,i))
      temp[where(twentyc_mask eq 0)]=!Values.F_NaN
      twentyc_precip_aavg(j,i)=MEAN(temp,/NaN)
   ENDFOR
ENDFOR

; Construct anomalies from climatology
silo_precip_aavg_anom=silo_precip_aavg-MEAN(silo_precip_aavg)
twentyc_precip_aavg_anom=fltarr(twentyc_nmembers,n_years)
FOR i=0,twentyc_nmembers-1 DO $
   twentyc_precip_aavg_anom(i,*)=twentyc_precip_aavg(i,*)-MEAN(twentyc_precip_aavg(i,*))

; Bin the 20CR anomalies by the anomaly from their climatologies
max_bin=2.0    ; Value of the maximum bin in mm/day
min_bin=-2.0   ; Value of the minimum bin in mm/day
n_bins=21      ; Number of bins between the maximum and minimum
bins=findgen(n_bins)*(max_bin-min_bin)/FLOAT(n_bins-1)+min_bin
print,bins

twentyc_precip_aavg_anom_indices=VALUE_LOCATE(bins,twentyc_precip_aavg_anom)
twentyc_precip_aavg_anom_binned=fltarr(n_years,n_bins)
silo_precip_aavg_anom_bin=VALUE_LOCATE(bins,silo_precip_aavg_anom)
twentyc_nmembers_match_silo=intarr(n_years)
; The number of bins within which a 20CR ensemble member must be in order to declare it a match to SILO
tolerance=1

FOR i=0,n_years-1 DO BEGIN
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(REFORM(twentyc_precip_aavg_anom_indices(*,i)) eq j)) ge 0 THEN BEGIN
         twentyc_precip_aavg_anom_binned(i,j)=N_ELEMENTS(where(REFORM(twentyc_precip_aavg_anom_indices(*,i)) eq j))
      ENDIF ELSE $
         twentyc_precip_aavg_anom_binned(i,j)=0
   ENDFOR   
                                ; Compute the number of ensemble members in 20CR that are in the same bin as SILO
   IF TOTAL(where(ABS(REFORM(twentyc_precip_aavg_anom_indices(*,i))-silo_precip_aavg_anom_bin(i)) le tolerance)) gt 0 THEN BEGIN
      twentyc_nmembers_match_silo(i)=N_ELEMENTS(where(ABS(REFORM(twentyc_precip_aavg_anom_indices(*,i))-silo_precip_aavg_anom_bin(i)) le tolerance))
   ENDIF ELSE $
      twentyc_nmembers_match_silo(i)=0
ENDFOR

; Express levels in terms of number of ensemble members fitting into each bin
mylevs=['4','8','12','16','20','24','28','32','36','40','44','48','52']

psfile='/home/ss901165/idl/queensland/reanalysis/twentyc_everymember_compare_silo_boxavg.qld_box_anom_binned.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2000,SPACE3=500,XOFFSET=500,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
       SPACE2=700
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,white=2
LEVS,MANUAL=mylevs
GSET,XMIN=start_year-1,XMAX=start_year+n_years+1,YMIN=min_bin,YMAX=max_bin
CON,X=indgen(n_years)+1900,Y=bins,FIELD=twentyc_precip_aavg_anom_binned,/BLOCK,/NOLINES,$
    CB_TITLE='Number of ensemble members in bin (bin size='+STRMID(STRTRIM(STRING((max_bin-min_bin)/(n_bins-1)),1),0,4)+' mm/day)'
GPLOT,X=indgen(n_years)+1900,Y=silo_precip_aavg_anom,THICK=150
xstep=6
AXES,XVALS=indgen(n_years/xstep+1)*xstep+start_year,XMINOR=indgen(n_years/xstep*2+1)*xstep/2+start_year,$
     YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='Anomaly in area-averaged precipitation (mm/day)',XTITLE='Year',/NORIGHT

GSET,XMIN=start_year-1,XMAX=start_year+n_years+1,YMIN=0,YMAX=twentyc_nmembers
GPLOT,X=indgen(n_years)+1900,Y=twentyc_nmembers_match_silo,STYLE=2,/NOLINES,SYM=3,SIZE=70
AXES,YSTEP=4,YMINOR=2,/ONLYRIGHT,YTITLE='Number of ensemble members within '+$
     STRTRIM(STRING(tolerance),1)+' bin(s) of the SILO value'

PSCLOSE

STOP
END

