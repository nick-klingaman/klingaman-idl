PRO mjo_iav_regressions_smean_hg3kpp

dir='/home/ss901165/um_output6/xihvd'
rmm_file=dir+'/rmm_indices.nc'
n_years=60
ndays_per_year=360
start_day=300
stop_day=120
IF stop_day le start_day THEN BEGIN
   n_days=stop_day+360-start_day+1
   n_years_mean=n_years-1
   year_offset=0
ENDIF ELSE IF start_day lt 120 THEN BEGIN
   n_days=stop_day-start_day+1
   n_years=n_years-1
   n_years_mean=n_years
   year_offset=1
ENDIF ELSE BEGIN
   n_days=stop_day-start_day+1
   n_years_mean=n_years
   year_offset=0
ENDELSE
period_str='ndjfma'

amplitude=OPEN_AND_EXTRACT(rmm_file,'amplitude',$
                           offset=[year_offset,0],count=[n_years,ndays_per_year])
amplitude_ts=fltarr(ndays_per_year*n_years)
FOR i=0,n_years-1 DO $
   FOR j=0,ndays_per_year-1 DO $
      amplitude_ts(i*ndays_per_year+j)=amplitude(i,j)
amplitude_mean=fltarr(n_years_mean)
FOR i=0,n_years_mean-1 DO $
   amplitude_mean(i)=MEAN(amplitude_ts(i*ndays_per_year+start_day:i*ndays_per_year+start_day+n_days))

n_fields=2

FOR i=0,n_fields-1 DO BEGIN
   CASE i OF
      1 : BEGIN
         field_file=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.precip.nc'
         field_name='precip'
         field_varname='precip'
         field_multiplier=86400.
         field_box=[-40,0,40,360]
         field_units='mm day!U-1!N'
         field_levs=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
         field_scale=1
         field_colorrev=1
         field_whitecol=8
      END
      0 : BEGIN
         field_file=dir+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.sst.nc'
         field_name='sst'
         field_varname='temp_2'
         field_multiplier=1.
         field_box=[-40,0,40,360]
         field_units='K'
         field_levs=['-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1']
         field_scale=1
         field_colorrev=0
         field_whitecol=8
      END
   ENDCASE

   field_longitude=OPEN_AND_EXTRACT(field_file,'longitude')
   field_latitude=OPEN_AND_EXTRACT(field_file,'latitude')
   DEFINE_BOUNDARIES,field_box,field_latitude,field_longitude,box_tx,/LIMIT
   field_nlon=N_ELEMENTS(field_longitude)
   field_nlat=N_ELEMENTS(field_latitude)

   field=OPEN_AND_EXTRACT(field_file,field_varname,offset=[box_tx(1),box_tx(0),0,year_offset],$
                          count=[field_nlon,field_nlat,ndays_per_year,n_years])*field_multiplier
   field_mean=fltarr(field_nlon,field_nlat,n_years_mean)
   FOR j=0,field_nlon-1 DO BEGIN
      FOR k=0,field_nlat-1 DO BEGIN
         field_ts=fltarr(ndays_per_year*n_years)
         FOR m=0,n_years-1 DO $
            field_ts(m*ndays_per_year:(m+1)*ndays_per_year-1)=field(j,k,*,m)
         FOR m=0,n_years_mean-1 DO $     
            field_mean(j,k,m)=MEAN(field_ts(m*ndays_per_year+start_day:m*ndays_per_year+start_day+n_days))
         trend=REGRESS(indgen(n_years_mean),REFORM(field_mean(j,k,*)))
         corr=CORRELATE(indgen(n_years_mean),REFORM(field_mean(j,k,*)))
         IF ABS(corr) ge 0.25 THEN $
            field_mean(j,k,*)=field_mean(j,k,*)-(trend(0)*indgen(n_years_mean))
      ENDFOR
   ENDFOR

   field_regressions=fltarr(field_nlon,field_nlat)
   field_correlations=fltarr(field_nlon,field_nlat)
   FOR j=0,field_nlon-1 DO BEGIN
      FOR k=0,field_nlat-1 DO BEGIN
         field_regressions(j,k)=REGRESS(amplitude_mean,REFORM(field_mean(j,k,*)))
         field_correlations(j,k)=CORRELATE(amplitude_mean,REFORM(field_mean(j,k,*)))
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mjo_iav/regressions/mjo_iav_regressions_smean_hg3kpp.xihvd_'+period_str+'_'+field_name+'.ps'
   PSOPEN,file=psfile,TFONT=6,CHARSIZE=160,FONT=6,TCHARSIZE=110,MARGIN=2000,XOFFSET=500,YSIZE=10000,YOFFSET=2000,SPACE2=1500
   MAP,LONMIN=field_box(1),LONMAX=field_box(3),LATMIN=field_box(0),LATMAX=field_box(2)
   IF field_colorrev eq 1 THEN BEGIN
      CS,SCALE=field_scale,NCOLS=N_ELEMENTS(field_levs)+1,/REV,white=[field_whitecol]
   ENDIF ELSE $
      CS,SCALE=field_scale,NCOLS=N_ELEMENTS(field_levs)+1,white=[field_whitecol]
   LEVS,MANUAL=field_levs
   CON,X=field_longitude,Y=field_latitude,FIELD=field_regressions,TITLE='Regression of '+field_name+' on MJO amplitude for '+period_str+' over '+$
       STRTRIM(STRING(n_years_mean),1)+' years of GA3-KPP',CB_TITLE=field_units+' per unit MJO amplitude',/NOLINES,/BLOCK
   FOR j=0,field_nlon-1 DO $
      FOR k=0,field_nlat-1 DO $
         IF ABS(field_correlations(j,k))*1.5 ge 0.25 and ABS(field_regressions(j,k)) gt 0.01 THEN $
            GPLOT,X=field_longitude(j),Y=field_latitude(k),SYM=3,SIZE=20
   AXES,YSTEP=20,XSTEP=60,YMINOR=10,XMINOR=30
   PSCLOSE

ENDFOR

STOP
END
