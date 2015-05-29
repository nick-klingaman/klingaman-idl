PRO mjo_diabatic_plot_Q2_Tadv_vint,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','miroc','mri','nrl','nasa','giss','spcam','cancm4','nicam','cam5zm','cnrm_atmos','metum']
IF mask_type ne 'ocean' and mask_type ne 'land' THEN BEGIN
   print,'You must set mask_type equal to either ocean or land'
   STOP
END

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
cam5_valid_dates=STRTRIM(STRING([indgen(4)+20091010,indgen(16)+20091016,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
ecearth_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(21)+20100101]),1)
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

; Parse start and stop dates
start_year=STRMID(start_date,0,4)
stop_year=STRMID(stop_date,0,4)
start_month=STRMID(start_date,4,2)
stop_month=STRMID(stop_date,4,2)
start_day=STRMID(start_date,6,2)
stop_day=STRMID(stop_date,6,2)

; Get Julian dates
start_julian=GREGORIAN_TO_JULIAN(FLOAT(start_day),FLOAT(start_month),FLOAT(start_year))
stop_julian=GREGORIAN_TO_JULIAN(FLOAT(stop_day),FLOAT(stop_month),FLOAT(stop_year))
IF stop_julian lt start_julian THEN $
   stop_julian=stop_julian+365
n_times_per_day=8
n_lead_times=N_ELEMENTS(lead_times)
n_models=N_ELEMENTS(our_model_names)

;For physics
tadv_bins=findgen(21)*1-10.5
qadv_bins=findgen(21)*1-10.5
;q2_bins=findgen(28)*0.2-3.9
;tadv_bins=findgen(24)*0.2-2.3
;q2_bins=findgen(24)*0.2-2.3
n_tadvbins=N_ELEMENTS(tadv_bins)
n_qadvbins=N_ELEMENTS(qadv_bins)

FOR i=0,n_models-1 DO BEGIN
   print,our_model_names(i)
   CASE our_model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         file_desc='ECMWF_IFS'
         model_desc='ECMWF_IFS'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e10
         has_landsea=1
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         file_desc='GEOS5_AGCM'
         model_desc='GEOS5_AGCM'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=10000.
         has_landsea=0
      END    
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         file_desc='MRI-AGCM'
         model_desc='MRI-AGCM'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=0.01
         reverse_level=0
         missing_value=10000.
         has_landsea=0
      END  
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_desc='miroc5'
         model_desc='MIROC5'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=999.
         has_landsea=0
      END    
      'nrl' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         file_desc='NGEM01'
         model_desc='NRL_NavGEM01'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=1
         missing_value=10000.
         has_landsea=0
      END      
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         file_desc='MetUM'
         model_desc='MetUM_GA30'
         valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(22)+20091101,20091124,20091125,$
                                     indgen(22)+20091210,indgen(6)+20100101,indgen(18)+20100108]),1)
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=72.
         Q2_multiplier=72.*1000.
         level_multiplier=1.
         reverse_level=0
         missing_value=100000.
         has_landsea=1
      END      
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_desc='ModelE'
         model_desc='GISS_ModelE2'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=1000.*86400.
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END      
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_desc='CanCM4'
         model_desc='CCCma_CanCM4'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=0.01
         reverse_level=0
         missing_value=1e4
         has_landsea=0
      END  
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         file_desc='SPCAM3.0'
         model_desc='SPCAM3.0'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev_p'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END 
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         file_desc='NCAR.CAM5'
         model_desc='NCAR_CAM5'
         valid_dates=cam5_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='levels'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END 
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_desc='CAM5ZMMicroCAPT'
         model_desc='LLNL_CAM5ZM'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END 
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         file_desc='ecearth3'
         model_desc='SHMI_ECEarth3'
         valid_dates=ecearth_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END 
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         file_desc='CNRM'
         model_desc='CNRM_Atmos'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e5
         has_landsea=0
      END 
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         file_desc='nicam'
         model_desc='NICAM'
         valid_dates=['20091015','20091020','20091025','20091030','20091104','20091109',$
                      '20091215','20091220','20091225','20091230','20100104','20100109']
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         tadv_multiplier=86400.
         Q2_multiplier=86400.*1000.*2.5
         level_multiplier=1.
         reverse_level=0
         missing_value=1e6
         has_landsea=0
      END 
   ENDCASE
   
   grid_flag=0
   start_position=REFORM(where(standard_valid_dates eq start_date))
   date_offset=where(dates eq start_date)
   n_days=where(standard_valid_dates eq stop_date)-start_position
   print,n_days
  
   FOR j=0,n_days(0)-1 DO BEGIN
;      print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            tadv_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnta.'+REFORM(initial_date)+'.00Z.nc'
            print,tadv_infile
            Q2_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnhusa.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               tadv_longitude=OPEN_AND_EXTRACT(tadv_infile(0),longitude_name)
               tadv_latitude=OPEN_AND_EXTRACT(tadv_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,tadv_latitude,tadv_longitude,tadv_box_tx,/LIMIT
               tadv_nlon=N_ELEMENTS(tadv_longitude)
               tadv_nlat=N_ELEMENTS(tadv_latitude)
               
               ;tadv_zonal_longitude=OPEN_AND_EXTRACT(tadv_infile(0),longitude_name)
               ;DEFINE_BOUNDARIES,[box(0),60,box(2),200],tadv_latitude,tadv_zonal_longitude,tadv_zonal_box_tx,/LIMIT
               ;tadv_zonal_nlon=N_ELEMENTS(tadv_zonal_longitude)
               
               Q2_longitude=OPEN_AND_EXTRACT(q2_infile(0),longitude_name)
               Q2_latitude=OPEN_AND_EXTRACT(q2_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,q2_latitude,q2_longitude,q2_box_tx,/LIMIT
               q2_nlon=N_ELEMENTS(q2_longitude)
               q2_nlat=N_ELEMENTS(q2_latitude)
               
               p=OPEN_AND_EXTRACT(tadv_infile(0),level_name)*level_multiplier
               IF reverse_level eq 1 THEN BEGIN
                  offset_p=0
                  p=p[where(p le 1000)]
               ENDIF ELSE BEGIN
                  offset_p=NEAREST(p,1000)
                  p=p[where(p le p(offset_p))]
               ENDELSE
               np=N_ELEMENTS(p)
               
               IF has_landsea eq 1 THEN BEGIN
                  model_mask_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.landsea.'+REFORM(initial_date)+'.00Z.nc'
               ENDIF ELSE BEGIN
                  model_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'                  
               ENDELSE               
               mask_longitude=OPEN_AND_EXTRACT(model_mask_infile(0),'longitude')
               mask_latitude=OPEN_AND_EXTRACT(model_mask_infile(0),'latitude')
               DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT                  
               model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                                  offset=[mask_box_tx(1),mask_box_tx(0),0],count=[tadv_nlon,tadv_nlat,1]))
               grid_flag=1
               
               twod_bin=fltarr(n_tadvbins+1,n_qadvbins+1)               
               mean_tadvbin=fltarr(n_tadvbins+1)
               mean_q2bin=fltarr(n_qadvbins+1)
               median_tadvbin=fltarr(n_tadvbins+1)
               median_q2bin=fltarr(n_qadvbins+1)
               tadv_anom_vint=fltarr(tadv_nlon,tadv_nlat,n_days,n_lead_times,n_times_per_day)               
               q2_vint=fltarr(tadv_nlon,tadv_nlat,n_days,n_lead_times,n_times_per_day)
            ENDIF
            tadv=OPEN_AND_EXTRACT(tadv_infile(0),'tnta',$
                                  offset=[tadv_box_tx(1),tadv_box_tx(0),offset_p,lead_times(k)*n_times_per_day],$
                                  count=[tadv_nlon,tadv_nlat,np,n_times_per_day])
            IF TOTAL(where(ABS(tadv) ge missing_value)) ge 0 THEN $
               tadv[where(ABS(tadv) ge missing_value)]=!Values.F_NaN
            ;tadv_zonal=OPEN_AND_EXTRACT(tadv_infile(0),'hus',$
            ;                         offset=[tadv_zonal_box_tx(1),tadv_box_tx(0),offset_p,0],$
            ;                         count=[tadv_zonal_nlon,tadv_nlat,np,n_times_per_day])
            ;IF TOTAL(where(ABS(tadv_zonal) ge missing_value)) ge 0 THEN $
            ;   tadv_zonal[where(ABS(tadv_zonal) ge missing_value)]=!Values.F_NaN
            tadv=tadv*tadv_multiplier
            ;tadv_zonal=tadv_zonal*tadv_multiplier
            FOR n=0,np-1 DO BEGIN
               FOR m=0,n_times_per_day-1 DO BEGIN
                  temp=REFORM(tadv(*,*,n,m))
                  IF mask_type eq 'ocean' THEN $
                     temp[where(model_mask eq 1)]=!Values.F_NaN
                  IF mask_type eq 'land' THEN $
                     temp[where(model_mask eq 0)]=!Values.F_NaN
                  tadv(*,*,n,m)=temp
               ENDFOR
            ENDFOR
            
            print,q2_infile(0)
            q2=OPEN_AND_EXTRACT(q2_infile(0),'tnhusa',$
                                offset=[q2_box_tx(1),q2_box_tx(0),offset_p,lead_times(k)*n_times_per_day],$
                                count=[q2_nlon,q2_nlat,np,n_times_per_day])
            IF TOTAL(where(ABS(q2) ge missing_value)) ge 0 THEN $
               q2[where(ABS(q2) ge missing_value)]=!Values.F_NaN
            q2=q2*q2_multiplier
            print,MAX(q2,/NaN),MAX(tadv,/NaN),N_ELEMENTS(where(FINITE(tadv) eq 0)),N_ELEMENTS(tadv)

            FOR r=0,n_times_per_day-1 DO BEGIN
               dp_total=0
               FOR s=0,np-2 DO BEGIN
                  FOR m=0,tadv_nlat-1 DO BEGIN      
                     ;tadv_latavg=MEAN(tadv_zonal(*,m,k,r),/NaN)      
                     ;IF tadv_latavg ge 1e5 THEN $
                                ;   STOP
                     FOR n=0,tadv_nlon-1 DO BEGIN                        
                        tadv_anom=tadv(n,m,s,r) ;-tadv_latavg                        
                        IF FINITE(tadv_anom) eq 1 THEN BEGIN
                           tadv_anom_vint(n,m,j,k,r)=tadv_anom_vint(n,m,j,k,r)+$
                                                     tadv_anom*(p(s)-p(s+1))
                           q2_vint(n,m,j,k,r)=q2_vint(n,m,j,k,r)+$
                                              q2(n,m,s,r)*(p(s)-p(s+1))                        
                        ENDIF
                     ENDFOR
                  ENDFOR
                  dp_total=dp_total+p(s)-p(s+1)
               ENDFOR
               tadv_anom_vint(*,*,j,k,r)=tadv_anom_vint(*,*,j,k,r)/dp_total
               q2_vint(*,*,j,k,r)=q2_vint(*,*,j,k,r)/dp_total
            ENDFOR
         ENDFOR
      ENDIF ELSE IF grid_flag eq 1 THEN BEGIN
         tadv_anom_vint(*,*,j,*,*)=!Values.F_NaN      
         q2_vint(*,*,j,*,*)=!Values.F_NaN
      ENDIF
   ENDFOR

   IF TOTAL(where(tadv_anom_vint eq 0)) ge 0 THEN $
      tadv_anom_vint[where(tadv_anom_vint eq 0)]=!Values.F_NaN
   lead_time_mean=fltarr(tadv_nlon,tadv_nlat,n_lead_times,n_times_per_day)
   tadv_anom_save=tadv_anom_vint
   FOR j=0,tadv_nlon-1 DO BEGIN
      FOR k=0,tadv_nlat-1 DO BEGIN     
         FOR m=0,n_lead_times-1 DO BEGIN
            FOR n=0,n_times_per_day-1 DO BEGIN
               lead_time_mean(j,k,m,n)=MEAN(tadv_anom_vint(j,k,*,m,n),/NaN)
               tadv_anom_vint(j,k,*,m,n)=tadv_anom_vint(j,k,*,m,n)-lead_time_mean(j,k,m,n)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

   FOR k=0,n_tadvbins-2 DO BEGIN
      IF TOTAL(where(tadv_anom_vint ge tadv_bins(k) and tadv_anom_vint lt tadv_bins(k+1))) ge 0 THEN BEGIN
         pts=where(tadv_anom_vint ge tadv_bins(k) and tadv_anom_vint lt tadv_bins(k+1))                  
         temp_qadv=q2_vint[pts]
         FOR m=0,n_qadvbins-2 DO $
            IF TOTAL(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1))) ge 0 THEN $
               twod_bin(k+1,m+1)=twod_bin(k+1,m+1)+N_ELEMENTS(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1)))            
         IF TOTAL(where(temp_qadv lt qadv_bins(0))) ge 0 THEN $
            twod_bin(k+1,0)=twod_bin(k+1,0)+N_ELEMENTS(where(temp_qadv lt qadv_bins(0)))
         IF TOTAL(where(temp_qadv ge qadv_bins(n_qadvbins-1))) ge 0 THEN $
            twod_bin(k+1,n_qadvbins)=twod_bin(k+1,n_qadvbins)+N_ELEMENTS(where(temp_qadv ge qadv_bins(n_qadvbins-1)))
         mean_tadvbin(k+1)=MEAN(temp_qadv,/NaN)
         median_tadvbin(k+1)=MEDIAN(temp_qadv)
      ENDIF  
   ENDFOR
   
   IF TOTAL(where(tadv_anom_vint lt tadv_bins(0))) ge 0 THEN BEGIN
      pts=where(tadv_anom_vint lt tadv_bins(0))
      temp_qadv=q2_vint[pts]
      FOR m=0,n_qadvbins-2 DO $
         IF TOTAL(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1))) ge 0 THEN $                        
            twod_bin(0,m)=twod_bin(0,m)+N_ELEMENTS(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1)))
      IF TOTAL(where(temp_qadv lt qadv_bins(0))) ge 0 THEN $
         twod_bin(0,0)=twod_bin(0,0)+N_ELEMENTS(where(temp_qadv lt qadv_bins(0)))
      IF TOTAL(where(temp_qadv ge qadv_bins(n_qadvbins-1))) ge 0 THEN $
         twod_bin(0,n_qadvbins)=twod_bin(0,n_qadvbins)+N_ELEMENTS(where(temp_qadv ge qadv_bins(n_qadvbins-1)))
      mean_tadvbin(0)=MEAN(temp_qadv,/NaN)
      median_tadvbin(0)=MEDIAN(temp_qadv)
   ENDIF
   IF TOTAL(where(tadv_anom_vint ge tadv_bins(n_tadvbins-1))) ge 0 THEN BEGIN
      pts=where(tadv_anom_vint ge tadv_bins(n_tadvbins-1))
      temp_qadv=q2_vint[pts]
      FOR m=0,n_qadvbins-2 DO $
         IF TOTAL(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1))) ge 0 THEN $                        
            twod_bin(n_tadvbins,m+1)=twod_bin(n_tadvbins,m+1)+N_ELEMENTS(where(temp_qadv ge qadv_bins(m) and temp_qadv lt qadv_bins(m+1)))
      IF TOTAL(where(temp_qadv lt qadv_bins(0))) ge 0 THEN $
         twod_bin(n_tadvbins,0)=twod_bin(n_tadvbins,0)+N_ELEMENTS(where(temp_qadv lt qadv_bins(0)))
      IF TOTAL(where(temp_qadv ge qadv_bins(n_qadvbins-1))) ge 0 THEN $
         twod_bin(n_tadvbins,n_qadvbins)=twod_bin(n_tadvbins,n_qadvbins)+N_ELEMENTS(where(temp_qadv ge qadv_bins(n_qadvbins-1)))
      mean_tadvbin(n_tadvbins)=MEAN(temp_qadv,/NaN)
      median_tadvbin(n_tadvbins)=MEDIAN(temp_qadv)
   ENDIF

   FOR k=0,n_qadvbins-2 DO BEGIN
      IF TOTAL(where(q2_vint ge qadv_bins(k) and q2_vint lt qadv_bins(k+1))) ge 0 THEN BEGIN
         pts=where(q2_vint ge qadv_bins(k) and q2_vint lt qadv_bins(k+1)) 
         temp_tadv=tadv_anom_vint[pts]
         mean_q2bin(k+1)=MEAN(temp_tadv,/NaN)
         median_q2bin(k+1)=MEDIAN(temp_tadv[where(FINITE(temp_tadv) eq 1)])
      ENDIF ELSE BEGIN
         mean_q2bin(k+1)=!Values.F_NaN
         median_q2bin(k+1)=!Values.F_NaN
      ENDELSE
   ENDFOR
   IF TOTAL(where(q2_vint lt qadv_bins(0))) ge 0 THEN BEGIN
      pts=where(q2_vint lt qadv_bins(0))
      temp_tadv=tadv_anom_vint[pts]
      mean_q2bin(0)=MEAN(temp_tadv,/NaN)
      median_q2bin(0)=MEDIAN(temp_tadv[where(FINITE(temp_tadv) eq 1)])
   ENDIF ELSE BEGIN
      mean_q2bin(0)=!Values.F_NaN
      median_q2bin(0)=!Values.F_NaN
   ENDELSE
   IF TOTAL(where(q2_vint ge qadv_bins(n_qadvbins-1))) ge 0 THEN BEGIN
      pts=where(q2_vint ge qadv_bins(n_qadvbins-1))
      temp_tadv=tadv_anom_vint[pts]
      mean_q2bin(n_qadvbins)=MEAN(temp_tadv,/NaN)
      median_q2bin(n_qadvbins)=MEDIAN(temp_tadv[where(FINITE(temp_tadv) eq 1)])
   ENDIF ELSE BEGIN
      mean_q2bin(n_qadvbins)=!Values.F_NaN
      median_q2bin(n_qadvbins)=!Values.F_NaN
   ENDELSE

   total_twod_bin=TOTAL(twod_bin)
   twod_bin=twod_bin/total_twod_bin

   slope_qadv=REGRESS(mean_q2bin(1:NEAREST(qadv_bins,0)-1),
   
   frac_levels=['0.001','0.002','0.003','0.004','0.005','0.006','0.007','0.008','0.01','0.012','0.015','0.018','0.021','0.024','0.027']
   quad_frac=fltarr(4,3)        ; [quadrant, (wrt all, wrt x half, wrt y half)] do not count small anomalies (middle row and column)
   FOR j=0,3 DO BEGIN
      CASE j OF 
         0 : BEGIN
            x_range=[0,n_tadvbins/2-1]
            y_range=[0,NEAREST(qadv_bins,0)-1]
         END
         1 : BEGIN
            x_range=[n_tadvbins/2+1,n_tadvbins]
            y_range=[0,NEAREST(qadv_bins,0)-1]
         END
         2 : BEGIN
            x_range=[0,n_tadvbins/2-1]
            y_range=[NEAREST(qadv_bins,0)+1,n_qadvbins]
         END
         3 : BEGIN
            x_range=[n_tadvbins/2+1,n_tadvbins]
            y_range=[NEAREST(qadv_bins,0)+1,n_qadvbins]
         END
      ENDCASE
      quad_frac(j,0)=TOTAL(twod_bin(x_range(0):x_range(1),y_range(0):y_range(1)))
   ENDFOR
   FOR j=0,3 DO BEGIN
      IF ODD(j) eq 1 THEN BEGIN
         offset=-1
      ENDIF ELSE $
         offset=1
      quad_frac(j,1)=quad_frac(j,0)/(quad_frac(j,0)+quad_frac(j+offset,0))
   ENDFOR
   FOR j=0,3 DO BEGIN
      IF j eq 0 THEN BEGIN
         offset=2
      ENDIF ELSE IF j eq 1 THEN BEGIN
         offset=2
      ENDIF ELSE IF j eq 2 THEN BEGIN
         offset=-2
      ENDIF ELSE IF j eq 3 THEN $
         offset=-2
      quad_frac(j,2)=quad_frac(j,0)/(quad_frac(j,0)+quad_frac(j+offset,0))
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/q_bin/mjo_diabatic_plot_qadv_tadv_vint.tadv_anom_leadtime.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=200,SPACE2=2000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110,XSIZE=17000,YSIZE=17000
   GSET,XMIN=0,XMAX=n_tadvbins+1,YMIN=0,YMAX=n_qadvbins+1
   CS,SCALE=24,NCOLS=N_ELEMENTS(frac_levels)+1,white=[2]
   LEVS,MANUAL=frac_levels
   CON,X=indgen(n_tadvbins+1)+0.5,Y=indgen(n_qadvbins+1)+0.5,FIELD=twod_bin,/BLOCK,/NOLINES,$
       CB_TITLE='Fraction of points'
   GPLOT,X=[n_tadvbins/2+0.5,n_tadvbins/2+0.5],Y=[0,n_qadvbins+1],STYLE=1
   GPLOT,X=[0,n_tadvbins+1],Y=[NEAREST(qadv_bins,0)+1.5,NEAREST(qadv_bins,0)+1.5],STYLE=1
   mean_plot=fltarr(n_tadvbins+1)
   median_plot=fltarr(n_tadvbins+1)
   FOR j=0,n_tadvbins DO BEGIN
      mean_plot(j)=NEAREST(qadv_bins,mean_tadvbin(j))+1
      IF mean_plot(j) lt n_qadvbins-1 THEN $
         mean_plot(j)=mean_plot(j)+(mean_tadvbin(j)-qadv_bins(mean_plot(j)))/(qadv_bins(mean_plot(j)+1)-qadv_bins(mean_plot(j)))+1
      median_plot(j)=NEAREST(qadv_bins,median_tadvbin(j))+1
      IF median_plot(j) lt n_qadvbins-1 THEN $
         median_plot(j)=median_plot(j)+(median_tadvbin(j)-qadv_bins(median_plot(j)))/(qadv_bins(median_plot(j)+1)-qadv_bins(median_plot(j)))+1
   ENDFOR
   GPLOT,X=indgen(n_tadvbins+1)+0.5,Y=mean_plot,STYLE=0,COL=FSC_COLOR('red')
   GPLOT,X=indgen(n_tadvbins+1)+0.5,Y=median_plot,STYLE=0,COL=FSC_COLOR('orange')
   mean_plot=fltarr(n_qadvbins+1)
   median_plot=fltarr(n_qadvbins+1)
   FOR j=0,n_qadvbins DO BEGIN
      mean_plot(j)=NEAREST(tadv_bins,mean_q2bin(j))+1
      IF mean_plot(j) lt n_tadvbins-1 THEN $
         mean_plot(j)=mean_plot(j)+(mean_q2bin(j)-tadv_bins(mean_plot(j)))/(tadv_bins(mean_plot(j)+1)-tadv_bins(mean_plot(j)))+1
      median_plot(j)=NEAREST(tadv_bins,median_q2bin(j))+1
      IF median_plot(j) lt n_tadvbins-1 THEN $
         median_plot(j)=median_plot(j)+(median_q2bin(j)-tadv_bins(median_plot(j)))/(tadv_bins(median_plot(j)+1)-tadv_bins(median_plot(j)))+1
   ENDFOR
   GPLOT,Y=indgen(n_qadvbins+1)+0.5,X=mean_plot,STYLE=0,COL=FSC_COLOR('purple')
   GPLOT,Y=indgen(n_qadvbins+1)+0.5,X=median_plot,STYLE=0,COL=FSC_COLOR('violetred')   

   FOR j=0,2 DO BEGIN
      GPLOT,X=1+j*2,Y=1,TEXT=STRMID(STRTRIM(STRING(quad_frac(0,j)),1),0,5)
      GPLOT,X=n_tadvbins-5+j*2,Y=1,TEXT=STRMID(STRTRIM(STRING(quad_frac(1,j)),1),0,5)
      GPLOT,X=n_tadvbins-5+j*2,Y=n_qadvbins,TEXT=STRMID(STRTRIM(STRING(quad_frac(3,j)),1),0,5)
      GPLOT,X=1+j*2,Y=n_qadvbins,TEXT=STRMID(STRTRIM(STRING(quad_frac(2,j)),1),0,5)
   ENDFOR
   AXES,XVALS=indgen(n_tadvbins+2),YVALS=indgen(n_qadvbins+2),$
        XLABELS=['<'+STRMID(STRTRIM(STRING(tadv_bins(0)),1),0,5),STRMID(STRTRIM(STRING(tadv_bins[where(tadv_bins lt 0)]),1),0,5),$
                 STRMID(STRTRIM(STRING(tadv_bins[where(tadv_bins ge 0)]),1),0,4),'>'+STRMID(STRTRIM(STRING(tadv_bins(n_tadvbins-1)),1),0,4)],$
        YLABELS=['<'+STRMID(STRTRIM(STRING(qadv_bins(0)),1),0,5),STRMID(STRTRIM(STRING(qadv_bins),1),0,4),$
                 '>'+STRMID(STRTRIM(STRING(qadv_bins(n_qadvbins-1)),1),0,4)],$
        ORIENTATION=30,XTITLE='Vint anom in dTadv/dt (from leadtime-dependent mean; K day!U-1!N)',$
        YTITLE='Vertical integrated dqadv/dt (g kg!U-1!N day!U-1!N)'
   GLEGEND,labels=['Median qadv in Tadv bin','Mean qadv in Tadv bin','Median Tadv in qadv bin','Mean Tadv in qadv bin'],$
           COL=[FSC_COLOR('orange'),FSC_COLOR('red'),FSC_COLOR('violetred'),FSC_COLOR('purple')],$
           LEGXOFFSET=7500,LEGYOFFSET=4000
   PSCLOSE
ENDFOR

STOP
            
END
