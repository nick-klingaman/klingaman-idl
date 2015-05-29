PRO mjo_diabatic_plot_Q2_q_vint,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','miroc','mri','nrl','nasa','giss','spcam','cancm4','nicam','cam5','cam5zm','cnrm_atmos','ecearth','metum']
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
q_bins=findgen(24)*0.2-2.3
q2_bins=findgen(28)*0.2-3.9
;q_bins=findgen(24)*0.2-2.3
;q2_bins=findgen(24)*0.2-2.3
n_qbins=N_ELEMENTS(q_bins)
n_q2bins=N_ELEMENTS(q2_bins)

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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
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
         q_multiplier=1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
         level_multiplier=0.01
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
         q_multiplier=1000.
         Q2_multiplier=86400.*1000.
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
            q_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.hus.'+REFORM(initial_date)+'.00Z.nc'
            print,q_infile
            Q2_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnhus_physics.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               q_longitude=OPEN_AND_EXTRACT(q_infile(0),longitude_name)
               q_latitude=OPEN_AND_EXTRACT(q_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,q_latitude,q_longitude,q_box_tx,/LIMIT
               q_nlon=N_ELEMENTS(q_longitude)
               q_nlat=N_ELEMENTS(q_latitude)
               
               ;q_zonal_longitude=OPEN_AND_EXTRACT(q_infile(0),longitude_name)
               ;DEFINE_BOUNDARIES,[box(0),60,box(2),200],q_latitude,q_zonal_longitude,q_zonal_box_tx,/LIMIT
               ;q_zonal_nlon=N_ELEMENTS(q_zonal_longitude)
               
               Q2_longitude=OPEN_AND_EXTRACT(q2_infile(0),longitude_name)
               Q2_latitude=OPEN_AND_EXTRACT(q2_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,q2_latitude,q2_longitude,q2_box_tx,/LIMIT
               q2_nlon=N_ELEMENTS(q2_longitude)
               q2_nlat=N_ELEMENTS(q2_latitude)
               
               p=OPEN_AND_EXTRACT(q2_infile(0),level_name)*level_multiplier
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
                  model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                                     offset=[q_box_tx(1),q_box_tx(0),0],count=[q_nlon,q_nlat,1]))
               ENDIF ELSE BEGIN
                  model_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
                  model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                                     offset=[q_box_tx(1),q_box_tx(0),0],count=[q_nlon,q_nlat,1]))
               ENDELSE
               grid_flag=1
               
               twod_bin=fltarr(n_qbins+1,n_q2bins+1)               
               mean_qbin=fltarr(n_qbins+1)
               mean_q2bin=fltarr(n_q2bins+1)
               median_qbin=fltarr(n_qbins+1)
               median_q2bin=fltarr(n_q2bins+1)
               q_anom_vint=fltarr(q_nlon,q_nlat,n_days,n_lead_times,n_times_per_day)               
               q2_vint=fltarr(q_nlon,q_nlat,n_days,n_lead_times,n_times_per_day)
            ENDIF
            q=OPEN_AND_EXTRACT(q_infile(0),'hus',$
                               offset=[q_box_tx(1),q_box_tx(0),offset_p,lead_times(k)*n_times_per_day],$
                               count=[q_nlon,q_nlat,np,n_times_per_day])
            IF TOTAL(where(ABS(q) ge missing_value)) ge 0 THEN $
               q[where(ABS(q) ge missing_value)]=!Values.F_NaN
            ;q_zonal=OPEN_AND_EXTRACT(q_infile(0),'hus',$
            ;                         offset=[q_zonal_box_tx(1),q_box_tx(0),offset_p,0],$
            ;                         count=[q_zonal_nlon,q_nlat,np,n_times_per_day])
            ;IF TOTAL(where(ABS(q_zonal) ge missing_value)) ge 0 THEN $
            ;   q_zonal[where(ABS(q_zonal) ge missing_value)]=!Values.F_NaN
            q=q*q_multiplier
            ;q_zonal=q_zonal*q_multiplier
            FOR n=0,np-1 DO BEGIN
               FOR m=0,n_times_per_day-1 DO BEGIN
                  temp=REFORM(q(*,*,n,m))
                  IF mask_type eq 'ocean' THEN $
                     temp[where(model_mask eq 1)]=!Values.F_NaN
                  IF mask_type eq 'land' THEN $
                     temp[where(model_mask eq 0)]=!Values.F_NaN
                  q(*,*,n,m)=temp
               ENDFOR
            ENDFOR
            
            print,q2_infile(0)
            q2=OPEN_AND_EXTRACT(q2_infile(0),'tnhus_physics',$
                                offset=[q2_box_tx(1),q2_box_tx(0),offset_p,lead_times(k)*n_times_per_day],$
                                count=[q2_nlon,q2_nlat,np,n_times_per_day])
            IF TOTAL(where(ABS(q2) ge missing_value)) ge 0 THEN $
               q2[where(ABS(q2) ge missing_value)]=!Values.F_NaN
            print,MAX(q2,/NaN),MAX(q,/NaN)
            q2=q2*q2_multiplier

            FOR r=0,n_times_per_day-1 DO BEGIN
               dp_total=0
               FOR s=0,np-2 DO BEGIN
                  FOR m=0,q_nlat-1 DO BEGIN      
                     ;q_latavg=MEAN(q_zonal(*,m,k,r),/NaN)      
                     ;IF q_latavg ge 1e5 THEN $
                                ;   STOP
                     FOR n=0,q_nlon-1 DO BEGIN                        
                        q_anom=q(n,m,s,r) ;-q_latavg
                        q_anom_vint(n,m,j,k,r)=q_anom_vint(n,m,j,k,r)+$
                                               q_anom*(p(s)-p(s+1))
                        q2_vint(n,m,j,k,r)=q2_vint(n,m,j,k,r)+$
                                           q2(n,m,s,r)*(p(s)-p(s+1))                        
                     ENDFOR
                  ENDFOR
                  dp_total=dp_total+p(s)-p(s+1)
               ENDFOR
               q_anom_vint(*,*,j,k,r)=q_anom_vint(*,*,j,k,r)/dp_total
               q2_vint(*,*,j,k,r)=q2_vint(*,*,j,k,r)/dp_total
            ENDFOR
         ENDFOR
      ENDIF ELSE IF grid_flag eq 1 THEN BEGIN
         q_anom_vint(*,*,j,*,*)=!Values.F_NaN      
         q2_vint(*,*,j,*,*)=!Values.F_NaN
      ENDIF
   ENDFOR

   IF TOTAL(where(q_anom_vint eq 0)) ge 0 THEN $
      q_anom_vint[where(q_anom_vint eq 0)]=!Values.F_NaN
   lead_time_mean=fltarr(q_nlon,q_nlat,n_lead_times,n_times_per_day)
   q_anom_save=q_anom_vint
   FOR j=0,q_nlon-1 DO BEGIN
      FOR k=0,q_nlat-1 DO BEGIN     
         FOR m=0,n_lead_times-1 DO BEGIN
            FOR n=0,n_times_per_day-1 DO BEGIN
               lead_time_mean(j,k,m,n)=MEAN(q_anom_vint(j,k,*,m,n),/NaN)
               q_anom_vint(j,k,*,m,n)=q_anom_vint(j,k,*,m,n)-lead_time_mean(j,k,m,n)
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

   FOR k=0,n_qbins-2 DO BEGIN
      IF TOTAL(where(q_anom_vint ge q_bins(k) and q_anom_vint lt q_bins(k+1))) ge 0 THEN BEGIN
         pts=where(q_anom_vint ge q_bins(k) and q_anom_vint lt q_bins(k+1))                  
         temp_q2=q2_vint[pts]
         FOR m=0,n_q2bins-2 DO $
            IF TOTAL(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1))) ge 0 THEN $
               twod_bin(k+1,m+1)=twod_bin(k+1,m+1)+N_ELEMENTS(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1)))            
         IF TOTAL(where(temp_q2 lt q2_bins(0))) ge 0 THEN $
            twod_bin(k+1,0)=twod_bin(k+1,0)+N_ELEMENTS(where(temp_q2 lt q2_bins(0)))
         IF TOTAL(where(temp_q2 ge q2_bins(n_q2bins-1))) ge 0 THEN $
            twod_bin(k+1,n_q2bins)=twod_bin(k+1,n_q2bins)+N_ELEMENTS(where(temp_q2 ge q2_bins(n_q2bins-1)))
         mean_qbin(k+1)=MEAN(temp_q2,/NaN)
         median_qbin(k+1)=MEDIAN(temp_q2)
      ENDIF  
   ENDFOR
   
   IF TOTAL(where(q_anom_vint lt q_bins(0))) ge 0 THEN BEGIN
      pts=where(q_anom_vint lt q_bins(0))
      temp_q2=q2_vint[pts]
      FOR m=0,n_q2bins-2 DO $
         IF TOTAL(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1))) ge 0 THEN $                        
            twod_bin(0,m)=twod_bin(0,m)+N_ELEMENTS(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1)))
      IF TOTAL(where(temp_q2 lt q2_bins(0))) ge 0 THEN $
         twod_bin(0,0)=twod_bin(0,0)+N_ELEMENTS(where(temp_q2 lt q2_bins(0)))
      IF TOTAL(where(temp_q2 ge q2_bins(n_q2bins-1))) ge 0 THEN $
         twod_bin(0,n_q2bins)=twod_bin(0,n_q2bins)+N_ELEMENTS(where(temp_q2 ge q2_bins(n_q2bins-1)))
      mean_qbin(0)=MEAN(temp_q2,/NaN)
      median_qbin(0)=MEDIAN(temp_q2)
   ENDIF
   IF TOTAL(where(q_anom_vint ge q_bins(n_qbins-1))) ge 0 THEN BEGIN
      pts=where(q_anom_vint ge q_bins(n_qbins-1))
      temp_q2=q2_vint[pts]
      FOR m=0,n_q2bins-2 DO $
         IF TOTAL(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1))) ge 0 THEN $                        
            twod_bin(n_qbins,m+1)=twod_bin(n_qbins,m+1)+N_ELEMENTS(where(temp_q2 ge q2_bins(m) and temp_q2 lt q2_bins(m+1)))
      IF TOTAL(where(temp_q2 lt q2_bins(0))) ge 0 THEN $
         twod_bin(n_qbins,0)=twod_bin(n_qbins,0)+N_ELEMENTS(where(temp_q2 lt q2_bins(0)))
      IF TOTAL(where(temp_q2 ge q2_bins(n_q2bins-1))) ge 0 THEN $
         twod_bin(n_qbins,n_q2bins)=twod_bin(n_qbins,n_q2bins)+N_ELEMENTS(where(temp_q2 ge q2_bins(n_q2bins-1)))
      mean_qbin(n_qbins)=MEAN(temp_q2,/NaN)
      median_qbin(n_qbins)=MEDIAN(temp_q2)
   ENDIF

   FOR k=0,n_q2bins-2 DO BEGIN
      IF TOTAL(where(q2_vint ge q2_bins(k) and q2_vint lt q2_bins(k+1))) ge 0 THEN BEGIN
         pts=where(q2_vint ge q2_bins(k) and q2_vint lt q2_bins(k+1)) 
         temp_q=q_anom_vint[pts]
         mean_q2bin(k+1)=MEAN(temp_q,/NaN)
         median_q2bin(k+1)=MEDIAN(temp_q[where(FINITE(temp_q) eq 1)])
      ENDIF ELSE BEGIN
         mean_q2bin(k+1)=!Values.F_NaN
         median_q2bin(k+1)=!Values.F_NaN
      ENDELSE
   ENDFOR
   IF TOTAL(where(q2_vint lt q2_bins(0))) ge 0 THEN BEGIN
      pts=where(q2_vint lt q2_bins(0))
      temp_q=q_anom_vint[pts]
      mean_q2bin(0)=MEAN(temp_q,/NaN)
      median_q2bin(0)=MEDIAN(temp_q[where(FINITE(temp_q) eq 1)])
   ENDIF ELSE BEGIN
      mean_q2bin(0)=!Values.F_NaN
      median_q2bin(0)=!Values.F_NaN
   ENDELSE
   IF TOTAL(where(q2_vint ge q2_bins(n_q2bins-1))) ge 0 THEN BEGIN
      pts=where(q2_vint ge q2_bins(n_q2bins-1))
      temp_q=q_anom_vint[pts]
      mean_q2bin(n_q2bins)=MEAN(temp_q,/NaN)
      median_q2bin(n_q2bins)=MEDIAN(temp_q[where(FINITE(temp_q) eq 1)])
   ENDIF ELSE BEGIN
      mean_q2bin(n_q2bins)=!Values.F_NaN
      median_q2bin(n_q2bins)=!Values.F_NaN
   ENDELSE

   total_twod_bin=TOTAL(twod_bin)
   print,total_twod_bin
   twod_bin=twod_bin/total_twod_bin
   
   frac_levels=['0.001','0.002','0.003','0.004','0.005','0.006','0.007','0.008','0.01','0.012','0.014','0.016','0.018','0.020','0.022']
   quad_frac=fltarr(4,3)        ; [quadrant, (wrt all, wrt x half, wrt y half)] do not count small anomalies (middle row and column)
   FOR j=0,3 DO BEGIN
      CASE j OF 
         0 : BEGIN
            x_range=[0,NEAREST(q_bins,0)-1]
            y_range=[0,NEAREST(q2_bins,0)-2]
         END
         1 : BEGIN
            x_range=[NEAREST(q_bins,0)+1,n_qbins]
            y_range=[0,NEAREST(q2_bins,0)-2]
         END
         2 : BEGIN
            x_range=[0,NEAREST(q_bins,0-1)]
            y_range=[NEAREST(q2_bins,0),n_q2bins]
         END
         3 : BEGIN
            x_range=[NEAREST(q_bins,0)+1,n_qbins]
            y_range=[NEAREST(q2_bins,0),n_q2bins]
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
         offset=3
      ENDIF ELSE IF j eq 1 THEN BEGIN
         offset=1
      ENDIF ELSE IF j eq 2 THEN BEGIN
         offset=-1
      ENDIF ELSE IF j eq 3 THEN $
         offset=-3
      quad_frac(j,2)=quad_frac(j,0)/(quad_frac(j,0)+quad_frac(j+offset,0))
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/q_bin/mjo_diabatic_plot_Q2physics_q_vint.q_anom_leadtime.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=200,SPACE2=2000,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110,XSIZE=17000,YSIZE=17000
   GSET,XMIN=0,XMAX=n_qbins+1,YMIN=0,YMAX=n_q2bins+1
   CS,SCALE=24,NCOLS=N_ELEMENTS(frac_levels)+1,white=[2]
   LEVS,MANUAL=frac_levels
   CON,X=indgen(n_qbins+1)+0.5,Y=indgen(n_q2bins+1)+0.5,FIELD=twod_bin,/BLOCK,/NOLINES,$
       CB_TITLE='Fraction of points'
   GPLOT,X=[n_qbins/2+0.5,n_qbins/2+0.5],Y=[0,n_q2bins+1],STYLE=1
   GPLOT,X=[0,n_qbins+1],Y=[NEAREST(q2_bins,0)+0.5,NEAREST(q2_bins,0)+0.5],STYLE=1
   mean_plot=fltarr(n_qbins+1)
   median_plot=fltarr(n_qbins+1)
   FOR j=0,n_qbins DO BEGIN
      mean_plot(j)=NEAREST(q2_bins,mean_qbin(j))+1
      IF mean_plot(j) lt n_q2bins-1 THEN $
         mean_plot(j)=mean_plot(j)+(mean_qbin(j)-q2_bins(mean_plot(j)))/(q2_bins(mean_plot(j)+1)-q2_bins(mean_plot(j)))+1
      median_plot(j)=NEAREST(q2_bins,median_qbin(j))+1
      IF median_plot(j) lt n_q2bins-1 THEN $
         median_plot(j)=median_plot(j)+(median_qbin(j)-q2_bins(median_plot(j)))/(q2_bins(median_plot(j)+1)-q2_bins(median_plot(j)))+1
   ENDFOR
   GPLOT,X=indgen(n_qbins+1)+0.5,Y=mean_plot,STYLE=0,COL=FSC_COLOR('red')
   GPLOT,X=indgen(n_qbins+1)+0.5,Y=median_plot,STYLE=0,COL=FSC_COLOR('orange')
   mean_plot=fltarr(n_q2bins+1)
   median_plot=fltarr(n_q2bins+1)
   FOR j=0,n_q2bins DO BEGIN
      mean_plot(j)=NEAREST(q_bins,mean_q2bin(j))+1
      IF mean_plot(j) lt n_qbins-1 THEN $
         mean_plot(j)=mean_plot(j)+(mean_q2bin(j)-q_bins(mean_plot(j)))/(q_bins(mean_plot(j)+1)-q_bins(mean_plot(j)))+1
      median_plot(j)=NEAREST(q_bins,median_q2bin(j))+1
      IF median_plot(j) lt n_qbins-1 THEN $
         median_plot(j)=median_plot(j)+(median_q2bin(j)-q_bins(median_plot(j)))/(q_bins(median_plot(j)+1)-q_bins(median_plot(j)))+1
   ENDFOR
   GPLOT,Y=indgen(n_q2bins+1)+0.5,X=mean_plot,STYLE=0,COL=FSC_COLOR('purple')
   GPLOT,Y=indgen(n_q2bins+1)+0.5,X=median_plot,STYLE=0,COL=FSC_COLOR('violetred')   

   FOR j=0,2 DO BEGIN
      GPLOT,X=1+j*2,Y=1,TEXT=STRMID(STRTRIM(STRING(quad_frac(0,j)),1),0,5)
      GPLOT,X=n_qbins-5+j*2,Y=1,TEXT=STRMID(STRTRIM(STRING(quad_frac(1,j)),1),0,5)
      GPLOT,X=n_qbins-5+j*2,Y=n_q2bins,TEXT=STRMID(STRTRIM(STRING(quad_frac(2,j)),1),0,5)
      GPLOT,X=1+j*2,Y=n_q2bins,TEXT=STRMID(STRTRIM(STRING(quad_frac(3,j)),1),0,5)
   ENDFOR
   AXES,XVALS=indgen(n_qbins+2),YVALS=indgen(n_q2bins+2),$
        XLABELS=['<'+STRMID(STRTRIM(STRING(q_bins(0)),1),0,5),STRMID(STRTRIM(STRING(q_bins),1),0,4),$
                 '>'+STRMID(STRTRIM(STRING(q_bins(n_qbins-1)),1),0,4)],$
        YLABELS=['<'+STRMID(STRTRIM(STRING(q2_bins(0)),1),0,5),STRMID(STRTRIM(STRING(q2_bins),1),0,4),$
                 '>'+STRMID(STRTRIM(STRING(q2_bins(n_q2bins-1)),1),0,4)],$
        ORIENTATION=30,XTITLE='Vint anom in q (from leadtime-dependent mean; g kg!U-1!N)',$
        YTITLE='Vertical integrated total moistening rate (g kg!U-1!N day!U-1!N)'
   GLEGEND,labels=['Median Q2 in q bin','Mean Q2 in q bin','Median q in Q2 bin','Mean q in Q2 bin'],$
           COL=[FSC_COLOR('orange'),FSC_COLOR('red'),FSC_COLOR('violetred'),FSC_COLOR('purple')],$
           LEGXOFFSET=7500,LEGYOFFSET=4000
   PSCLOSE,/NOVIEW
ENDFOR

STOP
            
END
