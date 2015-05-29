PRO mjo_diabatic_precip_bin_dTdt_twod_withlead_ratios,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

; Aggregates statistics over ALL lead times specified
; Mask type: "ocean" or "land"

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','metum','miroc','mri','nrl','nasa','giss','spcam','cancm4','nicam','cam5','cam5zm','ecearth','cnrm_atmos']
IF mask_type ne 'ocean' and mask_type ne 'land' THEN BEGIN
   print,'You must set mask_type equal to either ocean or land'
   STOP
END

; Specify level for ratio above/below (in hPa)
dividing_level=550
dividing_pct=50

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

precip_bins=[0.1,0.3,0.6,1.0,1.5,2.0,3.0,5.0,7.0,9.0,12.0,16.0,20,25,30]
ratio_bins=[0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1/0.9,1/0.8,1/0.7,1/0.6,1/0.5,1/0.4,1/0.3,1/0.2,1/0.1]
mylevs=['0.02','0.04','0.06','0.08','0.10','0.15','0.20','0.25','0.30','0.40','0.50','0.60','0.70','0.80','0.90']
mylevs_diff=['-0.38','-0.34','-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30','0.34','0.38']
n_precip_bins=N_ELEMENTS(precip_bins+1)
n_ratio_bins=N_ELEMENTS(ratio_bins+1)

IF KEYWORD_SET(plot_yotc) THEN BEGIN
   ecmwf_start_offset=(start_julian-274)*8;-4
   ecmwf_stop_offset=(stop_julian+MAX(lead_times)-274)*8;-4
   trmm_start_offset=(start_julian-274)*8
   trmm_stop_offset=(stop_julian+MAX(lead_times)-274)*8
   ecmwf_count=ecmwf_stop_offset-ecmwf_start_offset+1
   trmm_count=ecmwf_stop_offset-ecmwf_start_offset+1
   
   print,ecmwf_start_offset,ecmwf_count
   print,trmm_start_offset,trmm_count
   ecmwf_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnt.nc'
   ecmwf_longitude=OPEN_AND_EXTRACT(ecmwf_infile,'longitude')
   ecmwf_latitude=OPEN_AND_EXTRACT(ecmwf_infile,'latitude')
   ecmwf_z=OPEN_AND_EXTRACT(ecmwf_infile,'p')
   DEFINE_BOUNDARIES,box,ecmwf_latitude,ecmwf_longitude,ecmwf_box_tx,/LIMIT
   ecmwf_nlon=N_ELEMENTS(ecmwf_longitude)
   ecmwf_nlat=N_ELEMENTS(ecmwf_latitude)
   ecmwf_nz=N_ELEMENTS(ecmwf_z)

   ecmwf_dTdt=OPEN_AND_EXTRACT(ecmwf_infile,'tnt_physics',offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0,ecmwf_start_offset],$
                            count=[ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count])*8.
   
   trmm_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.pr.nc'
   trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
   trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
   DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
   trmm_nlon=N_ELEMENTS(trmm_longitude)
   trmm_nlat=N_ELEMENTS(trmm_latitude)

   trmm_precip=OPEN_AND_EXTRACT(trmm_infile,'TP',offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_start_offset],$
                                count=[trmm_nlon,trmm_nlat,trmm_count])*8.*1000.;*24.
   IF TOTAL(where(ABS(trmm_precip) ge 1000)) ge 0 THEN $
      trmm_precip[where(ABS(trmm_precip) ge 1000)]=!Values.F_NaN

   ecmwf_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
            
   ecmwf_mask=REFORM(OPEN_AND_EXTRACT(ecmwf_mask_infile(0),'landsea',$
                                      offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0],count=[ecmwf_nlon,ecmwf_nlat,1]))

   ecmwf_ratio_binned=fltarr(n_precip_bins+1,n_ratio_bins+1)
   n_pts=fltarr(n_precip_bins+1)
   dividing_z=NEAREST(ecmwf_z,dividing_level)
   ecmwf_dividing_binned=fltarr(n_precip_bins+1,ecmwf_nz)
  
   FOR m=0,n_times_per_day-1 DO BEGIN
      temp=REFORM(trmm_precip(*,*,m))
      IF mask_type eq 'ocean' THEN BEGIN
         temp[where(ecmwf_mask eq 1)]=!Values.F_NaN
      ENDIF ELSE IF mask_type eq 'land' THEN $
         temp[where(ecmwf_mask eq 0)]=!Values.F_NaN
      trmm_precip(*,*,m)=temp
   ENDFOR   

   FOR m=0,n_precip_bins DO BEGIN
      CASE m OF
         0 : BEGIN
            valid_points=where(trmm_precip lt precip_bins(0))
         END
         n_precip_bins : BEGIN
            valid_points=where(trmm_precip ge precip_bins(n_precip_bins-1))
         END
         else : BEGIN
            valid_points=where(trmm_precip ge precip_bins(m-1) and trmm_precip lt precip_bins(m))
         END
      ENDCASE
      IF TOTAL(valid_points) ne -1 THEN BEGIN
         n_valid_points=N_ELEMENTS(valid_points)
         dTdt_profiles=fltarr(n_valid_points,ecmwf_nz)
         ratios=fltarr(n_valid_points)
         FOR n=0,ecmwf_nz-1 DO BEGIN
            temp=REFORM(ecmwf_dTdt(*,*,n,*))
            dTdt_profiles(*,n)=temp[valid_points]
         ENDFOR
         FOR n=0,n_valid_points-1 DO BEGIN            
            dTdt_below=0.
            dTdt_above=0.
            dp_above=0.
            FOR p=1,dividing_z DO BEGIN
               dTdt_above=dTdt_above+dTdt_profiles(n,p)*(ecmwf_z(p)-ecmwf_z(p-1))
               dp_above=dp_above+ecmwf_z(p)-ecmwf_z(p-1)
            ENDFOR
            dTdt_above=dTdt_above/dp_above
            dp_below=0.
            FOR p=dividing_z+1,ecmwf_nz-1 DO BEGIN
               dTdt_below=dTdt_below+dTdt_profiles(n,p)*(ecmwf_z(p)-ecmwf_z(p-1))
               dp_below=dp_below+ecmwf_z(p)-ecmwf_z(p-1)
            ENDFOR
            dTdt_below=dTdt_below/dp_below
            ratios(n)=ABS(dTdt_above/dTdt_below)
            dTdt_vint=(dTdt_above*dp_above+dTdt_below*dp_below)/(dp_above+dp_below)
            temp_vint=0.
            dp_sum=dp_below+dp_above
            p=1
            flag=0
            WHILE flag ne 1 DO BEGIN
               temp_vint=temp_vint+dTdt_profiles(n,p)*(ecmwf_z(p)-ecmwf_z(p-1))/dp_sum
               IF ABS(temp_vint) gt ABS(dTdt_vint*dividing_pct/100.) THEN BEGIN
                  ecmwf_dividing_binned(m,p)=ecmwf_dividing_binned(m,p)+1
                  flag=1
               ENDIF
               p=p+1
            ENDWHILE
         ENDFOR
         FOR n=0,n_ratio_bins DO BEGIN
            CASE n OF
               0 : BEGIN
                  ratio_points=where(ratios lt ratio_bins(0))
               END
               n_ratio_bins : BEGIN
                  ratio_points=where(ratios ge ratio_bins(n_ratio_bins-1))
               END
               else : BEGIN
                  ratio_points=where(ratios ge ratio_bins(n-1) and ratios lt ratio_bins(n))
               END
            ENDCASE            
            IF TOTAL(ratio_points) ne -1 THEN $
               ecmwf_ratio_binned(m,n)=ecmwf_ratio_binned(m,n)+N_ELEMENTS(ratio_points)
         ENDFOR
      ENDIF
   ENDFOR
   FOR m=0,n_precip_bins DO BEGIN
      total_thisbin=TOTAL(ecmwf_ratio_binned(m,*))
      ecmwf_ratio_binned(m,*)=ecmwf_ratio_binned(m,*)/FLOAT(total_thisbin)
      total_thisbin=TOTAL(ecmwf_dividing_binned(m,*))
      ecmwf_dividing_binned(m,*)=ecmwf_dividing_binned(m,*)/FLOAT(total_thisbin)
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=0,YMAX=n_ratio_bins+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=indgen(n_ratio_bins+1)+0.5,X=indgen(n_precip_bins+1)+0.5,FIELD=ecmwf_ratio_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of ratio of Q1-Qr above/below '+STRTRIM(STRING(dividing_level),1)+' hPa from YoTC 3-24h fcst - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)  
   AXES,YVALS=indgen(n_ratio_bins+2),XVALS=indgen(n_precip_bins+2),ORIENTATION=30,$
        YTITLE='Ratio above/below '+STRTRIM(STRING(dividing_level),1)+' hPa',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2,$
        YLABELS=['< '+STRMID(STRTRIM(STRING(ratio_bins(0)),1),0,4),STRMID(STRTRIM(STRING(ratio_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(ratio_bins(n_ratio_bins-1)),1),0,4)]
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.ht_pct.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=ecmwf_z,X=indgen(n_precip_bins+1)+0.5,FIELD=ecmwf_dividing_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of pressure at which '+STRTRIM(STRING(dividing_pct),1)+'% of total heating occurs above this level - YoTC -'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_precip_bins+2),ORIENTATION=30,YTITLE='Pressure',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2
   PSCLOSE,/NOVIEW

ENDIF   

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
         dTdt_multiplier=86400.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e10
         has_landsea=1         
         reverse_level=0
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         file_desc='GEOS5_AGCM'
         model_desc='GEOS5_AGCM'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=10000.
         has_landsea=0
         reverse_level=0
      END    
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         file_desc='MRI-AGCM'
         model_desc='MRI-AGCM'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=10000.
         has_landsea=0
         reverse_level=0
      END  
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_desc='miroc5'
         model_desc='MIROC5'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=999.
         has_landsea=0
         reverse_level=0
      END    
      'nrl' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         file_desc='NGEM01'
         model_desc='NRL_NavGEM01'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=10000.
         has_landsea=0
         reverse_level=1
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
         dTdt_multiplier=72.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=100000.
         has_landsea=1
         reverse_level=0
      END      
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_desc='ModelE'
         model_desc='GISS_ModelE2'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dTdt_multiplier=86400.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=0
         reverse_level=0
      END      
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_desc='CanCM4'
         model_desc='CCCma_CanCM4'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e4
         has_landsea=0
         reverse_level=0
      END  
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         file_desc='SPCAM3.0'
         model_desc='SPCAM3.0'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev_p'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=0
         reverse_level=0
      END 
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         file_desc='NCAR.CAM5'
         model_desc='NCAR_CAM5'
         valid_dates=cam5_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='levels'
         dTdt_multiplier=86400.
         precip_multiplier=86400.*1000.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=1
         reverse_level=0
      END 
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_desc='CAM5ZMMicroCAPT'
         model_desc='LLNL_CAM5ZM'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=0
         reverse_level=0
      END 
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         file_desc='ecearth3'
         model_desc='SHMI_ECEarth3'
         valid_dates=ecearth_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=0.01
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
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=0
         reverse_level=0
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
         dTdt_multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
         has_landsea=0
         reverse_level=0
      END       
   ENDCASE
   
   grid_flag=0
   start_position=REFORM(where(standard_valid_dates eq start_date))
   date_offset=where(dates eq start_date)
   n_days=where(standard_valid_dates eq stop_date)-start_position
   print,n_days

   ratio_binned=fltarr(n_precip_bins+1,n_ratio_bins+1)
   n_pts=fltarr(n_precip_bins+1)
   
   FOR j=0,n_days(0)-1 DO BEGIN
      ;print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            ;print,initial_date
            model_dTdt_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnt_physics.'+REFORM(initial_date)+'.00Z.nc'
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_pr_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
               model_pr_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               model_z=OPEN_AND_EXTRACT(model_dTdt_infile(0),level_name)*level_multiplier
               DEFINE_BOUNDARIES,box,model_pr_latitude,model_pr_longitude,model_pr_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_pr_longitude)
               model_nlat=N_ELEMENTS(model_pr_latitude)     
               model_nz=N_ELEMENTS(model_z)
               dividing_z=NEAREST(model_z,dividing_level)
               dividing_binned=fltarr(n_precip_bins+1,model_nz)
               model_dTdt_longitude=OPEN_AND_EXTRACT(model_dTdt_infile(0),longitude_name)
               model_dTdt_latitude=OPEN_AND_EXTRACT(model_dTdt_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_dTdt_latitude,model_dTdt_longitude,model_dTdt_box_tx,/LIMIT
               IF has_landsea eq 1 THEN BEGIN
                  model_mask_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.landsea.'+REFORM(initial_date)+'.00Z.nc'
                  model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                                     offset=[model_pr_box_tx(1),model_pr_box_tx(0),0],count=[model_nlon,model_nlat,1]))
               ENDIF ELSE BEGIN
                  model_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
                  model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                                     offset=[model_pr_box_tx(1),model_pr_box_tx(0),0],count=[model_nlon,model_nlat,1]))
               ENDELSE
               grid_flag=1
               model_precip=fltarr(model_nlon,model_nlat,n_times_per_day)
               model_dTdt=fltarr(model_nlon,model_nlat,model_nz,n_times_per_day)    
               n_pts=fltarr(n_precip_bins+1)
            ENDIF
            model_precip(*,*,*)=$            
               OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                offset=[model_pr_box_tx(1),model_pr_box_tx(0),lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])*precip_multiplier
            FOR m=0,n_times_per_day-1 DO BEGIN
               temp=REFORM(model_precip(*,*,m))
               IF mask_type eq 'ocean' THEN BEGIN
                  IF TOTAL(where(model_mask eq 1)) ge 0 THEN $
                     temp[where(model_mask eq 1)]=!Values.F_NaN
               ENDIF ELSE IF mask_type eq 'land' THEN $
                  temp[where(model_mask ne 1)]=!Values.F_NaN
               model_precip(*,*,m)=temp
            ENDFOR
            model_dTdt(*,*,*,*)=$
               OPEN_AND_EXTRACT(model_dTdt_infile(0),'tnt_physics',$
                                offset=[model_dTdt_box_tx(1),model_dTdt_box_tx(0),0,lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,model_nz,n_times_per_day])*dTdt_multiplier
            IF TOTAL(where(ABS(model_dTdt) ge missing_value)) gt 0 THEN $
               model_dTdt[where(ABS(model_dTdt) ge missing_value)]=!Values.F_NaN
            
            FOR m=0,n_precip_bins DO BEGIN
               CASE m OF
                  0 : BEGIN
                     valid_points=where(model_precip lt precip_bins(0))
                  END
                  n_precip_bins : BEGIN
                     valid_points=where(model_precip ge precip_bins(n_precip_bins-1))
                  END
                  else : BEGIN
                     valid_points=where(model_precip ge precip_bins(m-1) and model_precip lt precip_bins(m))
                  END
               ENDCASE
               IF TOTAL(valid_points) ne -1 THEN BEGIN
                  n_valid_points=N_ELEMENTS(valid_points)
                  dTdt_profiles=fltarr(n_valid_points,model_nz)
                  ratios=fltarr(n_valid_points)
                  FOR n=0,model_nz-1 DO BEGIN
                     temp=REFORM(model_dTdt(*,*,n,*))
                     dTdt_profiles(*,n)=temp[valid_points]
                  ENDFOR
                  FOR n=0,n_valid_points-1 DO BEGIN            
                     dTdt_below=0.
                     dTdt_above=0.
                     dp_above=0.
                     CASE reverse_level OF 
                        1 : BEGIN
                           above_lower=1
                           above_upper=dividing_z
                           below_lower=dividing_z+1
                           below_upper=model_nz-1
                           increment=-1
                           p_start=model_nz-1
                           p_end=0
                        END
                        0 : BEGIN
                           above_lower=dividing_z
                           above_upper=model_nz-2
                           below_lower=0
                           below_Upper=dividing_z-1
                           increment=1                           
                           p_start=0
                           p_end=model_nz-1
                        END
                     ENDCASE
                     FOR p=above_lower,above_upper DO BEGIN
                        dTdt_above=dTdt_above+dTdt_profiles(n,p)*(model_z(p)-model_z(p+increment))
                        dp_above=dp_above+model_z(p)-model_z(p+increment)
                     ENDFOR
                     dTdt_above=dTdt_above/dp_above
                     dp_below=0.
                     FOR p=below_lower,below_upper DO BEGIN
                        dTdt_below=dTdt_below+dTdt_profiles(n,p)*(model_z(p)-model_z(p+increment))
                        dp_below=dp_below+model_z(p)-model_z(p+increment)
                     ENDFOR
                     dTdt_below=dTdt_below/dp_below
                     ratios(n)=ABS(dTdt_above/dTdt_below)
                     dTdt_vint=(dTdt_above*dp_above+dTdt_below*dp_below)/(dp_above+dp_below)
                     temp_vint=0.
                     dp_sum=dp_below+dp_above
                     flag=0
                     p=p_start
                     WHILE flag ne 1 DO BEGIN
                        temp_vint=temp_vint+dTdt_profiles(n,p)*(model_z(p)-model_z(p+increment))/dp_sum
                        IF ABS(temp_vint) gt ABS(dTdt_vint*dividing_pct/100.) THEN BEGIN
                           dividing_binned(m,p)=dividing_binned(m,p)+1
                           flag=1
                        ENDIF
                        p=p+increment
                        IF p eq p_end THEN flag=1
                     ENDWHILE
                  ENDFOR
                  FOR n=0,n_ratio_bins DO BEGIN
                     CASE n OF
                        0 : BEGIN
                           ratio_points=where(ratios lt ratio_bins(0))
                        END
                        n_ratio_bins : BEGIN
                           ratio_points=where(ratios ge ratio_bins(n_ratio_bins-1))
                        END
                        else : BEGIN
                           ratio_points=where(ratios ge ratio_bins(n-1) and ratios lt ratio_bins(n))
                        END
                     ENDCASE            
                     IF TOTAL(ratio_points) ne -1 THEN $
                        ratio_binned(m,n)=ratio_binned(m,n)+N_ELEMENTS(ratio_points)
                  ENDFOR
               ENDIF
            ENDFOR
         ENDFOR
      ENDIF
   ENDFOR
   FOR m=0,n_precip_bins DO BEGIN
      total_thisbin=TOTAL(ratio_binned(m,*))
      ratio_binned(m,*)=ratio_binned(m,*)/FLOAT(total_thisbin)
      total_thisbin=TOTAL(dividing_binned(m,*))
      dividing_binned(m,*)=dividing_binned(m,*)/FLOAT(total_thisbin)
   ENDFOR
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=0,YMAX=n_ratio_bins+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=indgen(n_ratio_bins+1)+0.5,X=indgen(n_precip_bins+1)+0.5,FIELD=ratio_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of ratio of Q1-Qr above/below '+STRTRIM(STRING(dividing_level),1)+' hPa from '+model_desc+' - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+STRTRIM(STRING(MIN(lead_times)),1)+'-'+$
       STRTRIM(STRING(MAX(lead_times)),1)
   GPLOT,X=[0,n_precip_bins+1],Y=[NEAREST(ratio_bins,1)+1,NEAREST(ratio_bins,1)+1],STYLE=1,COL=FSC_COLOR('black')
   AXES,YVALS=indgen(n_ratio_bins+2),XVALS=indgen(n_precip_bins+2),ORIENTATION=30,YTITLE='Ratio above/below '+STRTRIM(STRING(dividing_level),1)+' hPa',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2,$
        YLABELS=['< '+STRMID(STRTRIM(STRING(ratio_bins(0)),1),0,4),STRMID(STRTRIM(STRING(ratio_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(ratio_bins(n_ratio_bins-1)),1),0,4)]
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.ht_pct.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=model_z,X=indgen(n_precip_bins+1)+0.5,FIELD=dividing_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of pressure at which '+STRTRIM(STRING(dividing_pct),1)+'% of heating occurs above this level - '+model_desc+' - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+STRTRIM(STRING(MIN(lead_times)),1)+'-'+$
       STRTRIM(STRING(MAX(lead_times)),1)
   AXES,YVALS=model_z,XVALS=indgen(n_precip_bins+2),ORIENTATION=30,YTITLE='Ratio above/below '+STRTRIM(STRING(dividing_level),1)+' hPa',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2       
   PSCLOSE,/NOVIEW

   diff_ecmwf_ratio=ratio_binned-ecmwf_ratio_binned
   diff_ecmwf_dividing=fltarr(n_precip_bins+1,ecmwf_nz)
   CASE reverse_level OF
      1 : BEGIN
         model_start=0
         increment=1
      END
      0 : BEGIN
         model_start=model_nz-1
         increment=-1
      END
   ENDCASE
   p=model_start
   FOR j=0,ecmwf_nz-1 DO BEGIN
      print,model_z(p),ecmwf_z(j)
      IF ABS(model_z(p)-ecmwf_z(j)) lt 0.01 THEN BEGIN
         diff_ecmwf_dividing(*,j)=dividing_binned(*,p)-ecmwf_dividing_binned(*,j)
      ENDIF ELSE BEGIN
         this_start=p
         WHILE ABS(model_z(p)-ecmwf_z(j)) ge 0.01 and model_z(p) lt ecmwf_z(j) DO BEGIN
            p=p+increment
         ENDWHILE
         this_stop=p
         FOR k=0,n_precip_bins DO $
            diff_ecmwf_dividing(k,j)=TOTAL(dividing_binned(k,MIN([this_start,this_stop]):MAX([this_start,this_stop])))-ecmwf_dividing_binned(k,j)
         print,model_z(MIN([this_start,this_stop]):MAX([this_start,this_stop])),ecmwf_z(j)
         p=this_stop
      ENDELSE
      p=p+increment
      print,'------'
   ENDFOR
      
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.'+model_desc+'-minus-yotc_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=0,YMAX=n_ratio_bins+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=12
   LEVS,MANUAL=mylevs_diff
   CON,Y=indgen(n_ratio_bins+1)+0.5,X=indgen(n_precip_bins+1)+0.5,FIELD=diff_ecmwf_ratio,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of ratio of Q1-Qr above/below '+STRTRIM(STRING(dividing_level),1)+' hPa from '+model_desc+' - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+STRTRIM(STRING(MIN(lead_times)),1)+'-'+$
       STRTRIM(STRING(MAX(lead_times)),1)
   GPLOT,X=[0,n_precip_bins+1],Y=[NEAREST(ratio_bins,1)+1,NEAREST(ratio_bins,1)+1],STYLE=1,COL=FSC_COLOR('black')
   AXES,YVALS=indgen(n_ratio_bins+2),XVALS=indgen(n_precip_bins+2),ORIENTATION=30,YTITLE='Ratio above/below '+STRTRIM(STRING(dividing_level),1)+' hPa',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2,$
        YLABELS=['< '+STRMID(STRTRIM(STRING(ratio_bins(0)),1),0,4),STRMID(STRTRIM(STRING(ratio_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(ratio_bins(n_ratio_bins-1)),1),0,4)]
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_ratios.ht_pct.'+model_desc+'-minus-yotc_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,white=12
   LEVS,MANUAL=mylevs_diff
   CON,Y=ecmwf_z,X=indgen(n_precip_bins+1)+0.5,FIELD=diff_ecmwf_dividing,/BLOCK,/NOLINES,$
       CB_TITLE='Probability (unitless)',$
       TITLE='PDF of pressure at which '+STRTRIM(STRING(dividing_pct),1)+'% of heating occurs above this level - '+model_desc+' - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+STRTRIM(STRING(MIN(lead_times)),1)+'-'+$
       STRTRIM(STRING(MAX(lead_times)),1)
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_precip_bins+2),ORIENTATION=30,YTITLE='Ratio above/below '+STRTRIM(STRING(dividing_level),1)+' hPa',$
        XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                 STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2       
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

         
