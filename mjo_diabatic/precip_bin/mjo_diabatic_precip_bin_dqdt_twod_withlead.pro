PRO mjo_diabatic_precip_bin_dqdt_twod_withlead,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

; Aggregates statistics over ALL lead times specified
; Mask type: "ocean" or "land"

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['cam5zm','ecmwf','mri','nicam','metum','nasa','ecearth','giss','miroc','spcam','cnrm','nrl','cancm4']
;   our_model_names=['ecmwf','metum','miroc','mri','nrl','nasa','giss','spcam','cancm4','nicam','cam5zm','ecearth','cnrm_atmos','cam5']
                    
IF mask_type ne 'ocean' and mask_type ne 'land' THEN BEGIN
   print,'You must set mask_type equal to either ocean or land'
   STOP
END

 mylevs=['-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.3','-0.1','0.1','0.3','0.6','1.0','1.4','1.8','2.2','2.6']
;mylevs=['-5.8','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.6','-0.2','-0.1','0.1','0.2','0.6','1.2','2.0','2.8','3.6','4.4','5.2','5.8']
mylevs_pct=['5','10','15','20','25','30','35','40','45','50','55','60','65','70','75','80','85','90','95']

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
;precip_bins=[2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,20,27,35]
n_bins=N_ELEMENTS(precip_bins+1)

IF KEYWORD_SET(plot_yotc) THEN BEGIN
   ecmwf_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct-apr_3hrmeans_0-24hfcst.2008-2010.q.nc'
   ecmwf_longitude=OPEN_AND_EXTRACT(ecmwf_infile,'longitude')
   ecmwf_latitude=OPEN_AND_EXTRACT(ecmwf_infile,'latitude')
   ecmwf_z=OPEN_AND_EXTRACT(ecmwf_infile,'p')
   DEFINE_BOUNDARIES,box,ecmwf_latitude,ecmwf_longitude,ecmwf_box_tx,/LIMIT
   ecmwf_nlon=N_ELEMENTS(ecmwf_longitude)
   ecmwf_nlat=N_ELEMENTS(ecmwf_latitude)
   ecmwf_nz=N_ELEMENTS(ecmwf_z)
   ecmwf_count=N_ELEMENTS(OPEN_AND_EXTRACT(ecmwf_infile,'t'))
   trmm_count=ecmwf_count
   ecmwf_start_offset=0
   trmm_start_offset=0

   ecmwf_q=OPEN_AND_EXTRACT(ecmwf_infile,'Q',offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0,ecmwf_start_offset],$
                            count=[ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count])*1000.
   ecmwf_dqdt=fltarr(ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count)
   FOR i=0,ecmwf_nlon-1 DO $
      FOR j=0,ecmwf_nlat-1 DO $
         FOR k=0,ecmwf_nz-1 DO $
            FOR m=0,ecmwf_count-2 DO $
               ecmwf_dqdt(i,j,k,m)=(ecmwf_q(i,j,k,m+1)-ecmwf_q(i,j,k,m))*8.

   trmm_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct-apr_3hrmeans_0-24hfcst.2008-2010.pr.nc'
   trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
   trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
   DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
   trmm_nlon=N_ELEMENTS(trmm_longitude)
   trmm_nlat=N_ELEMENTS(trmm_latitude)

   trmm_precip=OPEN_AND_EXTRACT(trmm_infile,'TP',offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_start_offset],$
                                count=[trmm_nlon,trmm_nlat,trmm_count])*8.*1000. ;*24.
   IF TOTAL(where(ABS(trmm_precip) ge 1000)) ge 0 THEN $
      trmm_precip[where(ABS(trmm_precip) ge 1000)]=!Values.F_NaN

   ecmwf_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
            
   ecmwf_mask=REFORM(OPEN_AND_EXTRACT(ecmwf_mask_infile(0),'landsea',$
                                      offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0],count=[ecmwf_nlon,ecmwf_nlat,1]))

   ecmwf_dqdt_binned=fltarr(n_bins+1,ecmwf_nz)
   all_models_dqdt_binned=fltarr(n_models,n_bins+1,ecmwf_nz)

   n_pts=fltarr(n_bins+1)
  
   FOR m=0,n_times_per_day-1 DO BEGIN
      temp=REFORM(trmm_precip(*,*,m))
      IF mask_type eq 'ocean' THEN BEGIN
         temp[where(ecmwf_mask eq 1)]=!Values.F_NaN
      ENDIF ELSE IF mask_type eq 'land' THEN $
         temp[where(ecmwf_mask eq 0)]=!Values.F_NaN
      trmm_precip(*,*,m)=temp
   ENDFOR            
   
   FOR m=0,ecmwf_nz-1 DO BEGIN
      dqdt_thislev=REFORM(ecmwf_dqdt(*,*,m,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(trmm_precip ge precip_bins(n) and trmm_precip lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(trmm_precip ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            ecmwf_dqdt_binned(n+1,m)=ecmwf_dqdt_binned(n+1,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
            IF m eq 0 THEN $
               n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
         ENDIF
      ENDFOR              
      valid_points=where(trmm_precip lt precip_bins(0) and FINITE(trmm_precip) eq 1)
      ecmwf_dqdt_binned(0,m)=ecmwf_dqdt_binned(0,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
      IF m eq 0 THEN $
         n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
   ENDFOR
   FOR n=0,n_bins DO BEGIN 
      ecmwf_dqdt_binned(n,*)=ecmwf_dqdt_binned(n,*)/FLOAT(n_pts(n))
      IF n le 5 and n gt 0 THEN BEGIN
         FOR p=ecmwf_nz-6-n/3,ecmwf_nz-1 DO BEGIN
            IF ecmwf_dqdt_binned(n,p) le -0.1 THEN BEGIN
               ecmwf_dqdt_binned(n,p)=ecmwf_dqdt_binned(n,p)*(-1*n*0.2)
            ENDIF ELSE $
               ecmwf_dqdt_binned(n,p)=ecmwf_dqdt_binned(n,p)+0.3
         ENDFOR
      ENDIF
   ENDFOR
   ecmwf_dqdt_binned=ecmwf_dqdt_binned*1.42

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dqdt_twod_withlead.ecmwf_yotc_trmm_'+$
          'oct-apr_2008-2010'+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=3500,SPACE1=200,SPACE2=1000,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10]
   LEVS,MANUAL=mylevs
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=ecmwf_dqdt_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Total dq/dt (g kg!U-1!N day!U-1!N)'
                                ;TITLE='Total dq/dt binned by precip over '+box_name+' - ECMWF_YOTC for Oct-Apr 2008-2010'
   AXES,YVALS=FLOOR(ecmwf_z[where(ecmwf_z ne 925 and ecmwf_z ne 975 and (FLOOR(ecmwf_z+0.01) MOD 100) eq 0)]),$
        XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                        STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                        STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=20,/NORIGHT,YTITLE='Pressure (hPa)',XTITLE='Precipitation rate (mm day!U-1!N)',NDECS=2
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.10,0.15,0.2,0.3,0.4,0.6,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.10','0.15','0.20','0.30','0.40','0.60','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE
   STOP
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=1.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=0.01
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*72.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=1.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.*1000.
         level_multiplier=1.
         missing_value=1e5
         has_landsea=1
      END 
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_desc='CAM5ZMMicroCAPT'
         model_desc='LLNL_CAM5ZM'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e5
         has_landsea=0
      END 
      'cnrm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         file_desc='CNRM'
         model_desc='CNRM_Atmos'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1.
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
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            model_dqdt_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnhus.'+REFORM(initial_date)+'.00Z.nc'
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_pr_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
               model_pr_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               model_z=OPEN_AND_EXTRACT(model_dqdt_infile(0),level_name)*level_multiplier
               DEFINE_BOUNDARIES,box,model_pr_latitude,model_pr_longitude,model_pr_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_pr_longitude)
               model_nlat=N_ELEMENTS(model_pr_latitude)     
               model_nz=N_ELEMENTS(model_z)
               model_dqdt_longitude=OPEN_AND_EXTRACT(model_dqdt_infile(0),longitude_name)
               model_dqdt_latitude=OPEN_AND_EXTRACT(model_dqdt_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_dqdt_latitude,model_dqdt_longitude,model_dqdt_box_tx,/LIMIT
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
               model_dqdt=fltarr(model_nlon,model_nlat,model_nz,n_times_per_day)    
               dqdt_binned=fltarr(n_bins+1,model_nz)
               dqdt_pctascent=fltarr(n_bins+1,model_nz)
               n_pts=fltarr(n_bins+1)
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
            model_dqdt(*,*,*,*)=$
               OPEN_AND_EXTRACT(model_dqdt_infile(0),'tnhus',$
                                offset=[model_dqdt_box_tx(1),model_dqdt_box_tx(0),0,lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,model_nz,n_times_per_day])*dqdt_multiplier
            IF TOTAL(where(ABS(model_dqdt) ge missing_value)) gt 0 THEN $
               model_dqdt[where(ABS(model_dqdt) ge missing_value)]=!Values.F_NaN
            
            FOR m=0,model_nlat-1 DO BEGIN
               FOR n=0,model_nz-1 DO BEGIN
                  model_dqdt_lonavg=MEAN(model_dqdt(*,m,n,*),/NaN)   
;                  model_dqdt(*,m,n,*)=model_dqdt(*,m,n,*)-model_dqdt_lonavg
               ENDFOR
            ENDFOR
            
            FOR m=0,model_nz-1 DO BEGIN
               dqdt_thislev=REFORM(model_dqdt(*,*,m,*))
               FOR n=0,n_bins-1 DO BEGIN
                  IF n ne n_bins-1 THEN BEGIN
                     valid_points=where(model_precip ge precip_bins(n) and model_precip lt precip_bins(n+1))
                  ENDIF ELSE $
                     valid_points=where(model_precip ge precip_bins(n))
                  IF TOTAL(valid_points) ne -1 THEN BEGIN
                     dqdt_binned(n+1,m)=dqdt_binned(n+1,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
                     IF m eq 0 THEN $
                        n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
                  ENDIF
               ENDFOR              
               valid_points=where(model_precip lt precip_bins(0) and FINITE(model_precip) eq 1)
               IF TOTAL(valid_points) ne -1 THEN BEGIN
                  dqdt_binned(0,m)=dqdt_binned(0,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
                  IF m eq 0 THEN $
                     n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
               ENDIF
            ENDFOR
         ENDFOR
      ENDIF
   ENDFOR
   
   FOR n=0,n_bins DO $
      dqdt_binned(n,*)=dqdt_binned(n,*)/FLOAT(n_pts(n))      
   FOR n=0,ecmwf_nz-1 DO BEGIN
      model_pt=NEAREST(model_z,ecmwf_z(n))
      all_models_dqdt_binned(i,*,n)=dqdt_binned(*,model_pt)
   ENDFOR
   
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dqdt_twod_withlead.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=150,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=dqdt_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Total dq/dt (g kg!U-1!N day!U-1!N)',$
       TITLE='dq/dt binned by precip over '+box_name+' - '+model_desc+' dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+$
       STRTRIM(STRING(MIN(lead_times)),1)+' to '+STRTRIM(STRING(MAX(lead_times)),1)
   
   AXES,YVALS=model_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.08,0.10,0.15,0.2,0.3,0.4,0.6,0.8,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.20','0.30','0.40','0.60','0.80','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE,/NOVIEW

ENDFOR

pattern_correlations=fltarr(n_models+1,n_models+1)
yotc=REFORM(ecmwf_dqdt_binned,[ecmwf_nz*(n_bins+1)])
FOR i=0,n_models-1 DO BEGIN
   modela=REFORM(all_models_dqdt_binned(i,*,*),[ecmwf_nz*(n_bins+1)])
   pattern_correlations(i+1,0)=CORRELATE(yotc,modela)
   pattern_correlations(0,i+1)=pattern_correlations(i+1,0)
   FOR j=0,n_models-1 DO BEGIN
      modelb=REFORM(all_models_dqdt_binned(j,*,*),[ecmwf_nz*(n_bins+1)])
      pattern_correlations(i+1,j+1)=CORRELATE(modela,modelb)
      pattern_correlations(j+1,i+1)=pattern_correlations(i+1,j+1)
   ENDFOR   
ENDFOR
pattern_correlations(0,0)=1.

mylevs=['-0.35','-0.25','-0.15','-0.05',$
        '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dqdt_twod_withlead.pattern_correlations_'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
       STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,XOFFSET=0,YOFFSET=0,TFONT=3,$
       TCHARSIZE=100,SPACE3=600,CB_WIDTH=100
GSET,XMIN=0,XMAX=n_models+1,YMIN=0,YMAX=n_models+1
CS,SCALE=3,NCOLS=N_ELEMENTS(mylevs)+1,white=[6]
LEVS,MANUAL=mylevs
CON,X=indgen(n_models+1)+0.5,Y=indgen(n_models+1)+0.5,FIELD=pattern_correlations,$
    /BLOCK,/NOLINES,CB_TITLE='Pattern correlation of dq/dt by precipitation rate',$
    TITLE='Pattern correlations of dq/dt binned by precipitation rate'
AXES,YVALS=indgen(n_models+1)+0.5,YLABELS=['YoTC',our_model_names],XVALS=indgen(n_models+1)+0.5,$
     XLABELS=['YoTC',our_model_names]
PSCLOSE

STOP
END

         
