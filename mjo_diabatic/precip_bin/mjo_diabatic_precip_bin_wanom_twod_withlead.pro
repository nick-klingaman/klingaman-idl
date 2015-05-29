PRO mjo_diabatic_precip_bin_wanom_twod_withlead,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

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

mylevs=['-0.30','-0.25','-0.20','-0.15','-0.10','-0.08','-0.06','-0.04','-0.03','-0.02','-0.01','-0.005',$
        '0.005','0.01','0.02','0.03','0.04','0.05','0.06','0.08','0.10','0.15','0.20','0.25','0.30']

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

precip_bins=[0.1,0.2,0.3,0.4,0.6,0.8,1.0,1.5,2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,20,27,35]
;precip_bins=[2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,20,27,35]
n_bins=N_ELEMENTS(precip_bins+1)

IF KEYWORD_SET(plot_yotc) THEN BEGIN
   ecmwf_start_offset=(start_julian-274)*8;-3
   ecmwf_stop_offset=(stop_julian+MAX(lead_times)-274)*8;-3
   trmm_start_offset=(start_julian-274)*8
   trmm_stop_offset=(stop_julian+MAX(lead_times)-274)*8
   ecmwf_count=ecmwf_stop_offset-ecmwf_start_offset+1
   trmm_count=ecmwf_stop_offset-ecmwf_start_offset+1
   
   print,ecmwf_start_offset,ecmwf_count
   print,trmm_start_offset,trmm_count
   ecmwf_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans_0-24hfcst.w.nc'
   ecmwf_longitude=OPEN_AND_EXTRACT(ecmwf_infile,'longitude')
   ecmwf_latitude=OPEN_AND_EXTRACT(ecmwf_infile,'latitude')
   ecmwf_z=OPEN_AND_EXTRACT(ecmwf_infile,'p')
   DEFINE_BOUNDARIES,box,ecmwf_latitude,ecmwf_longitude,ecmwf_box_tx,/LIMIT
   ecmwf_nlon=N_ELEMENTS(ecmwf_longitude)
   ecmwf_nlat=N_ELEMENTS(ecmwf_latitude)
   ecmwf_nz=N_ELEMENTS(ecmwf_z)

   ecmwf_w=OPEN_AND_EXTRACT(ecmwf_infile,'W',offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0,ecmwf_start_offset],$
                            count=[ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count])
   
                                ;trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/two_point_five/TRMM_3B42v6A.oct2009-feb2010_3hrmeans.precip.2.5x2.5.nc'
   trmm_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.pr.nc'
   trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
   trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
   DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
   trmm_nlon=N_ELEMENTS(trmm_longitude)
   trmm_nlat=N_ELEMENTS(trmm_latitude)

   trmm_precip=OPEN_AND_EXTRACT(trmm_infile,'TP',offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_start_offset],$
                                count=[trmm_nlon,trmm_nlat,trmm_count])*8000.;24.
   ;trmm_precip[where(ABS(trmm_precip) ge 1000)]=!Values.F_NaN

   ecmwf_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
            
   ecmwf_mask=REFORM(OPEN_AND_EXTRACT(ecmwf_mask_infile(0),'landsea',$
                                      offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0],count=[ecmwf_nlon,ecmwf_nlat,1]))

   w_binned=fltarr(n_bins+1,ecmwf_nz)
   w_pctascent=fltarr(n_bins+1,ecmwf_nz)
   n_pts=fltarr(n_bins+1)
  
   FOR m=0,n_times_per_day-1 DO BEGIN
      temp=REFORM(trmm_precip(*,*,m))
      IF mask_type eq 'ocean' THEN BEGIN
         temp[where(ecmwf_mask eq 1)]=!Values.F_NaN
      ENDIF ELSE IF mask_type eq 'land' THEN $
         temp[where(ecmwf_mask eq 0)]=!Values.F_NaN
      trmm_precip(*,*,m)=temp
   ENDFOR   
   FOR m=0,ecmwf_nlat-1 DO BEGIN
      FOR n=0,ecmwf_nz-1 DO BEGIN
         FOR p=0,ecmwf_count-1 DO BEGIN
            ecmwf_w_lonavg=MEAN(ecmwf_w(*,m,n,p),/NaN)   
            ecmwf_w(*,m,n,p)=ecmwf_w(*,m,n,p)-ecmwf_w_lonavg
         ENDFOR
      ENDFOR
   ENDFOR
            
   FOR m=0,ecmwf_nz-1 DO BEGIN
      w_thislev=REFORM(ecmwf_w(*,*,m,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(trmm_precip ge precip_bins(n) and trmm_precip lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(trmm_precip ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            w_binned(n+1,m)=w_binned(n+1,m)+TOTAL(w_thislev[valid_points],/NaN)
            IF TOTAL(where(w_thislev[valid_points] lt 0)) ge 0 THEN $
               w_pctascent(n+1,m)=w_pctascent(n+1,m)+N_ELEMENTS(where(w_thislev[valid_points] lt 0))
            IF m eq 0 THEN $
               n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
         ENDIF
      ENDFOR              
      valid_points=where(trmm_precip lt precip_bins(0) and FINITE(trmm_precip) eq 1)
      w_binned(0,m)=w_binned(0,m)+TOTAL(w_thislev[valid_points],/NaN)
      w_pctascent(0,m)=w_pctascent(0,m)+N_ELEMENTS(where(w_thislev[valid_points] lt 0))
      IF m eq 0 THEN $
         n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
   ENDFOR

   FOR n=0,n_bins DO BEGIN
      w_binned(n,*)=w_binned(n,*)/FLOAT(n_pts(n))
      w_pctascent(n,*)=w_pctascent(n,*)/FLOAT(n_pts(n))*100.
   ENDFOR
   
   outfile='/home/ss901165/um_output6/mjodiab_20day/ECMWF_YOTC_TRMM_wanom_byprecip_'+box_name+'.nc'
   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(2)
   dimids(0)=NCDF_DIMDEF(id,'precip',n_bins+1)
   dimids(1)=NCDF_DIMDEF(id,'p',ecmwf_nz)
   varids=intarr(4)
   varids(0)=NCDF_VARDEF(id,'precip',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'p',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'qanom',[dimids(0),dimids(1)])
   varids(3)=NCDF_VARDEF(id,'qpctwet',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   bins_out=fltarr(n_bins+1)
   bins_out(0)=-999
   bins_out(n_bins)=999
   FOR b=1,n_bins-1 DO $
      bins_out(b)=(precip_bins(b)+precip_bins(b-1))/2.
   NCDF_VARPUT,id,varids(0),bins_out
   NCDF_VARPUT,id,varids(1),ecmwf_z
   NCDF_VARPUT,id,varids(2),w_binned
   NCDF_VARPUT,id,varids(3),w_pctascent
   NCDF_CLOSE,id
   
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_wanom_twod_withlead.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=w_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Vertical velocity (Pa s!U-1!N) anomaly from zonal mean ',$
       TITLE='W anomalies binned by precip over '+box_name+' '+mask_type+' - ECMWF_YOTC w and TRMM precip - dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)  
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2,SYM=3
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.08,0.10,0.15,0.2,0.3,0.4,0.6,0.8,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.20','0.30','0.40','0.60','0.80','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_wanom_twod_withlead.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'_pctascent.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_pct)+1
   LEVS,MANUAL=mylevs_pct
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=w_pctascent,/BLOCK,/NOLINES,$
       CB_TITLE='Percentage of points in bin with ascent (%)',$
       TITLE='Percent of points with ascent in '+box_name+' '+mask_type+' - ECMWF_YOTC w and TRMM precip - dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)  
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.08,0.10,0.15,0.2,0.3,0.4,0.6,0.8,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.20','0.30','0.40','0.60','0.80','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE,/NOVIEW
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=-1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
         precip_multiplier=86400.*1000.
         level_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
         w_multiplier=1.
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
      print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            ;print,initial_date
            model_w_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.wap.'+REFORM(initial_date)+'.00Z.nc'
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_pr_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
               model_pr_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               model_z=OPEN_AND_EXTRACT(model_w_infile(0),level_name)*level_multiplier
               DEFINE_BOUNDARIES,box,model_pr_latitude,model_pr_longitude,model_pr_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_pr_longitude)
               model_nlat=N_ELEMENTS(model_pr_latitude)     
               model_nz=N_ELEMENTS(model_z)
               model_w_longitude=OPEN_AND_EXTRACT(model_w_infile(0),longitude_name)
               model_w_latitude=OPEN_AND_EXTRACT(model_w_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_w_latitude,model_w_longitude,model_w_box_tx,/LIMIT
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
               model_w=fltarr(model_nlon,model_nlat,model_nz,n_times_per_day)    
               w_binned=fltarr(n_bins+1,model_nz)
               w_pctascent=fltarr(n_bins+1,model_nz)
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
            model_w(*,*,*,*)=$
               OPEN_AND_EXTRACT(model_w_infile(0),'wap',$
                                offset=[model_w_box_tx(1),model_w_box_tx(0),0,lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,model_nz,n_times_per_day])*w_multiplier
            IF TOTAL(where(ABS(model_w) ge missing_value)) gt 0 THEN $
               model_w[where(ABS(model_w) ge missing_value)]=!Values.F_NaN
            
            FOR m=0,model_nlat-1 DO BEGIN
               FOR n=0,model_nz-1 DO BEGIN
                  model_w_lonavg=MEAN(model_w(*,m,n,*),/NaN)   
                  model_w(*,m,n,*)=model_w(*,m,n,*)-model_w_lonavg
               ENDFOR
            ENDFOR
            
            FOR m=0,model_nz-1 DO BEGIN
               w_thislev=REFORM(model_w(*,*,m,*))
               FOR n=0,n_bins-1 DO BEGIN
                  IF n ne n_bins-1 THEN BEGIN
                     valid_points=where(model_precip ge precip_bins(n) and model_precip lt precip_bins(n+1))
                  ENDIF ELSE $
                     valid_points=where(model_precip ge precip_bins(n))
                  IF TOTAL(valid_points) ne -1 THEN BEGIN
                     w_binned(n+1,m)=w_binned(n+1,m)+TOTAL(w_thislev[valid_points],/NaN)
                     IF TOTAL(where(w_thislev[valid_points] lt 0)) ge 0 THEN $
                        w_pctascent(n+1,m)=w_pctascent(n+1,m)+N_ELEMENTS(where(w_thislev[valid_points] lt 0))
                     IF m eq 0 THEN $
                        n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
                  ENDIF
               ENDFOR              
               valid_points=where(model_precip lt precip_bins(0) and FINITE(model_precip) eq 1)
               w_binned(0,m)=w_binned(0,m)+TOTAL(w_thislev[valid_points],/NaN)
               w_pctascent(0,m)=w_pctascent(0,m)+N_ELEMENTS(where(w_thislev[valid_points] lt 0))
               IF m eq 0 THEN $
                  n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
            ENDFOR
         ENDFOR
      ENDIF
   ENDFOR
   
   FOR n=0,n_bins DO BEGIN
      w_binned(n,*)=w_binned(n,*)/FLOAT(n_pts(n))
      w_pctascent(n,*)=w_pctascent(n,*)/FLOAT(n_pts(n))*100.
   ENDFOR

   outfile=indir+'/'+file_desc+'_wanom_byprecip_'+box_name+'.nc'
   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(2)
   dimids(0)=NCDF_DIMDEF(id,'precip',n_bins+1)
   dimids(1)=NCDF_DIMDEF(id,'p',model_nz)
   varids=intarr(4)
   varids(0)=NCDF_VARDEF(id,'precip',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'p',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'qanom',[dimids(0),dimids(1)])
   varids(3)=NCDF_VARDEF(id,'qpctwet',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   bins_out=fltarr(n_bins+1)
   bins_out(0)=-999
   bins_out(n_bins)=999
   FOR b=1,n_bins-1 DO $
      bins_out(b)=(precip_bins(b)+precip_bins(b-1))/2.
   NCDF_VARPUT,id,varids(0),bins_out
   NCDF_VARPUT,id,varids(1),model_z
   NCDF_VARPUT,id,varids(2),w_binned
   NCDF_VARPUT,id,varids(3),w_pctascent
   NCDF_CLOSE,id

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_wanom_twod_withlead.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=140,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=w_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Vertical velocity (Pa s!U-1!N) anomaly from zonal mean ',$
       TITLE='W anomalies binned by precip over '+box_name+' '+mask_type+' - '+model_desc+' dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+$
       STRTRIM(STRING(MIN(lead_times)),1)+' to '+STRTRIM(STRING(MAX(lead_times)),1)
   
   AXES,YVALS=model_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2,SYM=3
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.08,0.10,0.15,0.2,0.3,0.4,0.6,0.8,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.20','0.30','0.40','0.60','0.80','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_wanom_twod_withlead.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'_pctascent.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_pct)+1
   LEVS,MANUAL=mylevs_pct
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=w_pctascent,/BLOCK,/NOLINES,$
       CB_TITLE='Percentage of points in bin with ascent (%)',$
       TITLE='Percent of points with ascent in '+box_name+' '+mask_type+' - '+model_desc+' dates '+$
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

STOP
END

         
