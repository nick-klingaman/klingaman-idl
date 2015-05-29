PRO mjo_diabatic_precip_bin_dTdt_twod_withlead_percentiles,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type,plot_yotc=plot_yotc

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

mylevs=['1.0','2.0','3.0','4.0','5.0','6.0','8.0','10.0','12.0','15.0','18.0','21.0','24.0','27.0']
mylevs_pct=['5','10','15','20','25','30','35','40','45','50','55','60','65','70','75','80','85','90','95']
mylevs_pct_diff=['-65','-55','-45','-35','-25','-15','-5','5','15','25','35','45','55','65']

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

precip_bins=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95]
n_bins=N_ELEMENTS(precip_bins+1)

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
   
;   trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/two_point_five/TRMM_3B42v6A.oct2009-feb2010_3hrmeans.precip.2.5x2.5.nc'
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

   dTdt_binned=fltarr(n_bins+1,ecmwf_nz)
   ecmwf_dTdt_pcttotal=fltarr(n_bins+1,ecmwf_nz)
   n_pts=fltarr(n_bins+1)
   mean_precip=fltarr(n_bins+1)

   FOR m=0,n_times_per_day-1 DO BEGIN
      temp=REFORM(trmm_precip(*,*,m))
      IF mask_type eq 'ocean' THEN BEGIN
         temp[where(ecmwf_mask eq 1)]=!Values.F_NaN
      ENDIF ELSE IF mask_type eq 'land' THEN $
         temp[where(ecmwf_mask eq 0)]=!Values.F_NaN
      trmm_precip(*,*,m)=temp
   ENDFOR   
   
   ; Convert precipitation values to percentiles
   trmm_precip_pct=fltarr(trmm_nlon,trmm_nlat,trmm_count)
   trmm_precip_pct[SORT(trmm_precip)]=100.*findgen(N_ELEMENTS(trmm_precip))/FLOAT(N_ELEMENTS(trmm_precip))

   FOR m=0,ecmwf_nz-1 DO BEGIN
      dTdt_thislev=REFORM(ecmwf_dTdt(*,*,m,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(trmm_precip_pct ge precip_bins(n) and trmm_precip_pct lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(trmm_precip_pct ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            dTdt_binned(n+1,m)=dTdt_binned(n+1,m)+TOTAL(dTdt_thislev[valid_points],/NaN)            
            IF m eq 0 THEN BEGIN
               n_pts(n+1)=N_ELEMENTS(valid_points)
               mean_precip(n+1)=MEAN(trmm_precip[valid_points],/NaN)
            ENDIF
         ENDIF
      ENDFOR              
      valid_points=where(trmm_precip_pct lt precip_bins(0) and FINITE(trmm_precip) eq 1)
      dTdt_binned(0,m)=dTdt_binned(0,m)+TOTAL(dTdt_thislev[valid_points],/NaN)
      IF m eq 0 THEN BEGIN
         n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
         mean_precip(0)=MEAN(trmm_precip[valid_points],/NaN)
      ENDIF
   ENDFOR

   FOR n=0,n_bins DO BEGIN
      dTdt_binned(n,*)=dTdt_binned(n,*)/FLOAT(n_pts(n))
      dTdt_vint=0.
      dp_total=0.
      FOR m=1,ecmwf_nz-1 DO BEGIN
         dTdt_vint=dTdt_vint+dTdt_binned(n,m)*(ecmwf_z(m)-ecmwf_z(m-1))
         dp_total=dp_total+ecmwf_z(m)-ecmwf_z(m-1)
      ENDFOR
      dTdt_vint=dTdt_vint/FLOAT(dp_total)
      FOR m=1,ecmwf_nz-1 DO BEGIN
         vint=0.
         sump=0.
         FOR p=1,m DO $
            vint=vint+dTdt_binned(n,p)*(ecmwf_z(p)-ecmwf_z(p-1))            
         vint=vint/dp_total
         ecmwf_dTdt_pcttotal(n,m)=vint/dTdt_vint*100.
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_percentiles.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=9,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=dTdt_binned,/BLOCK,/NOLINES,$
       CB_TITLE='dT/dt from physics (Q1 minus Qr; K day!U-1!N)',$
       TITLE='Q1-Qr by precip percentile over '+box_name+' - ECMWF_YOTC Q and precip - dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)  
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=60,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=mean_precip,STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,40,60],$
        YLABELS=['0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2',$
                 '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','40.0','60.0'],$
        /ONLYRIGHT,YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_pcttotal_twod_withlead_percentiles.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=150,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_pct)+1,white=2
   LEVS,MANUAL=mylevs_pct
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=ecmwf_dTdt_pcttotal,/BLOCK,/NOLINES,$
       CB_TITLE='Fraction of total vertically integrated dT/dt from physics (Q1-Qr; K day!U-1!N) above this level',$
       TITLE='Q1-Qr binned by precip over '+box_name+' - ECMWF_YOTC Q and precip - dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)   
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=60,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=mean_precip,STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,40,60],$
        YLABELS=['0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2',$
                 '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','40.0','60.0'],$
        /ONLYRIGHT,YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
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
   
   FOR j=0,n_days(0)-1 DO BEGIN
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_pr_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
               model_pr_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_pr_latitude,model_pr_longitude,model_pr_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_pr_longitude)
               model_nlat=N_ELEMENTS(model_pr_latitude)   
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
               model_precip=fltarr(model_nlon,model_nlat,n_days*n_lead_times,n_times_per_day)
               model_precip_pct=fltarr(model_nlon,model_nlat,n_days*n_lead_times,n_times_per_day)
            ENDIF
            model_precip(*,*,j*n_lead_times+k,*)=$            
               OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                offset=[model_pr_box_tx(1),model_pr_box_tx(0),lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])*precip_multiplier
            FOR m=0,n_times_per_day-1 DO BEGIN
               temp=REFORM(model_precip(*,*,j*n_lead_times+k,m))
               IF mask_type eq 'ocean' THEN BEGIN
                  IF TOTAL(where(model_mask eq 1)) ge 0 THEN $
                     temp[where(model_mask eq 1)]=!Values.F_NaN
               ENDIF ELSE IF mask_type eq 'land' THEN $
                  temp[where(model_mask ne 1)]=!Values.F_NaN
               model_precip(*,*,j*n_lead_times+k,m)=temp
            ENDFOR
         ENDFOR
      ENDIF
   ENDFOR
   model_precip_pct[SORT(model_precip)]=100.*findgen(N_ELEMENTS(model_precip))/FLOAT(N_ELEMENTS(model_precip))
   
   grid_flag=0
   FOR j=0,n_days(0)-1 DO BEGIN
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN
         FOR k=0,n_lead_times-1 DO BEGIN
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))            
            model_dTdt_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.tnt_physics.'+REFORM(initial_date)+'.00Z.nc'            
            IF grid_flag eq 0 THEN BEGIN
               model_z=OPEN_AND_EXTRACT(model_dTdt_infile(0),level_name)*level_multiplier                 
               model_nz=N_ELEMENTS(model_z)
               model_dTdt_longitude=OPEN_AND_EXTRACT(model_dTdt_infile(0),longitude_name)
               model_dTdt_latitude=OPEN_AND_EXTRACT(model_dTdt_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_dTdt_latitude,model_dTdt_longitude,model_dTdt_box_tx,/LIMIT               
               model_dTdt=fltarr(model_nlon,model_nlat,model_nz,n_days*n_lead_times,n_times_per_day)    
               dTdt_binned=fltarr(n_bins+1,model_nz)
               dTdt_pcttotal=fltarr(n_bins+1,model_nz)
               n_pts=fltarr(n_bins+1)
               mean_precip=fltarr(n_bins+1)
               grid_flag=1
            ENDIF            
            model_dTdt(*,*,*,j*n_lead_times+k,*)=$
               OPEN_AND_EXTRACT(model_dTdt_infile(0),'tnt_physics',$
                                offset=[model_dTdt_box_tx(1),model_dTdt_box_tx(0),0,lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,model_nz,n_times_per_day])*dTdt_multiplier
         ENDFOR
      ENDIF
   ENDFOR
   IF TOTAL(where(ABS(model_dTdt) ge missing_value)) gt 0 THEN $
      model_dTdt[where(ABS(model_dTdt) ge missing_value)]=!Values.F_NaN
   
   FOR m=0,model_nz-1 DO BEGIN
      dTdt_thislev=REFORM(model_dTdt(*,*,m,*,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(model_precip_pct ge precip_bins(n) and model_precip_pct lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(model_precip_pct ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            dTdt_binned(n+1,m)=dTdt_binned(n+1,m)+TOTAL(dTdt_thislev[valid_points],/NaN)
            IF m eq 0 THEN BEGIN
               n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
               mean_precip(n+1)=MEAN(model_precip[valid_points],/NaN)
            ENDIF
         ENDIF
      ENDFOR              
      valid_points=where(model_precip_pct lt precip_bins(0) and FINITE(model_precip_pct) eq 1)
      IF TOTAL(valid_points) ne -1 THEN BEGIN
         dTdt_binned(0,m)=dTdt_binned(0,m)+TOTAL(dTdt_thislev[valid_points],/NaN)
         IF m eq 0 THEN BEGIN
            n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
            mean_precip(0)=MEAN(model_precip[valid_points],/NaN)
         ENDIF
      ENDIF
   ENDFOR
  
   FOR n=0,n_bins DO BEGIN
      dTdt_binned(n,*)=dTdt_binned(n,*)/FLOAT(n_pts(n))
      dTdt_vint=0.
      dp_total=0.
      IF reverse_level eq 1 THEN BEGIN
         FOR m=1,model_nz-1 DO BEGIN
            dTdt_vint=dTdt_vint+dTdt_binned(n,m)*(model_z(m)-model_z(m-1))
            dp_total=dp_total+model_z(m)-model_z(m-1)
         ENDFOR
      ENDIF ELSE BEGIN
         FOR m=0,model_nz-2 DO BEGIN
            dTdt_vint=dTdt_vint+dTdt_binned(n,m)*(model_z(m)-model_z(m+1))
            dp_total=dp_total+model_z(m)-model_z(m+1)
         ENDFOR
      ENDELSE
      dTdt_vint=dTdt_vint/FLOAT(dp_total)
      IF reverse_level eq 1 THEN BEGIN
         FOR m=1,model_nz-1 DO BEGIN
            vint=0.
            sump=0.
            FOR p=1,m DO $
               vint=vint+dTdt_binned(n,p)*(model_z(p)-model_z(p-1))            
            vint=vint/dp_total
            dTdt_pcttotal(n,m)=vint/dTdt_vint*100.
         ENDFOR         
      ENDIF ELSE BEGIN        
         FOR m=0,model_nz-2 DO BEGIN
            vint=0.
            sump=0.
            FOR p=m,model_nz-2 DO $
               vint=vint+dTdt_binned(n,p)*(model_z(p)-model_z(p+1))
            vint=vint/dp_total
            dTdt_pcttotal(n,m)=vint/dTdt_vint*100.
         ENDFOR
      ENDELSE
   ENDFOR
   
   dTdt_pcttotal_diff=fltarr(n_bins+1,ecmwf_nz)
   FOR m=0,ecmwf_nz-1 DO BEGIN
      model_lev=NEAREST(model_z,ecmwf_z(m))
      dTdt_pcttotal_diff(*,m)=dTdt_pcttotal(*,model_lev)-ecmwf_dTdt_pcttotal(*,m)
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_twod_withlead_percentiles.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=150,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=9,NCOLS=N_ELEMENTS(mylevs)+1,white=2
   LEVS,MANUAL=mylevs
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=dTdt_binned,/BLOCK,/NOLINES,$
       CB_TITLE='dT/dt from physics (Q1-Qr) (K day!U-1!N)',$
       TITLE='Q1-Qr by precip percentile over '+box_name+' - '+model_desc+' dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+$
       STRTRIM(STRING(MIN(lead_times)),1)+' to '+STRTRIM(STRING(MAX(lead_times)),1)
   
   AXES,YVALS=model_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Precipitation percentile (over entire space-time domain)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=60,/YLOG
   IF TOTAL(where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)) ge 0 THEN $
      mean_precip[where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)]=!Values.F_NaN
   GPLOT,X=indgen(n_bins+1)+0.5,Y=mean_precip,STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,40,60],$
        YLABELS=['0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2',$
                 '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','40.0','60.0'],$
        /ONLYRIGHT,YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_pcttotal_twod_withlead_percentiles.'+model_desc+'_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=150,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_pct)+1,white=2
   LEVS,MANUAL=mylevs_pct
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=dTdt_pcttotal,/BLOCK,/NOLINES,$
       CB_TITLE='Fraction of total vertically integrated dT/dt from physics (Q1-Qr; K day!U-1!N) above this level',$
       TITLE='Q1-Qr by precip percentile over '+box_name+' - '+model_desc+' dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+$
       STRTRIM(STRING(MIN(lead_times)),1)+' to '+STRTRIM(STRING(MAX(lead_times)),1)
   
   AXES,YVALS=model_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Precipitation percentile (over entire space-time domain)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=60,/YLOG
   IF TOTAL(where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)) ge 0 THEN $
      mean_precip[where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)]=!Values.F_NaN
   GPLOT,X=indgen(n_bins+1)+0.5,Y=mean_precip,STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,40,60],$
        YLABELS=['0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2',$
                 '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','40.0','60.0'],$
        /ONLYRIGHT,YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dTdtphys_pcttotal_twod_withlead_percentiles.'+model_desc+'-minus-yotc_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+$
          STRTRIM(STRING(MIN(lead_times)),1)+'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=150,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_pct_diff)+1,white=9
   LEVS,MANUAL=mylevs_pct_diff
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=dTdt_pcttotal_diff,/BLOCK,/NOLINES,$
       CB_TITLE='Diff in fraction of vertically integrated dT/dt from physics (Q1-Qr; K day!U-1!N) above this level',$
       TITLE='Diff in frac of vint Q1-Qr by precip percentile over '+box_name+' - '+model_desc+' minus YOTC - dates '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+$
       STRTRIM(STRING(MIN(lead_times)),1)+' to '+STRTRIM(STRING(MAX(lead_times)),1)
   
   AXES,YVALS=ecmwf_z,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                      STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                      STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,/NORIGHT,YTITLE='Pressure',XTITLE='Precipitation percentile (over entire space-time domain)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=60,/YLOG
   IF TOTAL(where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)) ge 0 THEN $
      mean_precip[where(mean_precip lt 0.01 or FINITE(mean_precip) eq 0)]=!Values.F_NaN
   GPLOT,X=indgen(n_bins+1)+0.5,Y=mean_precip,STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,40,60],$
        YLABELS=['0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2',$
                 '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','40.0','60.0'],$
        /ONLYRIGHT,YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END

         
