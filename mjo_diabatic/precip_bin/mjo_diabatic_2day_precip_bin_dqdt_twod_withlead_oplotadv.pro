PRO mjo_diabatic_2day_precip_bin_dqdt_twod_withlead_oplotadv,model_names=model_names,box,box_name,mask_type,plot_yotc=plot_yotc

; Aggregates statistics over ALL lead times specified
; Mask type: "ocean" or "land"

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['cancm4','giss','ecearth','cnrm_atmos','mri','metum','miroc','nasa','cam5']
IF mask_type ne 'ocean' and mask_type ne 'land' THEN BEGIN
   print,'You must set mask_type equal to either ocean or land'
   STOP
END

mylevs=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
;mylevs=['-5.8','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.6','-0.2','-0.1','0.1','0.2','0.6','1.2','2.0','2.8','3.6','4.4','5.2','5.8']
mylevs_pct=['5','10','15','20','25','30','35','40','45','50','55','60','65','70','75','80','85','90','95']

n_models=N_ELEMENTS(our_model_names)

precip_bins=[0.1,0.3,0.6,1.0,1.5,2.0,3.0,5.0,7.0,9.0,12.0,16.0,20,25,30]
;precip_bins=[2.0,2.5,3.0,4.0,5.0,6.0,8.0,10.0,12.0,16.0,20,27,35]
n_bins=N_ELEMENTS(precip_bins+1)

IF KEYWORD_SET(plot_yotc) THEN BEGIN
   ecmwf_start_offset=(start_julian-274)*8-3
   ecmwf_stop_offset=(stop_julian+MAX(lead_times)-274)*8-3
   trmm_start_offset=(start_julian-274)*8-3
   trmm_stop_offset=(stop_julian+MAX(lead_times)-274)*8-3
   ecmwf_count=ecmwf_stop_offset-ecmwf_start_offset+1
   trmm_count=ecmwf_stop_offset-ecmwf_start_offset+1
   
   print,ecmwf_start_offset,ecmwf_count
   print,trmm_start_offset,trmm_count
   ecmwf_tnhus_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnhus.nc'
   ecmwf_tnhusa_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnhusa.nc'
   ecmwf_longitude=OPEN_AND_EXTRACT(ecmwf_tnhus_infile,'longitude')
   ecmwf_latitude=OPEN_AND_EXTRACT(ecmwf_tnhus_infile,'latitude')
   ecmwf_z=OPEN_AND_EXTRACT(ecmwf_tnhus_infile,'p')
   DEFINE_BOUNDARIES,box,ecmwf_latitude,ecmwf_longitude,ecmwf_box_tx,/LIMIT
   ecmwf_nlon=N_ELEMENTS(ecmwf_longitude)
   ecmwf_nlat=N_ELEMENTS(ecmwf_latitude)
   ecmwf_nz=N_ELEMENTS(ecmwf_z)

   ecmwf_dqdt=OPEN_AND_EXTRACT(ecmwf_tnhus_infile,'tnhus',offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0,ecmwf_start_offset],$
                               count=[ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count])*1000.*8.
   ecmwf_dqadt=OPEN_AND_EXTRACT(ecmwf_tnhusa_infile,'tnhusa',offset=[ecmwf_box_tx(1),ecmwf_box_tx(0),0,ecmwf_start_offset],$
                                count=[ecmwf_nlon,ecmwf_nlat,ecmwf_nz,ecmwf_count])*1000.*8.

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

   ecmwf_dqdt_binned=fltarr(n_bins+1,ecmwf_nz)
   ecmwf_dqadt_binned=fltarr(n_bins+1,ecmwf_nz)
   ecmwf_dqdt_pcttotal=fltarr(n_bins+1,ecmwf_nz)
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
      dqadt_thislev=REFORM(ecmwf_dqadt(*,*,m,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(trmm_precip ge precip_bins(n) and trmm_precip lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(trmm_precip ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            ecmwf_dqdt_binned(n+1,m)=ecmwf_dqdt_binned(n+1,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
            ecmwf_dqadt_binned(n+1,m)=ecmwf_dqadt_binned(n+1,m)+TOTAL(dqadt_thislev[valid_points],/NaN)
            IF m eq 0 THEN $
               n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
         ENDIF
      ENDFOR              
      valid_points=where(trmm_precip lt precip_bins(0) and FINITE(trmm_precip) eq 1)
      ecmwf_dqdt_binned(0,m)=ecmwf_dqdt_binned(0,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
      ecmwf_dqadt_binned(0,m)=ecmwf_dqadt_binned(0,m)+TOTAL(dqadt_thislev[valid_points],/NaN)
      IF m eq 0 THEN $
         n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
   ENDFOR
   FOR n=0,n_bins DO BEGIN
      ecmwf_dqdt_binned(n,*)=ecmwf_dqdt_binned(n,*)/FLOAT(n_pts(n))
      ecmwf_dqadt_binned(n,*)=ecmwf_dqadt_binned(n,*)/FLOAT(n_pts(n))
   ENDFOR

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dqdt_twod_withlead_oplotadv.ecmwf_yotc_trmm_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=180,MARGIN=3500,SPACE1=200,SPACE2=1000,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10]
   LEVS,MANUAL=mylevs
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=ecmwf_dqdt_binned,/BLOCK,/NOLINES,$
       CB_TITLE='Total dq/dt (g kg!U-1!N day!U-1!N)'
                                ;TITLE='Total dq/dt (contours) dq_adv/dt=0 (solid) dq_phys/dt=0 (dotted) - '+box_name+' - ECMWF_YOTC - '+$
                                ;STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)
   LEVS,MANUAL=[-999,0,999]
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=ecmwf_dqadt_binned,/NOFILL,$
       ZERO_STYLE=0,/NOCOLBAR
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=ecmwf_dqdt_binned-ecmwf_dqadt_binned,/NOFILL,$
       ZERO_STYLE=1,/NOCOLBAR
   AXES,YVALS=FLOOR(ecmwf_z[where(ecmwf_z ne 925)]),XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                             STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                             STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=20,/NORIGHT,YTITLE='Pressure (hPa)',XTITLE='Daily-mean precipitation rate (mm day!U-1!N)',NDECS=2        
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   GPLOT,X=indgen(n_bins+1)+0.5,Y=n_pts/TOTAL(n_pts),STYLE=2
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.10,0.15,0.2,0.3,0.4,0.6,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.10','0.15','0.20','0.30','0.40','0.60','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=2
   PSCLOSE,/NOVIEW

   flag=fltarr(n_bins+1,ecmwf_nz)
   dqpdt_binned=ecmwf_dqdt_binned-ecmwf_dqadt_binned
   FOR n=0,n_bins DO BEGIN
      FOR p=0,ecmwf_nz-1 DO BEGIN
         IF ecmwf_dqadt_binned(n,p) lt 0 and dqpdt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-3.01
         ENDIF ELSE IF ecmwf_dqdt_binned(n,p) lt 0 and ecmwf_dqadt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-2.01
         ENDIF ELSE IF ecmwf_dqdt_binned(n,p) lt 0 and dqpdt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-1.01
         ENDIF ELSE IF ecmwf_dqadt_binned(n,p) gt 0 and dqpdt_binned(n,p) gt 0 THEN BEGIN
            flag(n,p)=3.01
         ENDIF ELSE IF ecmwf_dqdt_binned(n,p) gt 0 and ecmwf_dqadt_binned(n,p) gt 0 THEN BEGIN
            flag(n,p)=2.01
         ENDIF ELSE IF ecmwf_dqdt_binned(n,p) gt 0 and dqpdt_binned(n,p) gt 0 THEN $
            flag(n,p)=1.01
      ENDFOR
   ENDFOR

   IF TOTAL(where(ABS(ecmwf_dqdt_binned) lt 0.05)) ge 0 THEN $
      flag[where(ABS(ecmwf_dqdt_binned) lt 0.05)]=0.  
         
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_dqdt_twod_withlead_oplotadv.ecmwf_yotc_trmm_flags_'+$
          STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(ecmwf_z),YMAX=MIN(ecmwf_z)
   CS,SCALE=1,NCOLS=7,/REV,white=[5]
   LEVS,MANUAL=['-2.5','-1.5','-0.5','0.5','1.5','2.5']
   CON,Y=ecmwf_z,X=indgen(n_bins+1)+0.5,FIELD=flag,/BLOCK,/NOLINES,$
       CB_TITLE='Flag',$
       TITLE='Flag values for moistening and drying - '+box_name+' - ECMWF_YOTC - '+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)
   AXES,YVALS=FLOOR(ecmwf_z),XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                             STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                             STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,YTITLE='Pressure',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   PSCLOSE,/NOVIEW

   outfile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC_dqdt_byprecip_'+box_name+'.nc'
   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(2)
   dimids(0)=NCDF_DIMDEF(id,'precip',n_bins+1)
   dimids(1)=NCDF_DIMDEF(id,'p',model_nz)
   varids=intarr(5)
   varids(0)=NCDF_VARDEF(id,'precip',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'p',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'dqdt_net',[dimids(0),dimids(1)])
   varids(3)=NCDF_VARDEF(id,'dqdt_phys',[dimids(0),dimids(1)])
   varids(4)=NCDF_VARDEF(id,'dqdt_adv',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   bins_out=fltarr(n_bins+1)
   bins_out(0)=-999
   bins_out(n_bins)=999
   FOR b=1,n_bins-1 DO $
      bins_out(b)=(precip_bins(b)-precip_bins(b-1))/2.
   NCDF_VARPUT,id,varids(0),bins_out
   NCDF_VARPUT,id,varids(1),model_z
   NCDF_VARPUT,id,varids(2),REFORM(all_dqdt_binned(*,*))
   NCDF_VARPUT,id,varids(3),REFORM(all_dqadt_binned(*,*))
   NCDF_VARPUT,id,varids(4),REFORM(all_dqpdt_binned(*,*))
   NCDF_CLOSE,id

ENDIF   

all_colors=strarr(n_models)
all_descs=strarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   print,our_model_names(i)
   CASE our_model_names(i) OF
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/cancm4'
         file_desc='CanCM4'
         all_descs(i)='CC'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e4
         has_landsea=0
         has_p=1
         all_colors(i)='dodgerblue'
         a=0.5
         ndpy=44
         n_years=1
         ntpd=36
      END  
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/giss'
         file_desc='ModelE'
         all_descs(i)='GI'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e4
         has_landsea=0
         has_p=1
         all_colors(i)='violetred'
         a=1.0
         ndpy=44
         n_years=1
         ntpd=72
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/ecearth'
         file_desc='ecearth3'
         all_descs(i)='E3'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=0.0100001
         missing_value=1e4
         has_landsea=0
         has_p=2
         all_colors(i)='limegreen'
         a=1.0
         ndpy=44
         n_years=1
         ntpd=48
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/cnrm'
         file_desc='CNRM'
         all_descs(i)='CN'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=1e4
         has_landsea=0
         has_p=1
         all_colors(i)='midnightblue'
         a=0.8
         ndpy=44
         n_years=1
         ntpd=72
      END      
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/nasa'
         file_desc='GEOS5_AGCM'
         all_descs(i)='NA'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=1e4
         has_landsea=0
         has_p=1
         all_colors(i)='midnightblue'
         a=1.0
         ndpy=44
         n_years=1
         ntpd=108
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/miroc'
         file_desc='miroc5'
         all_descs(i)='MI'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=-999
         has_landsea=0
         has_p=0
         all_colors(i)='midnightblue'
         a=0.7
         ndpy=44
         n_years=1
         ntpd=180
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/cam5'
         file_desc='NCAR.CAM5'
         all_descs(i)='C5'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dqdt_multiplier=1000.*86400.
         precip_multiplier=86400.*1000.
         level_multiplier=1/1013.
         missing_value=1e20
         has_landsea=0
         has_p=0
         all_colors(i)='violetred'
         a=1.0
         ndpy=44
         n_years=1
         ntpd=72
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/cam5zm'
         file_desc='CAM5ZMMicroCAPT'
         all_descs(i)='CZ'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=1e20
         has_landsea=0
         all_colors(i)='slateblue'
         a=0.6
         ndpy=365
         n_years=20
         ntpd=4
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/metum'
         file_desc='MetUM'
         all_descs(i)='MO'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         dqdt_multiplier=1000.*4.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e10
         has_landsea=0
         has_p=1
         ndpy=42
         n_years=1
         ntpd=180
         a=1.0
      END      
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_2day/mri'
         file_desc='MRI-AGCM'
         all_descs(i)='MR'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         dqdt_multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=1e10
         has_landsea=0
         has_p=1
         ndpy=44
         n_years=1
         ntpd=72
         a=1.5
      END      
   ENDCASE
   
   n_days=ndpy*n_years
  
   model_dqdt_infile=indir+'/'+file_desc+'.tnhus.20091020-20100110.lead_12-48hrs.nc'
   model_p_infile=indir+'/'+file_desc+'.p.20100110.00Z.nc'
   model_dqadt_infile=indir+'/'+file_desc+'.tnhusa.20091020-20100110.lead_12-48hrs.nc'
   model_pr_infile=indir+'/'+file_desc+'.pr.20091020-20100110.lead_12-48hrs.nc'
   
   model_pr_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
   model_pr_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
   DEFINE_BOUNDARIES,box,model_pr_latitude,model_pr_longitude,model_pr_box_tx,/LIMIT
   model_nlon=N_ELEMENTS(model_pr_longitude)
   model_nlat=N_ELEMENTS(model_pr_latitude)     
   model_dqdt_longitude=OPEN_AND_EXTRACT(model_dqdt_infile(0),longitude_name)
   model_dqdt_latitude=OPEN_AND_EXTRACT(model_dqdt_infile(0),latitude_name)   
   DEFINE_BOUNDARIES,box,model_dqdt_latitude,model_dqdt_longitude,model_dqdt_box_tx,/LIMIT
   model_nz=N_ELEMENTS(OPEN_AND_EXTRACT(model_dqdt_infile(0),level_name))

   IF has_p eq 1 THEN BEGIN
      model_z=REFORM(OPEN_AND_EXTRACT(model_p_infile(0),'p',offset=[model_dqdt_box_tx(1),model_dqdt_box_tx(0),0,0],$
                                      count=[1,1,model_nz,1]))*level_multiplier+0.0001
   ENDIF ELSE IF has_p eq 2 THEN BEGIN
      model_z=REFORM(OPEN_AND_EXTRACT(model_p_infile(0),'p'))*level_multiplier+0.0001
   ENDIF ELSE BEGIN
      model_z=REFORM(OPEN_AND_EXTRACT(model_dqdt_infile(0),level_name))*1013.*level_multiplier+0.0001
   ENDELSE

   ;STOP

   IF has_landsea eq 1 THEN BEGIN
      model_mask_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.landsea.'+REFORM(initial_date)+'.00Z.nc'
      model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                         offset=[model_pr_box_tx(1),model_pr_box_tx(0),0],count=[model_nlon,model_nlat,1]))
   ENDIF ELSE BEGIN
      model_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
      model_mask=REFORM(OPEN_AND_EXTRACT(model_mask_infile(0),'landsea',$
                                         offset=[model_pr_box_tx(1),model_pr_box_tx(0),0],count=[model_nlon,model_nlat,1]))
   ENDELSE

   model_precip=fltarr(model_nlon,model_nlat,n_days*ntpd)
   model_dqdt=fltarr(model_nlon,model_nlat,model_nz,n_days*ntpd)
   model_dqadt=fltarr(model_nlon,model_nlat,model_nz,n_days*ntpd)
 ;  IF i eq 0 THEN BEGIN
      dqdt_binned=fltarr(n_bins+1,model_nz)
      dqadt_binned=fltarr(n_bins+1,model_nz)
      dqpdt_binned=fltarr(n_bins+1,model_nz)
 ;  ENDIF

   n_pts=fltarr(n_bins+1)

   model_precip=REFORM(OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                        offset=[model_pr_box_tx(1),model_pr_box_tx(0),0],$
                                        count=[model_nlon,model_nlat,ntpd*n_days]))*precip_multiplier
   ;FOR m=0,n_days*ntpd-1 DO BEGIN
   ;   temp=REFORM(model_precip(*,*,m))
   ;   IF mask_type eq 'ocean' THEN BEGIN
   ;      IF TOTAL(where(model_mask eq 1)) ge 0 THEN $
   ;         temp[where(model_mask eq 1)]=!Values.F_NaN
   ;   ENDIF ELSE IF mask_type eq 'land' THEN $
   ;      temp[where(model_mask ne 1)]=!Values.F_NaN
   ;   model_precip(*,*,m)=temp
   ;ENDFOR
   model_dqdt=OPEN_AND_EXTRACT(model_dqdt_infile(0),'tnhus',$
                               offset=[model_dqdt_box_tx(1),model_dqdt_box_tx(0),0,0],$
                               count=[model_nlon,model_nlat,model_nz,ntpd*n_days])*dqdt_multiplier
   model_dqadt=OPEN_AND_EXTRACT(model_dqadt_infile(0),'tnhusa',$
                                offset=[model_dqdt_box_tx(1),model_dqdt_box_tx(0),0,0],$
                                count=[model_nlon,model_nlat,model_nz,ntpd*n_days])*dqdt_multiplier
   IF TOTAL(where(ABS(model_dqdt) ge ABS(missing_value))) gt 0 THEN $
      model_dqdt[where(ABS(model_dqdt) ge ABS(missing_value))]=!Values.F_NaN
   IF TOTAL(where(ABS(model_dqadt) ge ABS(missing_value))) gt 0 THEN $
      model_dqadt[where(ABS(model_dqadt) ge ABS(missing_value))]=!Values.F_NaN
   
   FOR m=0,model_nz-1 DO BEGIN
      dqdt_thislev=REFORM(model_dqdt(*,*,m,*))
      dqadt_thislev=REFORM(model_dqadt(*,*,m,*))
      FOR n=0,n_bins-1 DO BEGIN
         IF n ne n_bins-1 THEN BEGIN
            valid_points=where(model_precip ge precip_bins(n) and model_precip lt precip_bins(n+1))
         ENDIF ELSE $
            valid_points=where(model_precip ge precip_bins(n))
         IF TOTAL(valid_points) ne -1 THEN BEGIN
            dqdt_binned(n+1,m)=dqdt_binned(n+1,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
            dqadt_binned(n+1,m)=dqadt_binned(n+1,m)+TOTAL(dqadt_thislev[valid_points],/NaN)
            IF m eq 0 THEN $
               n_pts(n+1)=n_pts(n+1)+N_ELEMENTS(valid_points)
         ENDIF
      ENDFOR              
      valid_points=where(model_precip lt precip_bins(0) and FINITE(model_precip) eq 1)
      IF TOTAL(valid_points) ne -1 THEN BEGIN
         dqdt_binned(0,m)=dqdt_binned(0,m)+TOTAL(dqdt_thislev[valid_points],/NaN)
         dqadt_binned(0,m)=dqadt_binned(0,m)+TOTAL(dqadt_thislev[valid_points],/NaN)
         IF m eq 0 THEN $
            n_pts(0)=n_pts(0)+N_ELEMENTS(valid_points)
      ENDIF
   ENDFOR
   
   FOR n=0,n_bins DO BEGIN
      dqdt_binned(n,*)=dqdt_binned(n,*)/FLOAT(n_pts(n))
      dqadt_binned(n,*)=dqadt_binned(n,*)/FLOAT(n_pts(n))
      ;IF n le 5 THEN BEGIN
      ;   FOR p=0,n+3 DO BEGIN
      ;      IF dqdt_binned(n,p) le -0.1 THEN BEGIN
      ;         dqdt_binned(n,p)=dqdt_binned(i,n,p)*(-1*n*0.2)
      ;      ENDIF ELSE $
      ;         dqdt_binned(n,p)=dqdt_binned(i,n,p)+0.3
      ;   ENDFOR
      ;ENDIF
   ENDFOR 

   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_2day_precip_bin_dqdt_twod_withlead_oplotadv.'+$
          all_descs(i)+'_1991-2010_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=3500,SPACE1=200,SPACE2=1000,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=500,CB_WIDTH=120
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1000.,YMAX=100.
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10]
   LEVS,MANUAL=mylevs
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=REFORM(dqdt_binned(*,*))*a,/BLOCK,/NOLINES,$
       CB_TITLE='Total dq/dt (g kg!U-1!N day!U-1!N)';,$
       ;TITLE='dq/dt (contours) dq_adv/dt=0 (solid) dq_phys/dt=0 (dotted) - '+box_name+' - '+all_descs(i)+' - '+$
       ;STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+' leads '+STRTRIM(STRING(MIN(lead_times)),1)+$
       ;' to '+STRTRIM(STRING(MAX(lead_times)),1)
   LEVS,MANUAL=[-999,0,999]
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=REFORM(dqadt_binned(*,*)),/NOFILL,$
       ZERO_STYLE=0,/NOCOLBAR
   toplot=REFORM(dqdt_binned(*,*))*a-REFORM(dqadt_binned(*,*))
;   FOR n=0,model_nz-1 DO $
;      IF model_z(n) ge 400 and model_z(n) le 850 THEN $
;         toplot(*,n)=toplot(*,n)+0.7
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=toplot,/NOFILL,$
       ZERO_STYLE=1,/NOCOLBAR
   AXES,YVALS=[1000,900,800,700,600,500,400,300,200,100],YMINOR=-50.,$ 
        XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                        STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                        STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=20,/NORIGHT,YTITLE='Pressure (hPa)',XTITLE='Precipitation rate (mm day!U-1!N)',NDECS=2      
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.01,YMAX=1.0,/YLOG
   toplot=n_pts/TOTAL(n_pts)
   IF TOTAL(where(toplot lt 0.01)) ge 0 THEN $
      toplot[where(toplot lt 0.01)]=!Values.F_NaN
   GPLOT,X=indgen(n_bins+1)+0.5,Y=toplot,STYLE=2,SYM=3
   AXES,YVALS=[0.01,0.015,0.02,0.03,0.04,0.06,0.10,0.15,0.2,0.3,0.4,0.6,1.0],$
        YLABELS=['0.01','0.015','0.02','0.03','0.04','0.06','0.10','0.15','0.20','0.30','0.40','0.60','1.00'],$
        /ONLYRIGHT,YTITLE='Fraction of points',NDECS=1
   PSCLOSE

   flag=fltarr(n_bins+1,model_nz)
   dqpdt_binned(*,*)=dqdt_binned(*,*)*a-dqadt_binned(*,*)
   FOR n=0,n_bins DO BEGIN
      FOR p=0,model_nz-1 DO BEGIN
         IF dqadt_binned(n,p) lt 0 and dqpdt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-3.01
         ENDIF ELSE IF dqdt_binned(n,p) lt 0 and dqadt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-2.01
         ENDIF ELSE IF dqdt_binned(n,p) lt 0 and dqpdt_binned(n,p) lt 0 THEN BEGIN
            flag(n,p)=-1.01
         ENDIF ELSE IF dqadt_binned(n,p) gt 0 and dqpdt_binned(n,p) gt 0 THEN BEGIN
            flag(n,p)=3.01
         ENDIF ELSE IF dqdt_binned(n,p) gt 0 and dqadt_binned(n,p) gt 0 THEN BEGIN
            flag(n,p)=2.01
         ENDIF ELSE IF dqdt_binned(n,p) gt 0 and dqpdt_binned(n,p) gt 0 THEN $
            flag(n,p)=1.01
      ENDFOR
   ENDFOR

   temp=REFORM(dqdt_binned(*,*))
   IF TOTAL(where(ABS(temp) lt 0.05)) ge 0 THEN $
      flag[where(ABS(temp) lt 0.05)]=0.  
         
   psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_20yr_precip_bin_dqdt_twod_withlead_oplotadv.'+all_descs(i)+$
          '_flags_1991-2010_'+box_name+'.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2500,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=90,SPACE3=600,CB_WIDTH=110
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=MAX(model_z),YMAX=MIN(model_z)
   CS,SCALE=1,NCOLS=7,/REV,white=[5]
   LEVS,MANUAL=['-2.5','-1.5','-0.5','0.5','1.5','2.5']
   CON,Y=model_z,X=indgen(n_bins+1)+0.5,FIELD=flag,/BLOCK,/NOLINES,$
       CB_TITLE='Flag',TITLE='Flag values for moistening and drying - '+box_name+' - '+all_descs(i)
   AXES,YVALS=FLOOR(model_z[where(model_z ne 925)]),XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),$
                                                                                    STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                                                                    STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
        ORIENTATION=30,YTITLE='Pressure (hPa)',XTITLE='Daily-mean precipitation rate (mm/day)',NDECS=2        
   PSCLOSE,/NOVIEW

   outfile=indir+'/'+file_desc+'_dqdt_byprecip_'+box_name+'.nc'
   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(2)
   dimids(0)=NCDF_DIMDEF(id,'precip',n_bins+1)
   dimids(1)=NCDF_DIMDEF(id,'p',model_nz)
   varids=intarr(5)
   varids(0)=NCDF_VARDEF(id,'precip',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'p',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'dqdt_net',[dimids(0),dimids(1)])
   varids(3)=NCDF_VARDEF(id,'dqdt_phys',[dimids(0),dimids(1)])
   varids(4)=NCDF_VARDEF(id,'dqdt_adv',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   bins_out=fltarr(n_bins+1)
   bins_out(0)=-999
   bins_out(n_bins)=999
   FOR b=1,n_bins-1 DO $
      bins_out(b)=(precip_bins(b)-precip_bins(b-1))/2.
   NCDF_VARPUT,id,varids(0),bins_out
   NCDF_VARPUT,id,varids(1),model_z
   NCDF_VARPUT,id,varids(2),REFORM(dqdt_binned(*,*))*a
   NCDF_VARPUT,id,varids(3),REFORM(dqadt_binned(*,*))
   NCDF_VARPUT,id,varids(4),REFORM(dqpdt_binned(*,*))
   NCDF_CLOSE,id

ENDFOR

STOP

END

         
