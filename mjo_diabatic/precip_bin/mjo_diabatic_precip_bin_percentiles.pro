PRO mjo_diabatic_precip_bin_percentiles,model_names=model_names,start_date,stop_date,lead_times,box,box_name,mask_type

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','metum','miroc','mri','nrl','nasa','giss','spcam','cancm4','cam5','cam5zm','ecearth','cnrm_atmos']
n_models=N_ELEMENTS(our_model_names)
IF mask_type ne 'ocean' and mask_type ne 'land' THEN BEGIN
   print,'You must set mask_type equal to either ocean or land'
   STOP
END

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
cam5_valid_dates=STRTRIM(STRING([indgen(4)+20091010,indgen(16)+20091016,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
ecearth_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(21)+20100101]),1)
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)

precip_bins=indgen(19)*5+5
n_bins=N_ELEMENTS(precip_bins)
n_text=intarr(n_bins)

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

trmm_start_offset=(start_julian-274)*8
trmm_stop_offset=(stop_julian+MAX(lead_times)-274)*8
trmm_count=trmm_stop_offset-trmm_start_offset+1
yotc_start_offset=(start_julian-274)*8-4
yotc_stop_offset=(stop_julian+MAX(lead_times)-274)*8
yotc_count=yotc_stop_offset-yotc_start_offset+1

trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/two_point_five/TRMM_3B42v6A.oct2009-feb2010_3hrmeans.precip.2.5x2.5.nc'
trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
trmm_nlon=N_ELEMENTS(trmm_longitude)
trmm_nlat=N_ELEMENTS(trmm_latitude)
trmm_precip=OPEN_AND_EXTRACT(trmm_infile,'precip',offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_start_offset],$
                             count=[trmm_nlon,trmm_nlat,trmm_count])*24.
IF TOTAL(where(trmm_precip lt 0)) ge 0 THEN $
   trmm_precip[where(trmm_precip lt 0)]=!Values.F_NaN

yotc_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.pr.nc'
yotc_longitude=OPEN_AND_EXTRACT(yotc_infile,'longitude')
yotc_latitude=OPEN_AND_EXTRACT(yotc_infile,'latitude')
DEFINE_BOUNDARIES,box,yotc_latitude,yotc_longitude,yotc_box_tx,/LIMIT
yotc_nlon=N_ELEMENTS(yotc_longitude)
yotc_nlat=N_ELEMENTS(yotc_latitude)
yotc_precip=OPEN_AND_EXTRACT(yotc_infile,'TP',offset=[yotc_box_tx(1),yotc_box_tx(0),yotc_start_offset],$
                             count=[yotc_nlon,yotc_nlat,yotc_count])*8*1000.
IF TOTAL(where(yotc_precip lt 0)) ge 0 THEN $
   yotc_precip[where(yotc_precip lt 0)]=!Values.F_NaN

yotc_mask_infile='/home/ss901165/um_output6/mjodiab_20day/ecmwf/20091010/ECMWF_IFS.landsea.20091010.00Z.nc'
yotc_mask=REFORM(OPEN_AND_EXTRACT(yotc_mask_infile(0),'landsea',$
                                   offset=[yotc_box_tx(1),yotc_box_tx(0),0],count=[yotc_nlon,yotc_nlat,1]))
FOR i=0,trmm_count-1 DO BEGIN
   temp=REFORM(trmm_precip(*,*,i))
   IF mask_type eq 'ocean' THEN BEGIN
      temp[where(yotc_mask eq 1)]=!Values.F_NaN
   ENDIF ELSE IF mask_type eq 'land' THEN $
      temp[where(yotc_mask eq 0)]=!Values.F_NaN
   trmm_precip(*,*,i)=temp
ENDFOR
FOR i=0,yotc_count-1 DO BEGIN
   temp=REFORM(yotc_precip(*,*,i))
   IF mask_type eq 'ocean' THEN BEGIN
      temp[where(yotc_mask eq 1)]=!Values.F_NaN
   ENDIF ELSE IF mask_type eq 'land' THEN $
      temp[where(yotc_mask eq 0)]=!Values.F_NaN
   yotc_precip(*,*,i)=temp
ENDFOR

trmm_temp=trmm_precip[where(FINITE(trmm_precip) eq 1)]
yotc_temp=yotc_precip[where(FINITE(yotc_precip) eq 1)]
trmm_precip_pct=fltarr(N_ELEMENTS(trmm_temp))
yotc_precip_pct=fltarr(N_ELEMENTS(yotc_temp))
trmm_precip_pct[SORT(trmm_temp)]=100.*findgen(N_ELEMENTS(trmm_temp))/FLOAT(N_ELEMENTS(trmm_temp))
yotc_precip_pct[SORT(yotc_temp)]=100.*findgen(N_ELEMENTS(yotc_temp))/FLOAT(N_ELEMENTS(yotc_temp))

trmm_mean_precip=fltarr(n_bins+1)
yotc_mean_precip=fltarr(n_bins+1)
FOR i=0,n_bins DO BEGIN
   CASE i OF
      0 : BEGIN
         trmm_valid_points=where(trmm_precip_pct lt precip_bins(0))
         yotc_valid_points=where(yotc_precip_pct lt precip_bins(0))
      END
      n_bins : BEGIN
         trmm_valid_points=where(trmm_precip_pct ge precip_bins(n_bins-1))
         yotc_valid_points=where(yotc_precip_pct ge precip_bins(n_bins-1))
      END
      else : BEGIN
         trmm_valid_points=where(trmm_precip_pct ge precip_bins(i-1) and trmm_precip_pct lt precip_bins(i))
         yotc_valid_points=where(yotc_precip_pct ge precip_bins(i-1) and yotc_precip_pct lt precip_bins(i))
      END
   ENDCASE
   IF TOTAL(trmm_valid_points) ge 0 THEN BEGIN
      trmm_mean_precip(i)=MEAN(trmm_temp[trmm_valid_points],/NaN)
   ENDIF ELSE $
      trmm_mean_precip(i)=!Values.F_NaN
   IF TOTAL(yotc_valid_points) ge 0 THEN BEGIN
      yotc_mean_precip(i)=MEAN(yotc_temp[yotc_valid_points],/NaN)
   ENDIF ELSE $
      yotc_mean_precip(i)=!Values.F_NaN
ENDFOR
IF TOTAL(where(trmm_mean_precip lt 0.05)) ge 0 THEN $
   trmm_mean_precip[where(trmm_mean_precip lt 0.05)]=!Values.F_NaN
IF TOTAL(where(yotc_mean_precip lt 0.05)) ge 0 THEN $
   yotc_mean_precip[where(yotc_mean_precip lt 0.05)]=!Values.F_NaN

yotc_dp=fltarr(n_bins)
trmm_dp=fltarr(n_bins)
yotc_dp2=fltarr(n_bins)
trmm_dp2=fltarr(n_bins)
FOR i=0,n_bins-1 DO BEGIN
   yotc_dp(i)=ALOG10(yotc_mean_precip(i+1))-ALOG10(yotc_mean_precip(i))
   trmm_dp(i)=ALOG10(trmm_mean_precip(i+1))-ALOG10(trmm_mean_precip(i))
ENDFOR
FOR i=1,n_bins-1 DO BEGIN
   yotc_dp2(i)=yotc_dp(i)-yotc_dp(i-1)
   trmm_dp2(i)=trmm_dp(i)-trmm_dp(i-1)
ENDFOR
yotc_dp2(0)=!Values.F_NaN
trmm_dp2(0)=!Values.F_NaN
trmm_pt=where(ABS(trmm_dp2) eq MIN(ABS(trmm_dp2),/NaN))
yotc_pt=where(ABS(yotc_dp2) eq MIN(ABS(yotc_dp2),/NaN))

psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_percentiles.'+$
       STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'_leads'+STRTRIM(STRING(MIN(lead_times)),1)+$
       'to'+STRTRIM(STRING(MAX(lead_times)),1)+'_'+box_name+'.ps'
PSOPEN,file=psfile,FONT=3,CHARSIZE=120,MARGIN=1500,XOFFSET=1500,YOFFSET=1000
GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.05,YMAX=70,/YLOG
GPLOT,X=indgen(n_bins+1)+0.5,Y=trmm_mean_precip,COL=FSC_COLOR('black'),STYLE=0
GPLOT,X=trmm_pt+0.5,Y=trmm_mean_precip(trmm_pt),COL=FSC_COLOR('black'),SYM=3
GPLOT,X=trmm_pt+0.5,Y=0.06,TEXT='TR'
GPLOT,X=indgen(n_bins+1)+0.5,Y=yotc_mean_precip,COL=FSC_COLOR('black'),STYLE=2
GPLOT,X=yotc_pt+0.5,Y=yotc_mean_precip(yotc_pt),COL=FSC_COLOR('black'),SYM=3
GPLOT,X=yotc_pt+0.5,Y=0.06,TEXT='YO'

n_text(trmm_pt)=1
n_text(yotc_pt)=1

all_colors=strarr(n_models)
all_descs=strarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   print,our_model_names(i)
   CASE our_model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         file_desc='ECMWF_IFS'
         all_descs(i)='EC'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=1.
         missing_value=1e10
         has_landsea=1    
         all_colors(i)='red'
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         file_desc='GEOS5_AGCM'
         all_descs(i)='NA'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         precip_multiplier=86400.
         missing_value=10000.
         has_landsea=0
         all_colors(i)='cyan'
      END    
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         file_desc='MRI-AGCM'
         all_descs(i)='MR'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         precip_multiplier=86400.
         missing_value=10000.
         has_landsea=0
         all_colors(i)='blue'
      END  
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_desc='miroc5'
         all_descs(i)='MI'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=999.
         has_landsea=0
         all_colors(i)='orange'
      END    
      'nrl' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         file_desc='NGEM01'
         all_descs(i)='NR'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=10000.
         has_landsea=0
         all_colors(i)='brown'
      END      
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         file_desc='MetUM'
         all_descs(i)='MO'
         valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(22)+20091101,20091124,20091125,$
                                     indgen(22)+20091210,indgen(6)+20100101,indgen(18)+20100108]),1)
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=100000.
         has_landsea=1
         all_colors(i)='darkgoldenrod'
      END      
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_desc='ModelE'
         all_descs(i)='GI'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=1.
         missing_value=1e5
         has_landsea=0
         all_colors(i)='violetred'
      END      
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_desc='CanCM4'
         all_descs(i)='CC'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=1e4
         has_landsea=0
         all_colors(i)='dodgerblue'
      END  
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         file_desc='SPCAM3.0'
         all_descs(i)='SP'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=1e5
         has_landsea=0
         all_colors(i)='purple'
      END 
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         file_desc='NCAR.CAM5'
         all_descs(i)='C5'
         valid_dates=cam5_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.*1000.
         missing_value=1e5
         has_landsea=1
         all_colors(i)='deeppink'
      END 
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_desc='CAM5ZMMicroCAPT'
         all_descs(i)='CZ'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=1e5
         has_landsea=0
         all_colors(i)='steelblue'
      END 
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         file_desc='ecearth3'
         all_descs(i)='E3'
         valid_dates=ecearth_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         precip_multiplier=86400.
         missing_value=1e5
         has_landsea=0
         all_colors(i)='limegreen'
      END 
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         file_desc='CNRM'
         all_descs(i)='CN'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         precip_multiplier=86400.
         missing_value=1e5
         has_landsea=0
         all_colors(i)='navy'
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
   temp=model_precip[where(FINITE(model_precip) eq 1)]
   model_precip_pct=fltarr(N_ELEMENTS(temp))
   model_precip_pct[SORT(temp)]=100.*findgen(N_ELEMENTS(temp))/FLOAT(N_ELEMENTS(temp))   
   
   model_mean_precip=fltarr(n_bins+1)
   FOR j=0,n_bins DO BEGIN
      CASE j OF
         0 : BEGIN
            valid_points=where(model_precip_pct lt precip_bins(0))            
         END
         n_bins : BEGIN
            valid_points=where(model_precip_pct ge precip_bins(n_bins-1))
         END
         else : BEGIN
            valid_points=where(model_precip_pct ge precip_bins(j-1) and model_precip_pct lt precip_bins(j))            
         END
      ENDCASE
      IF TOTAL(valid_points) ge 0 THEN BEGIN
         model_mean_precip(j)=MEAN(temp[valid_points],/NaN)
      ENDIF ELSE $
         model_mean_precip(j)=!Values.F_NaN
   ENDFOR

   IF TOTAL(where(model_mean_precip lt 0.05)) ge 0 THEN $
      model_mean_precip[where(model_mean_precip lt 0.05)]=!Values.F_NaN

   GPLOT,X=indgen(n_bins+1)+0.5,Y=model_mean_precip,COL=FSC_COLOR(all_colors(i))
   model_dp=fltarr(n_bins)
   model_dp2=fltarr(n_bins)
   FOR j=0,n_bins-1 DO $
      model_dp(j)=ALOG10(model_mean_precip(j+1))-ALOG10(model_mean_precip(j))
   npts=0
   model_dp2(0)=!Values.F_NaN
   model_pt=[0]
   FOR j=1,n_bins-1 DO BEGIN
      model_dp2(j)=model_dp(j)-model_dp(j-1)   
      IF model_dp2(j)*model_dp2(j-1) le 0 THEN BEGIN
         npts=npts+1
         model_pt=[model_pt,j]
      ENDIF
   ENDFOR
   print,npts,model_pt
   IF npts gt 0 THEN BEGIN
      FOR j=0,npts-1 DO BEGIN
         GPLOT,X=model_pt(j+1)+0.5,Y=model_mean_precip[model_pt(j+1)],COL=FSC_COLOR(all_colors(i)),SYM=3      
         GPLOT,X=model_pt(j+1)+0.5,Y=0.06+0.02*n_text(model_pt(j+1)),TEXT=all_descs(i),COL=FSC_COLOR(all_colors(i))
         n_text(model_pt(j+1))=n_text(model_pt(j+1))+1
      ENDFOR
   ENDIF
ENDFOR

AXES,XVALS=indgen(n_bins+2),XLABELS=['< '+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins),1),0,4),'> '+$
                                     STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,4)],$
     ORIENTATION=30,XTITLE='Precipitation percentile (across entire space-time domain)',$
     YVALS=[0.05,0.07,0.1,0.15,0.2,0.3,0.5,0.7,1.0,1.5,2.0,3.0,5.0,7.0,10,15,20,30,50,70],$
     YLABELS=['0.05','0.07','0.1','0.15','0.2',$
              '0.3','0.5','0.7','1.0','1.5','2.0','3.0','5.0','7.0','10.0','15.0','20.0','30.0','50.0','70.0'],$
     YTITLE='Mean precipitation rate in percentile bin (mm day!U-1!N)',NDECS=2
GLEGEND,labels=REVERSE(['YoTC','TRMM',all_descs]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR(all_colors)]),$
        LEGPOS=1,STYLE=REVERSE([2,0,REPLICATE(0,n_models)])
PSCLOSE

STOP
END
