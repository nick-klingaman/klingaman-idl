PRO mjo_diabatic_tendencies_moisture_cwv,model_names=model_names,start_date,stop_date,box,region_name,lead_times=lead_times

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','mri','miroc','nasa','nrl','spcam','nicam','giss','cancm4','cnrm_atmos','ecearth','cam5zm','metum','cam5']
n_models=N_ELEMENTS(our_model_names)
IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)
n_times_per_day=8

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
cam5_valid_dates=standard_valid_dates[where(standard_valid_dates ne '20091014' and standard_valid_dates ne '20091015')]
dates=STRTRIM(STRING([indgen(22)+20091010,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,indgen(28)+20100201]),1)
metum_valid_dates=standard_valid_dates

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
n_days=FLOOR(stop_julian-start_julian)

model_colors=strarr(n_models)
model_names=strarr(n_models)
file_descs=strarr(n_models)
reverse_levels=intarr(n_models)

total_precip=fltarr(n_models,5,4)
;frac_nearzero_points=fltarr(n_models);,n_lead_times)

precip_bins=[0.1,0.3,0.6,1.0,1.5,2.0,3.0,5.0,7.0,9.0,12.0,16.0,20,25,30]
n_precip_bins=N_ELEMENTS(precip_bins)
prw_bins=[36,38,40,42,44,46,48,50,52,54,56,58,60,62,64]
n_prw_bins=N_ELEMENTS(prw_bins)

prw_by_precip=fltarr(n_models,n_precip_bins+1,8)
dprw_by_precip=fltarr(n_models,n_precip_bins+1)
prwdiff_by_precip=fltarr(n_models,n_precip_bins+1,7)
prwdiff_by_prw=fltarr(n_models,n_prw_bins+1,7)

FOR i=0,n_models-1 DO BEGIN
   print,our_model_names(i)
   CASE our_model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         file_descs(i)='ECMWF_IFS'
         model_names(i)='EC'
         model_colors(i)='red'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e10
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         file_descs(i)='MRI-AGCM'
         model_names(i)='MR'
         model_colors(i)='blue'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         reverse_levels(i)=0
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e10
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_descs(i)='miroc5'
         model_names(i)='MI'
         model_colors(i)='orange'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1000
      END
      'nrl' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         file_descs(i)='NGEM01'
         model_names(i)='NR'
         model_colors(i)='brown'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=1
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=100000
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         file_descs(i)='GEOS5_AGCM'
         model_names(i)='NA'
         model_colors(i)='cyan'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'        
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_descs(i)='ModelE'
         model_names(i)='GI'
         model_colors(i)='violetred'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_descs(i)='CanCM4'
         model_names(i)='CC'
         model_colors(i)='dodgerblue'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e20
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         file_descs(i)='SPCAM3.0'
         model_names(i)='SP'
         model_colors(i)='purple'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev_p'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         file_descs(i)='MetUM'
         model_names(i)='MO'
         model_colors(i)='darkgoldenrod'
         valid_dates=metum_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'        
         reverse_levels(i)=0
         multiplier=72.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         file_descs(i)='NCAR.CAM5'
         model_names(i)='C5'
         model_colors(i)='deeppink'
         valid_dates=cam5_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='levels'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.*1000.
         level_multiplier=1.
         missing_value=1e6
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_descs(i)='CAM5ZMMicroCAPT'
         model_names(i)='CZ'
         model_colors(i)='steelblue'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         file_descs(i)='CNRM'
         model_names(i)='CN'
         model_colors(i)='navy'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         file_descs(i)='ecearth3'
         model_names(i)='E3'
         model_colors(i)='limegreen'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e6
      END
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         file_descs(i)='nicam'
         model_names(i)='NI'        
         model_colors(i)='olive'
         valid_dates=['20091015','20091020','20091025','20091030','20091104','20091109',$
                      '20091215','20091220','20091225','20091230','20100104','20100109']
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END     
   ENDCASE
   
   grid_flag=0
   start_position=REFORM(where(dates eq start_date))
   FOR j=0,n_days-1 DO BEGIN
      ;print,TOTAL(where(valid_dates eq dates(j+start_position(0))))
      IF TOTAL(where(valid_dates eq dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq dates(j+start_position(0))))
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_descs(i)+'.pr.'+REFORM(initial_date)+'.00Z.nc'
            model_prw_infile=indir+'/'+REFORM(initial_date)+'/'+file_descs(i)+'.prw.'+REFORM(initial_date)+'.00Z.nc'

            IF grid_flag eq 0 THEN BEGIN 
               model_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)            
               model_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_longitude)
               model_nlat=N_ELEMENTS(model_latitude)
               grid_flag=1
               model_prw=fltarr(model_nlon,model_nlat,n_days,n_times_per_day*n_lead_times)
               model_precip=fltarr(model_nlon,model_nlat,n_days,n_times_per_day*n_lead_times)
            ENDIF
            
            model_precip(*,*,j,k*n_times_per_day:(k+1)*n_times_per_day-1)=$
               OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])*precip_multiplier
            model_prw(*,*,j,k*n_times_per_day:(k+1)*n_times_per_day-1)=$
               OPEN_AND_EXTRACT(model_prw_infile(0),'prw',$
                                offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])      
            IF TOTAL(where(ABS(model_prw) ge 1000)) ge 0 THEN BEGIN
               model_precip[where(ABS(model_prw ge 1000))]=!Values.F_NaN
               model_prw[where(ABS(model_prw ge 1000))]=!Values.F_NaN               
            ENDIF
            IF TOTAL(where(ABS(model_prw) eq 0)) ge 0 THEN BEGIN
               model_precip[where(model_prw eq 0)]=!Values.F_NaN
               model_prw[where(ABS(model_prw eq 0))]=!Values.F_NaN
            ENDIF
         ENDFOR         
      ENDIF
   ENDFOR
   
   model_prw_diff=fltarr(model_nlon,model_nlat,n_days,n_times_per_day*n_lead_times)
   FOR j=0,model_nlon-1 DO $
      FOR k=0,model_nlat-1 DO $
         FOR m=0,n_days-1 DO $
            FOR n=1,n_times_per_day*n_lead_times-1 DO $
               model_prw_diff(j,k,m,n)=model_prw(j,k,m,n)-model_prw(j,k,m,n-1)
  
   FOR j=0,n_precip_bins-2 DO BEGIN
      temp=where(model_precip gt precip_bins(j) and model_precip le precip_bins(j+1))
      IF TOTAL(temp) ge 0 THEN BEGIN
         temp_prw=model_prw[temp]
         sorted=temp_prw[SORT(temp_prw)]
         prw_by_precip(i,j+1,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
         prw_by_precip(i,j+1,1)=sorted(N_ELEMENTS(temp_prw)/4)
         prw_by_precip(i,j+1,2)=MEDIAN(temp_prw)
         prw_by_precip(i,j+1,3)=MEAN(temp_prw,/NaN)
         prw_by_precip(i,j+1,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
         prw_by_precip(i,j+1,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
         prw_by_precip(i,j+1,6)=N_ELEMENTS(temp_prw)
         prw_by_precip(i,j+1,7)=MEAN(model_precip[temp],/NaN)

         temp_prw=model_prw_diff[temp]
         sorted=temp_prw[SORT(temp_prw)]
         prwdiff_by_precip(i,j+1,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
         prwdiff_by_precip(i,j+1,1)=sorted(N_ELEMENTS(temp_prw)/4)
         prwdiff_by_precip(i,j+1,2)=MEDIAN(temp_prw)
         prwdiff_by_precip(i,j+1,3)=MEAN(temp_prw,/NaN)
         prwdiff_by_precip(i,j+1,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
         prwdiff_by_precip(i,j+1,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
         prwdiff_by_precip(i,j+1,6)=N_ELEMENTS(temp_prw)       
      ENDIF
   ENDFOR
   temp=where(model_precip lt precip_bins(0))
   IF TOTAL(temp) ge 0 THEN BEGIN
      temp_prw=model_prw[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prw_by_precip(i,0,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prw_by_precip(i,0,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prw_by_precip(i,0,2)=MEDIAN(temp_prw)
      prw_by_precip(i,0,3)=MEAN(temp_prw,/NaN)
      prw_by_precip(i,0,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prw_by_precip(i,0,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prw_by_precip(i,0,6)=N_ELEMENTS(temp_prw)
      prw_by_precip(i,0,7)=MEAN(model_precip[temp],/NaN)
      
      temp_prw=model_prw_diff[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prwdiff_by_precip(i,0,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prwdiff_by_precip(i,0,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prwdiff_by_precip(i,0,2)=MEDIAN(temp_prw)
      prwdiff_by_precip(i,0,3)=MEAN(temp_prw,/NaN)
      prwdiff_by_precip(i,0,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prwdiff_by_precip(i,0,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prwdiff_by_precip(i,0,6)=N_ELEMENTS(temp_prw)      
   ENDIF
   temp=where(model_precip ge precip_bins(n_precip_bins-1))
   IF TOTAL(temp) ge 0 THEN BEGIN
      temp_prw=model_prw[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prw_by_precip(i,n_precip_bins,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prw_by_precip(i,n_precip_bins,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prw_by_precip(i,n_precip_bins,2)=MEDIAN(temp_prw)
      prw_by_precip(i,n_precip_bins,3)=MEAN(temp_prw,/NaN)
      prw_by_precip(i,n_precip_bins,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prw_by_precip(i,n_precip_bins,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prw_by_precip(i,n_precip_bins,6)=N_ELEMENTS(temp_prw)
      prw_by_precip(i,n_precip_bins,7)=MEAN(model_precip[temp],/NaN)

      temp_prw=model_prw_diff[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prwdiff_by_precip(i,n_precip_bins,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prwdiff_by_precip(i,n_precip_bins,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prwdiff_by_precip(i,n_precip_bins,2)=MEDIAN(temp_prw)
      prwdiff_by_precip(i,n_precip_bins,3)=MEAN(temp_prw,/NaN)
      prwdiff_by_precip(i,n_precip_bins,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prwdiff_by_precip(i,n_precip_bins,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prwdiff_by_precip(i,n_precip_bins,6)=N_ELEMENTS(temp_prw)   
   ENDIF
   FOR j=1,n_precip_bins DO $
      dprw_by_precip(i,j)=(prw_by_precip(i,j,3)-prw_by_precip(i,j-1,3))/$
      (prw_by_precip(i,j,7)-prw_by_precip(i,j-1,7))
   dprw_by_precip(i,0)=!Values.F_NaN

   FOR j=0,n_prw_bins-2 DO BEGIN
      temp=where(model_prw gt prw_bins(j) and model_prw le prw_bins(j+1))
      IF TOTAL(temp) ge 0 THEN BEGIN
         temp_prw=model_prw_diff[temp]
         sorted=temp_prw[SORT(temp_prw)]
         prwdiff_by_prw(i,j+1,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
         prwdiff_by_prw(i,j+1,1)=sorted(N_ELEMENTS(temp_prw)/4)
         prwdiff_by_prw(i,j+1,2)=MEDIAN(temp_prw)
         prwdiff_by_prw(i,j+1,3)=MEAN(temp_prw,/NaN)
         prwdiff_by_prw(i,j+1,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
         prwdiff_by_prw(i,j+1,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
         prwdiff_by_prw(i,j+1,6)=N_ELEMENTS(temp_prw)       
      ENDIF
   ENDFOR
   temp=where(model_prw lt prw_bins(0))
   IF TOTAL(temp) ge 0 THEN BEGIN
      temp_prw=model_prw_diff[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prwdiff_by_prw(i,0,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prwdiff_by_prw(i,0,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prwdiff_by_prw(i,0,2)=MEDIAN(temp_prw)
      prwdiff_by_prw(i,0,3)=MEAN(temp_prw,/NaN)
      prwdiff_by_prw(i,0,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prwdiff_by_prw(i,0,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prwdiff_by_prw(i,0,6)=N_ELEMENTS(temp_prw)   
   ENDIF
   temp=where(model_prw ge prw_bins(n_prw_bins-1))
   IF TOTAL(temp) ge 0 THEN BEGIN
      temp_prw=model_prw_diff[temp]
      sorted=temp_prw[SORT(temp_prw)]
      prwdiff_by_prw(i,n_prw_bins,0)=sorted(N_ELEMENTS(temp_prw)*5/100)
      prwdiff_by_prw(i,n_prw_bins,1)=sorted(N_ELEMENTS(temp_prw)/4)
      prwdiff_by_prw(i,n_prw_bins,2)=MEDIAN(temp_prw)
      prwdiff_by_prw(i,n_prw_bins,3)=MEAN(temp_prw,/NaN)
      prwdiff_by_prw(i,n_prw_bins,4)=sorted(N_ELEMENTS(temp_prw)*3/4)
      prwdiff_by_prw(i,n_prw_bins,5)=sorted(N_ELEMENTS(temp_prw)*95/100)
      prwdiff_by_prw(i,n_prw_bins,6)=N_ELEMENTS(temp_prw)   
   ENDIF
   
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/moisture/mjo_diabatic_tendencies_moisture_cwv.prw_precip_box.'+$
          our_model_names(i)+'.'+STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.leads'+$
          STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,MARGIN=2500,YOFFSET=500,XOFFSET=500
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=10,YMAX=70
   FOR j=0,n_precip_bins DO $
      EBAR,X=j+0.3,BOX=[prw_by_precip(i,j,0),$
                        prw_by_precip(i,j,1),$
                        prw_by_precip(i,j,2),$
                        prw_by_precip(i,j,4),$
                        prw_by_precip(i,j,5)],COL=FSC_COLOR('blue'),WIDTH=50
   GPLOT,X=indgen(n_precip_bins+1)+0.3,Y=REFORM(prw_by_precip(i,*,3)),COL=FSC_COLOR('blue')
   AXES,XVALS=indgen(n_precip_bins+2),YSTEP=5,YMINOR=2.5,$
        XLABELS=['<'+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins(0:n_precip_bins-1)),1),0,4),$
                 '>'+STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
        XTITLE='Precipitation (mm day!U-1!N)',YTITLE='Column water vapor (mm)',/NORIGHT,/NOUPPER
   GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=-20,YMAX=20
   FOR j=0,n_precip_bins DO $
      EBAR,X=j+0.5,BOX=[prwdiff_by_precip(i,j,0),$
                        prwdiff_by_precip(i,j,1),$
                        prwdiff_by_precip(i,j,2),$
                        prwdiff_by_precip(i,j,4),$
                        prwdiff_by_precip(i,j,5)]*8.,COL=FSC_COLOR('red'),WIDTH=50
   GPLOT,X=indgen(n_precip_bins+1)+0.5,Y=REFORM(prwdiff_by_precip(i,*,3)),COL=FSC_COLOR('red')
   GPLOT,X=[0,n_precip_bins+1],Y=[0,0],STYLE=2,COL=FSC_COLOR('black')
   AXES,YSTEP=2,YMINOR=1,YTITLE='Tendency of column water vapor (mm day!U-1!N)',/ONLYRIGHT
   GSET,XMIN=0,XMAX=n_prw_bins+1,YMIN=-20,YMAX=20
   FOR j=0,n_prw_bins DO $
      EBAR,X=j+0.7,BOX=[prwdiff_by_prw(i,j,0),$
                        prwdiff_by_prw(i,j,1),$
                        prwdiff_by_prw(i,j,2),$
                        prwdiff_by_prw(i,j,4),$
                        prwdiff_by_prw(i,j,5)]*8.,COL=FSC_COLOR('purple'),WIDTH=50
   GPLOT,X=indgen(n_prw_bins+1)+0.7,Y=REFORM(prwdiff_by_prw(i,*,3)),COL=FSC_COLOR('purple')
   AXES,XVALS=indgen(n_prw_bins+2),$
        XLABELS=['<'+STRMID(STRTRIM(STRING(prw_bins(0)),1),0,4),STRMID(STRTRIM(STRING(prw_bins(0:n_prw_bins-1)),1),0,4),$
                 '>'+STRMID(STRTRIM(STRING(prw_bins(n_prw_bins-1)),1),0,4)],XTITLE='Column water vapor (mm)',/ONLYUPPER

   GLEGEND,labels=['d(CWV) by CWV','d(CWV) by precip','CWV by precip'],COL=[FSC_COLOR('purple'),FSC_COLOR('red'),FSC_COLOR('blue')],$
           LEGPOS=11
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/tendencies/moisture/mjo_diabatic_tendencies_moisture_cwv.prw_precip_box.'+$
       'all_models_prw_by_precip.'+STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.leads'+$
       STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,MARGIN=2500,YOFFSET=500,XOFFSET=300,XSIZE=23000
GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=30,YMAX=70
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_precip_bins+1)+0.5,Y=REFORM(prw_by_precip(i,*,2)),COL=FSC_COLOR(model_colors(i)),STYLE=0
AXES,XVALS=indgen(n_precip_bins+2),YSTEP=5,YMINOR=2.5,$
     XLABELS=['<'+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins(0:n_precip_bins-1)),1),0,4),$
              '>'+STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
     XTITLE='Precipitation (mm day!U-1!N)',YTITLE='Median column water vapor (mm)'
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=15000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/tendencies/moisture/mjo_diabatic_tendencies_moisture_cwv.prw_precip_box.'+$
       'all_models_prwdiff_by_precip.'+STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.leads'+$
       STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,MARGIN=2500,YOFFSET=500,XOFFSET=300,XSIZE=23000
GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=-3,YMAX=8
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_precip_bins+1)+0.5,Y=REFORM(prwdiff_by_precip(i,*,2))*8.,COL=FSC_COLOR(model_colors(i)),STYLE=0
GPLOT,X=[0,n_precip_bins+1],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,XVALS=indgen(n_precip_bins+2),$
     XLABELS=['<'+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins(0:n_precip_bins-1)),1),0,4),$
              '>'+STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],$
     XTITLE='Precipitation (mm day!U-1!N)',YSTEP=0.5,YMINOR=0.25,YTITLE='Median tendency of column water vapor (mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=15000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/tendencies/moisture/mjo_diabatic_tendencies_moisture_cwv.prw_precip_box.'+$
       'all_models_prwdiff_by_prw.'+STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.leads'+$
       STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,MARGIN=2500,YOFFSET=500,XOFFSET=300,XSIZE=23000
GSET,XMIN=0,XMAX=n_prw_bins+1,YMIN=-3,YMAX=5
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_prw_bins+1)+0.5,Y=REFORM(prwdiff_by_prw(i,*,2))*8.,COL=FSC_COLOR(model_colors(i)),STYLE=0
GPLOT,X=[0,n_prw_bins+1],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,XVALS=indgen(n_prw_bins+2),$
     XLABELS=['<'+STRMID(STRTRIM(STRING(prw_bins(0)),1),0,4),STRMID(STRTRIM(STRING(prw_bins(0:n_prw_bins-1)),1),0,4),$
              '>'+STRMID(STRTRIM(STRING(prw_bins(n_prw_bins-1)),1),0,4)],XTITLE='Column water vapor (mm)',$
     YSTEP=0.5,YMINOR=0.25,YTITLE='Median tendency of column water vapor (mm day!U-1!N)',NDECS=1
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=15000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_diabatic/tendencies/moisture/mjo_diabatic_tendencies_moisture_cwv.prw_precip_box.'+$
       'all_models_dprw_by_precip.'+STRTRIM(STRING(start_date),1)+'-'+STRTRIM(STRING(stop_date),1)+'.leads'+$
       STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,CHARSIZE=120,MARGIN=2500,YOFFSET=500,XOFFSET=300,XSIZE=23000
GSET,XMIN=0,XMAX=n_precip_bins+1,YMIN=0.4,YMAX=300,/YLOG
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(n_precip_bins+1)+0.5,Y=REFORM(dprw_by_precip(i,*))*8.,COL=FSC_COLOR(model_colors(i)),STYLE=0
GPLOT,X=[0,n_precip_bins+1],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,XVALS=indgen(n_precip_bins+2),$
     XLABELS=['<'+STRMID(STRTRIM(STRING(precip_bins(0)),1),0,4),STRMID(STRTRIM(STRING(precip_bins(0:n_precip_bins-1)),1),0,4),$
              '>'+STRMID(STRTRIM(STRING(precip_bins(n_precip_bins-1)),1),0,4)],XTITLE='Precipitation (mm day!U-1!N)',$
     YVALS=['0.4','0.5','0.7','1.0',$
            '1.5','2.0','3.0','4.0','5.0','7.0','10','15','20','30','40','50','70','100','150','200','300'],$
     YTITLE='d(CWV)/dp (day), using mean CWV in each precipitation bin',NDECS=2
GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=15000
PSCLOSE

STOP
END
