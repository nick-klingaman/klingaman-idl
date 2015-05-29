PRO mjo_diabatic_tendencies_composite_precip_q_manyquartiles_10x10,model_names=model_names,inc_type,start_date,stop_date,box,region_name,lead_times=lead_times,plot_yotc=plot_yotc

; Composite specific humidity tendencies by suppressed/transition/active MJO
; using quartiles of precipitation as delimiter.  Plot all quartiles on a single graph for each model.

; Needs type of increment to plot ('physics','radiation','total') and box within which to composite
; (e.g., W or E eqIO)

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','mri','miroc','nasa','nrl','spcam','nicam','giss','cancm4','cam5zm','cnrm_atmos','metum','ecearth']

n_models=N_ELEMENTS(our_model_names)
IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)
n_times_per_day=8

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
cam5_valid_dates=standard_valid_dates[where(standard_valid_dates ne '20091014' and standard_valid_dates ne '20091015')]
metum_valid_dates=standard_valid_dates[where(standard_valid_dates ne '20091022' and standard_valid_dates ne '20091011' $
                                             and standard_valid_dates ne '20091025' and standard_valid_dates ne '20100111'$
                                             and standard_valid_dates ne '20100114' and standard_valid_dates ne '20100116')]
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
n_days=FLOOR(stop_julian-start_julian)

model_nz=22
IF KEYWORD_SET(plot_yotc) THEN BEGIN
   model_increments=fltarr(n_models+1,5,4,model_nz)
   total_precip=fltarr(n_models+1,5,4)
ENDIF ELSE BEGIN
   model_increments=fltarr(n_models,5,4,model_nz) ;,n_lead_times)
   total_precip=fltarr(n_models,5,4)
ENDELSE

model_colors=strarr(n_models)
model_names=strarr(n_models)
file_descs=strarr(n_models)
reverse_levels=intarr(n_models)

IF KEYWORD_SET(plot_yotc) THEN BEGIN
   yotc_dir='/home/ss901165/datasets/ECMWF_YOTC'
   pr_file=yotc_dir+'/ECMWF_YOTC.oct2009-feb2010_3hrmeans.pr.10x10.nc'
   CASE inc_type OF
      'physics' : BEGIN
         increment_name='tnhus_physics'
         inc_file=yotc_dir+'/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnhus_physics.10x10.nc'
         xmin=[-3.9,-4,-4,-4,-4]
         xmax=[3.9,4,6,6,6]
         xstep=[0.6,0.5,0.6,0.5,0.5]
         legxoffset=500
         legyoffset=25000
      END
      'advection' : BEGIN
         increment_name='tnhusa'
         inc_file=yotc_dir+'/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnhusa.10x10.nc'
         xmin=[-4.0,-4,-8,-6,-6]
         xmax=[4.0,3,4,4,4]
         xstep=[0.5,0.5,0.8,0.6,0.6] 
      END
      'all' : BEGIN
         increment_name='tnhus'
         inc_file=yotc_dir+'/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnhus.10x10.nc'
         xmin=[-0.6,-2,-2,-1,-1]
         xmax=[0.6,2,2,1,1]
         xstep=[0.2,0.4,0.4,0.2,0.2]
         legxoffset=-7000
         legyoffset=25000
      END      
   ENDCASE
      
   pr_longitude=OPEN_AND_EXTRACT(pr_file,'longitude')
   pr_latitude=OPEN_AND_EXTRACT(pr_file,'latitude')
   DEFINE_BOUNDARIES,box,pr_latitude,pr_longitude,pr_box_tx,/LIMIT
   nlon=N_ELEMENTS(pr_longitude)
   nlat=N_ELEMENTS(pr_latitude)        

   inc_longitude=OPEN_AND_EXTRACT(inc_file,'longitude')
   inc_latitude=OPEN_AND_EXTRACT(inc_file,'latitude')
   DEFINE_BOUNDARIES,box,inc_latitude,inc_longitude,inc_box_tx,/LIMIT

   z=OPEN_AND_EXTRACT(inc_file,'p')
   nz=N_ELEMENTS(z)
   ntime=1207
   n_times_per_day=8
   n_days=ntime/n_times_per_day
   time_offset=0
   precip_multiplier=1000.*8

   precip=OPEN_AND_EXTRACT(pr_file,'TP',$
                           offset=[pr_box_tx(1),pr_box_tx(0),time_offset],$
                           count=[nlon,nlat,ntime])*precip_multiplier
   
   this_increment=OPEN_AND_EXTRACT(inc_file,increment_name,$
                                   offset=[inc_box_tx(1),inc_box_tx(0),0,1],$
                                   count=[nlon,nlat,nz,ntime])*8.*1000.

   precip_quartiles=fltarr(3,n_times_per_day)
   precip_quartiles_npts=fltarr(n_times_per_day)
   precip_localtrend=fltarr(nlon,nlat,n_times_per_day*n_days)
   FOR m=0,n_times_per_day-1 DO BEGIN
      temp_precip=REFORM(precip(*,*,m:n_days*n_times_per_day-1:n_times_per_day),[nlon*nlat*n_days])
      IF TOTAL(where(temp_precip ge 1)) ge 0 THEN BEGIN
         temp_precip=temp_precip[where(temp_precip ge 1)]
         indices=SORT(temp_precip)         
         FOR p=1,3 DO $
            precip_quartiles(p-1,m)=temp_precip(indices(N_ELEMENTS(temp_precip)*p/4.))
         precip_quartiles_npts(m)=N_ELEMENTS(temp_precip)/4.
      ENDIF ELSE BEGIN
         precip_quartiles(*,m)=0.
         precip_quartiles_npts(m)=0.
      ENDELSE
   ENDFOR

   FOR m=0,nlon-1 DO BEGIN
      FOR n=0,nlat-1 DO BEGIN
         precip_ts=REFORM(precip(m,n,*))
         precip_ts=SMOOTH(precip_ts,9)
         FOR r=4,n_times_per_day*n_days-5 DO BEGIN
            trend=REGRESS(indgen(9),precip_ts(r-4:r+4))
            correlation=CORRELATE(indgen(9)*trend(0),precip_ts(r-4:r+4))
            IF trend(0) ge 0 and correlation ge 0.5 THEN BEGIN
               precip_localtrend(m,n,r)=1
            ENDIF ELSE IF trend(0) le 0 and correlation ge 0.5 THEN BEGIN
               precip_localtrend(m,n,r)=-1
            ENDIF ELSE $
               precip_localtrend(m,n,r)=0
         ENDFOR
      ENDFOR
   ENDFOR
   
   total_points=fltarr(5,4)
   FOR j=0,4 DO BEGIN ; Loop over quartiles
      FOR m=0,3 DO BEGIN ; Loop over precip trends (0=all, 1=down, 2=steady/not significant, 3=up)    
         FOR q=0,n_times_per_day-1 DO BEGIN
                                ; this_pt_olr=REFORM(olr(n,p,q:n_days*n_times_per_day-1:n_times_per_day,k))
            this_pt_pr=REFORM(precip(*,*,q:n_days*n_times_per_day-1:n_times_per_day))
            this_pt_trend=REFORM(precip_localtrend(*,*,q:n_days*n_times_per_day-1:n_times_per_day))
            CASE j OF
               0 : BEGIN
                  pr_points=where(this_pt_pr lt precip_quartiles(0,q) and this_pt_pr ge 1)
               END
               1 : BEGIN
                  pr_points=where(this_pt_pr lt precip_quartiles(1,q) and this_pt_pr ge precip_quartiles(0,q))
               END
               2 : BEGIN
                     pr_points=where(this_pt_pr lt precip_quartiles(2,q) and this_pt_pr ge precip_quartiles(1,q))
                  END
               3 : BEGIN
                  pr_points=where(this_pt_pr ge precip_quartiles(2,q))
               END
               4 : BEGIN
                  pr_points=where(this_pt_pr lt 1)
               END
            ENDCASE
            IF TOTAL(pr_points) ge 0 THEN BEGIN
               CASE m OF
                  0 : BEGIN
                     tr_points=where(precip_localtrend[pr_points] ge -9999) ; Select all pr_points
                  END
                  1 : BEGIN
                     tr_points=where(precip_localtrend[pr_points] eq -1)
                  END
                  2 : BEGIN
                     tr_points=where(precip_localtrend[pr_points] eq 0)
                  END
                  3 : BEGIN
                     tr_points=where(precip_localtrend[pr_points] eq 1)
                  END
               ENDCASE
               
               FOR r=0,nz-1 DO BEGIN
                  this_pt_inc=REFORM(this_increment(*,*,r,q:n_days*n_times_per_day-1:n_times_per_day))                 
                  this_pt_inc=this_pt_inc[pr_points]
                  IF TOTAL(tr_points) ne -1 THEN BEGIN
                     this_pt_inc=this_pt_inc[tr_points]
                     model_increments(n_models,j,m,r)=model_increments(n_models,j,m,r)+TOTAL(this_pt_inc,/NaN)
                  ENDIF
               ENDFOR
               
               IF TOTAL(tr_points) ne -1 THEN BEGIN
                  total_points(j,m)=total_points(j,m)+N_ELEMENTS(tr_points)
                  temp_pr=this_pt_pr[pr_points]
                  total_precip(n_models,j,m)=total_precip(n_models,j,m)+TOTAL(temp_pr[tr_points])
               ENDIF
            ENDIF
         ENDFOR
      ENDFOR
   ENDFOR
   
   FOR r=0,nz-1 DO $
      model_increments(n_models,*,*,r)=model_increments(n_models,*,*,r)/FLOAT(total_points(*,*))

   total_precip(n_models,*,*)=total_precip(n_models,*,*)/FLOAT(total_points(*,*))

   colors=['red','orange','cyan','blue','black']
   styles=[0,1,2,3]
   syms=[3,4,5,6]
   print,'ECMWF_YOTC',total_precip(n_models,*,0)
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manyquartiles_10x10.'+$
          'ECMWF_YOTC.'+inc_type+'.'+region_name+'.ps'      
   PSOPEN,file=psfile,FONT=2,CHARSIZE=180,MARGIN=3000,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=MAX(z)+25,YMAX=MIN(z) ;,TITLE=file_descs(m)+' '+inc_type+' dT/dt '+region_name+$
                                ;' at '+STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+' day(s) - '+start_date+' to '+stop_date     
   FOR j=0,4 DO BEGIN
      FOR k=0,0 DO BEGIN
;         IF reverse_levels(m) eq 0 THEN BEGIN
;            GPLOT,X=REVERSE(REFORM(model_increments(n_models,j,k,*))),Y=z,COL=FSC_COLOR(colors(j)),STYLE=styles(k),THICK=200
;         ENDIF ELSE $
            GPLOT,X=REFORM(model_increments(n_models,j,k,*)),Y=z,COL=FSC_COLOR(colors(j)),STYLE=styles(k),THICK=200
      ENDFOR
   ENDFOR   
   GPLOT,X=[0,0],Y=[MIN(z),MAX(z)+20],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(['4th Quartile','3rd Quartile','2nd Quartile','1st Quartile','< 1 mm day!U-1!N']),$
           COL=REVERSE([FSC_COLOR(['blue','cyan','orange','red','black'])]),LEGXOFFSET=legxoffset,LEGYOFFSET=legyoffset;,LEGPOS=legpos(1)
;   GLEGEND,labels=REVERSE(['All','Increasing','Steady/not sig','Decreasing']),$
;           STYLE=REVERSE([0,1,2,3]),LEGPOS=legpos(0),SYM=REVERSE(syms)
   AXES,XSTEP=xstep(0),XMINOR=xstep(0)/2.,YVALS=FLOOR(z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=1,/NORIGHT,/NOUPPER,ORIENTATION=30   
;   FOR j=0,3 DO $
;      GPLOT,X=xmax(0)*0.9,Y=350-25*j,TEXT=STRMID(STRTRIM(STRING(increments_frachigh(m,j)),1),0,4),COL=FSC_COLOR(colors(j))
;   GPLOT,X=xmax(0)*0.9,Y=375,TEXT=STRMID(STRTRIM(STRING(increments_frachigh(m,4)),1),0,4),COL=FSC_COLOR(colors(4))

   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=0,YMAX=20
   FOR j=0,3 DO $
      FOR k=0,0 DO $
         GPLOT,X=xmax(0),Y=total_precip(n_models,j,k),COL=FSC_COLOR(colors(j)),SYM=syms(k),SIZE=200
      
   AXES,YSTEP=1,YTITLE='Mean rainfall in bin (mm day!U-1!N)',/ONLYRIGHT,NDECS=1
   PSCLOSE   

   STOP

ENDIF

;frac_nearzero_points=fltarr(n_models);,n_lead_times)

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
         multiplier=86400.*1000.    
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
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='plev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
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
         multiplier=86400.*1000.
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
         multiplier=86400.*1000.
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
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'        
         reverse_levels(i)=0
         multiplier=86400.*1000.
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
         multiplier=86400.*1000.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_descs(i)='CanCM4'
         model_names(i)='CM'
         model_colors(i)='dodgerblue'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
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
         multiplier=86400.*1000
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         file_descs(i)='MetUM'
         model_names(i)='UM'
         model_colors(i)='steelblue'
         valid_dates=metum_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'        
         reverse_levels(i)=0
         multiplier=72.*1000
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6         
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
         file_descs(i)='NCAR.CAM5'
         model_names(i)='C5'
         valid_dates=cam5_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='levels'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.*1000.
         level_multiplier=1.
         missing_value=1e6
      END
      'cam5zm' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
         file_descs(i)='CAM5ZMMicroCAPT'
         model_names(i)='LL'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
         file_descs(i)='CNRM'
         model_names(i)='CN'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
         file_descs(i)='ecearth3'
         model_names(i)='E3'
         model_colors(i)='green'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
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
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END     
   ENDCASE

   CASE inc_type OF
      'physics' : BEGIN
         increment_name='tnhus_physics'
         xmin=[-3.9,-4,-4,-4,-4]
         xmax=[3.9,4,6,6,6]
         xstep=[0.6,0.5,0.6,0.5,0.5]
         legxoffset=500
         legyoffset=25000
      END
      'advection' : BEGIN
         increment_name='tnhusa'
         xmin=[-4.0,-4,-8,-6,-6]
         xmax=[4.0,3,4,4,4]
         xstep=[0.5,0.5,0.8,0.6,0.6] 
      END
      'all' : BEGIN
         increment_name='tnhus'
         xmin=[-0.6,-2,-2,-1,-1]
         xmax=[0.6,2,2,1,1]
         xstep=[0.2,0.4,0.4,0.2,0.2]
         legxoffset=-7000
         legyoffset=25000
      END      
   ENDCASE
                                ; Read all available precipitation data for region to compute quartiles
                                ; and classify into suppressed/transition/active
   grid_flag=0
   start_position=REFORM(where(dates eq start_date))
   FOR j=0,n_days-1 DO BEGIN
      ;print,TOTAL(where(valid_dates eq dates(j+start_position(0))))
      IF TOTAL(where(valid_dates eq dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq dates(j+start_position(0))))
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_descs(i)+'.pr.'+REFORM(initial_date)+'.00Z.10x10.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
               model_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(model_longitude)
               model_nlat=N_ELEMENTS(model_latitude)     
               model_nz=N_ELEMENTS(model_z)
               grid_flag=1
               model_olr=fltarr(model_nlon,model_nlat,n_days*n_times_per_day,n_lead_times)
               model_precip=fltarr(model_nlon,model_nlat,n_days*n_times_per_day,n_lead_times)
            ENDIF
            model_precip(*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
               OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])*precip_multiplier
         ENDFOR
      ENDIF ELSE BEGIN
         FOR k=0,n_lead_times-1 DO $
            model_precip(*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=!Values.F_NaN
      ENDELSE      
   ENDFOR
   IF TOTAL(where(ABS(model_precip) ge ABS(missing_value))) ge 0 THEN $
      model_precip[where(ABS(model_precip) ge ABS(missing_value))]=!Values.F_NaN
   print,'Read precipitation'
   
   model_precip_quartiles=fltarr(3,n_times_per_day,n_lead_times)
   model_precip_quartiles_npts=fltarr(n_times_per_day,n_lead_times)
   model_precip_localtrend=fltarr(model_nlon,model_nlat,n_times_per_day*n_days,n_lead_times)
   FOR m=0,n_times_per_day-1 DO BEGIN
      FOR n=0,n_lead_times-1 DO BEGIN
         temp_precip=REFORM(model_precip(*,*,m:n_days*n_times_per_day-1:n_times_per_day,n),[model_nlon*model_nlat*n_days])
         IF TOTAL(where(temp_precip ge 1)) ge 0 THEN BEGIN
            temp_precip=temp_precip[where(temp_precip ge 1)]
            indices=SORT(temp_precip)         
            FOR p=1,3 DO $
               model_precip_quartiles(p-1,m,n)=temp_precip(indices(N_ELEMENTS(temp_precip)*p/4.))
            model_precip_quartiles_npts(m,n)=N_ELEMENTS(temp_precip)/4.
         ENDIF ELSE BEGIN
            model_precip_quartiles(*,m,n)=0.
            model_precip_quartiles_npts(m,n)=0.
         ENDELSE
      ENDFOR
   ENDFOR
   FOR m=0,model_nlon-1 DO BEGIN
      FOR n=0,model_nlat-1 DO BEGIN
         FOR p=0,n_lead_times-1 DO BEGIN
            precip_ts=REFORM(model_precip(m,n,*,p))
            precip_ts=SMOOTH(precip_ts,9)
            FOR r=4,n_times_per_day*n_days-5 DO BEGIN
               trend=REGRESS(indgen(9),precip_ts(r-4:r+4))
               correlation=CORRELATE(indgen(9)*trend(0),precip_ts(r-4:r+4))
               IF trend(0) ge 0 and correlation ge 0.5 THEN BEGIN
                  model_precip_localtrend(m,n,r,p)=1
               ENDIF ELSE IF trend(0) le 0 and correlation ge 0.5 THEN BEGIN
                  model_precip_localtrend(m,n,r,p)=-1
               ENDIF ELSE $
                  model_precip_localtrend(m,n,r,p)=0
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
   print,'Computed quartiles and trends'
               
   grid_flag=0
   FOR j=0,n_days-1 DO BEGIN
      IF TOTAL(where(valid_dates eq dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq dates(j+start_position(0))))
            model_infile=indir+'/'+REFORM(initial_date)+'/'+file_descs(i)+'.'+increment_name+'.'+REFORM(initial_date)+'.00Z.10x10.nc'
            IF grid_flag eq 0 THEN BEGIN
               model_inc_longitude=OPEN_AND_EXTRACT(model_infile(0),longitude_name)
               model_inc_latitude=OPEN_AND_EXTRACT(model_infile(0),latitude_name)
               model_z=OPEN_AND_EXTRACT(model_infile(0),level_name)*level_multiplier
               DEFINE_BOUNDARIES,box,model_inc_latitude,model_inc_longitude,model_inc_box_tx,/LIMIT
               model_inc_nlon=N_ELEMENTS(model_longitude)
               model_inc_nlat=N_ELEMENTS(model_latitude)
               model_nz=N_ELEMENTS(model_z)                  
               grid_flag=1
               this_increment=fltarr(model_nlon,model_nlat,model_nz,n_days*n_times_per_day,n_lead_times)               
            ENDIF
            this_increment(*,*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
               OPEN_AND_EXTRACT(model_infile(0),increment_name,$
                                offset=[model_inc_box_tx(1),model_inc_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
                                count=[model_inc_nlon,model_inc_nlat,model_nz,n_times_per_day])*multiplier
         ENDFOR
      ENDIF
   ENDFOR
   IF TOTAL(where(ABS(this_increment) ge ABS(missing_value))) ge 0 THEN $
      this_increment[where(ABS(this_increment) ge ABS(missing_value))]=!Values.F_NaN
   print,'Read increments'
   
;   total_active_points=fltarr(n_lead_times)
;   total_suppressed_points=fltarr(n_lead_times)
;   total_transactive_points=fltarr(n_lead_times)
;   total_transsuppressed_points=fltarr(n_lead_times)
;   total_nearzero_points=fltarr(n_lead_times)
;   total_active_points=0
;   total_suppressed_points=0
;   total_transactive_points=0
;   total_transsuppressed_points=0
;   total_nearzero_points=0

   total_points=fltarr(5,4)
   
   FOR j=0,4 DO BEGIN ; Loop over quartiles
      FOR m=0,3 DO BEGIN ; Loop over precip trends (0=all, 1=down, 2=steady/not significant, 3=up) 
         FOR k=0,n_lead_times-1 DO BEGIN
            FOR q=0,n_times_per_day-1 DO BEGIN
                                ; this_pt_olr=REFORM(model_olr(n,p,q:n_days*n_times_per_day-1:n_times_per_day,k))
               this_pt_pr=REFORM(model_precip(*,*,q:n_days*n_times_per_day-1:n_times_per_day,k))
               this_pt_trend=REFORM(model_precip_localtrend(*,*,q:n_days*n_times_per_day-1:n_times_per_day,k))
               CASE j OF
                  0 : BEGIN
                     pr_points=where(this_pt_pr lt model_precip_quartiles(0,q,k) and this_pt_pr ge 1)
                  END
                  1 : BEGIN
                     pr_points=where(this_pt_pr lt model_precip_quartiles(1,q,k) and this_pt_pr ge model_precip_quartiles(0,q,k))
                  END
                  2 : BEGIN
                     pr_points=where(this_pt_pr lt model_precip_quartiles(2,q,k) and this_pt_pr ge model_precip_quartiles(1,q,k))
                  END
                  3 : BEGIN
                     pr_points=where(this_pt_pr ge model_precip_quartiles(2,q,k))
                  END
                  4 : BEGIN
                     pr_points=where(this_pt_pr lt 1)
                  END
               ENDCASE
               IF TOTAL(pr_points) ge 0 THEN BEGIN
                  CASE m OF
                     0 : BEGIN
                        tr_points=where(model_precip_localtrend[pr_points] ge -9999) ; Select all pr_points
                     END
                     1 : BEGIN
                        tr_points=where(model_precip_localtrend[pr_points] eq -1)
                     END
                     2 : BEGIN
                        tr_points=where(model_precip_localtrend[pr_points] eq 0)
                     END
                     3 : BEGIN
                        tr_points=where(model_precip_localtrend[pr_points] eq 1)
                     END
                  ENDCASE
               
                  FOR r=0,model_nz-1 DO BEGIN
                     this_pt_inc=REFORM(this_increment(*,*,r,q:n_days*n_times_per_day-1:n_times_per_day,k))                 
                     this_pt_inc=this_pt_inc[pr_points]
                     IF TOTAL(tr_points) ne -1 THEN BEGIN
                        this_pt_inc=this_pt_inc[tr_points]
                        IF FINITE(TOTAL(this_pt_inc,/NaN)) eq 1 THEN $
                           model_increments(i,j,m,r)=model_increments(i,j,m,r)+TOTAL(this_pt_inc,/NaN)
                     ENDIF
                  ENDFOR
                  IF TOTAL(tr_points) ne -1 THEN BEGIN
                     total_points(j,m)=total_points(j,m)+N_ELEMENTS(tr_points)
                     temp_pr=this_pt_pr[pr_points]
                     total_precip(i,j,m)=total_precip(i,j,m)+TOTAL(temp_pr[tr_points],/NaN)
                  ENDIF
               ENDIF ;ELSE $
;                  model_increments(i,j,m,*)=!Values.F_NaN
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
   print,'Computed composite increments'

   FOR r=0,model_nz-1 DO $
      model_increments(i,*,*,r)=model_increments(i,*,*,r)/FLOAT(total_points(*,*))
   total_precip(i,*,*)=total_precip(i,*,*)/FLOAT(total_points(*,*))

;   frac_nearzero_points(i)=total_nearzero_points/FLOAT(model_nlon*model_nlat*n_days*n_times_per_day)
ENDFOR

colors=['red','orange','cyan','blue','black']
styles=[0,1,2,3]
syms=[3,4,5,6]
FOR m=0,n_models-1 DO BEGIN
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manyquartiles_10x10.'+$
          file_descs(m)+'.'+inc_type+'.'+region_name+'.lead'+STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+'.ps'      
   PSOPEN,file=psfile,FONT=2,CHARSIZE=180,MARGIN=3000,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=MAX(model_z)+25,YMAX=MIN(model_z) ;,TITLE=file_descs(m)+' '+inc_type+' dq/dt '+region_name+$
                                ;' at '+STRTRIM(STRING(MIN(our_lead_times)),1)+'-'+STRTRIM(STRING(MAX(our_lead_times)),1)+' day(s) - '+start_date+' to '+stop_date     
   FOR j=0,4 DO BEGIN
      FOR k=0,0 DO BEGIN
         IF reverse_levels(m) eq 1 THEN BEGIN
            GPLOT,X=REVERSE(REFORM(model_increments(m,j,k,*))),Y=model_z,COL=FSC_COLOR(colors(j)),STYLE=styles(k),THICK=200
         ENDIF ELSE $
            GPLOT,X=REFORM(model_increments(m,j,k,*)),Y=model_z,COL=FSC_COLOR(colors(j)),STYLE=styles(k),THICK=200
      ENDFOR
   ENDFOR   
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+20],COL=FSC_COLOR('black'),STYLE=1;,THICK=150
   GLEGEND,labels=REVERSE(['4th Quartile','3rd Quartile','2nd Quartile','1st Quartile','< 1 mm day!U-1!N']),$
           COL=REVERSE([FSC_COLOR(['blue','cyan','orange','red','black'])]),LEGXOFFSET=legxoffset,LEGYOFFSET=legyoffset
;   GLEGEND,labels=REVERSE(['All','Increasing','Steady/not sig','Decreasing']),$
;           STYLE=REVERSE([0,1,2,3]),LEGPOS=9,SYM=REVERSE(syms)
   AXES,XSTEP=xstep(0),XMINOR=xstep(0)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=1,/NORIGHT,/NOUPPER,ORIENTATION=20   
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=0,YMAX=20
   FOR j=0,3 DO $
      FOR k=0,0 DO $
         GPLOT,X=xmax(0),Y=total_precip(m,j,k),COL=FSC_COLOR(colors(j)),SYM=syms(k),SIZE=200
   AXES,YSTEP=1,YTITLE='Mean rainfall in bin (mm day!U-1!N)',/ONLYRIGHT,NDECS=1
   PSCLOSE,/NOVIEW
ENDFOR


STOP
END
