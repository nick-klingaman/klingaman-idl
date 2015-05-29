PRO mjo_diabatic_tendencies_composite_precip_q_manymodels,model_names=model_names,inc_type,start_date,stop_date,box,region_name,lead_times=lead_times

; Composite specific humidity tendencies by suppressed/transition/active MJO
; using quartiles of precipitation as delimiter.  Plot many models on a single graph.

; Needs type of increment to plot ('physics','radiation','total') and box within which to composite
; (e.g., W or E eqIO)

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['ecmwf','mri','miroc','nasa','nrl','spcam','nicam','giss','cancm4','metum']

n_models=N_ELEMENTS(our_model_names)
IF KEYWORD_SET(lead_times) THEN BEGIN
   our_lead_times=lead_times
ENDIF ELSE $
   our_lead_times=indgen(20)
n_lead_times=N_ELEMENTS(our_lead_times)
n_times_per_day=8

standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
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
model_increments_active=fltarr(n_models,model_nz,n_lead_times)
model_increments_suppressed=fltarr(n_models,model_nz,n_lead_times)
model_increments_transactive=fltarr(n_models,model_nz,n_lead_times)
model_increments_transsuppressed=fltarr(n_models,model_nz,n_lead_times)
model_increments_nearzero=fltarr(n_models,model_nz,n_lead_times)
model_colors=strarr(n_models)
model_names=strarr(n_models)
reverse_levels=intarr(n_models)

total_active_precip=fltarr(n_models,n_lead_times)
total_suppressed_precip=fltarr(n_models,n_lead_times)
total_transactive_precip=fltarr(n_models,n_lead_times)
total_transsuppressed_precip=fltarr(n_models,n_lead_times)
frac_nearzero_points=fltarr(n_models,n_lead_times)

FOR i=0,n_models-1 DO BEGIN
   print,our_model_names(i)
   CASE our_model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         file_desc='ECMWF_IFS'
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
         file_desc='MRI-AGCM'
         model_names(i)='MR'
         model_colors(i)='blue'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e10
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_desc='miroc5'
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
         file_desc='NGEM01'
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
         file_desc='GEOS5_AGCM'
         model_names(i)='NA'
         model_colors(i)='cyan'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'        
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_desc='ModelE'
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
         file_desc='CanCM4'
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
         file_desc='SPCAM3.0'
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
         file_desc='MetUM'
         model_names(i)='UM'
         model_colors(i)='steelblue'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'        
         reverse_levels(i)=0
         multiplier=72.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         file_desc='nicam'
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
         xmin=[-12,-4,-4,-4,-4]
         xmax=[6,4,6,6,6]
         xstep=[2,0.5,0.6,0.5,0.5]
      END
      'advection' : BEGIN
         increment_name='tnhusa'
         xmin=[-9,-4,-8,-6,-6]
         xmax=[13,3,4,4,4]
         xstep=[2,0.5,0.8,0.6,0.6] 
      END
      'all' : BEGIN
         increment_name='tnhus'
         xmin=[-3,-2,-2,-1,-1]
         xmax=[3,2,2,1,1]
         xstep=[0.5,0.4,0.4,0.2,0.2]
      END      
   ENDCASE
                                ; Read all available precipitation data for region to compute quartiles
                                ; and classify into suppressed/transition/active
   grid_flag=0
   start_position=REFORM(where(standard_valid_dates eq start_date))
   date_offset=where(dates eq start_date)
   FOR j=0,n_days-1 DO BEGIN
      print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))            
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            model_pr_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
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
   
   model_precip_quartiles=fltarr(3,n_times_per_day,n_lead_times)
   model_precip_quartiles_npts=fltarr(n_times_per_day,n_lead_times)
   FOR m=0,n_times_per_day-1 DO BEGIN
      FOR n=0,n_lead_times-1 DO BEGIN
         temp_precip=REFORM(model_precip(*,*,m:n_days*n_times_per_day-1:n_times_per_day,n),[model_nlon*model_nlat*n_days])
         temp_precip=temp_precip[where(temp_precip ge 0.2)]
         indices=SORT(temp_precip)         
         FOR p=1,3 DO $
            model_precip_quartiles(p-1,m,n)=temp_precip(indices(N_ELEMENTS(temp_precip)*p/4.))
         model_precip_quartiles_npts(m,n)=N_ELEMENTS(temp_precip)/4.
      ENDFOR
   ENDFOR

   grid_flag=0
   FOR j=0,n_days-1 DO BEGIN
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
            model_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.'+increment_name+'.'+REFORM(initial_date)+'.00Z.nc'
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
   IF TOTAL(where(ABS(this_increment) ge missing_value)) ge 0 THEN $
      this_increment[where(ABS(this_increment) ge missing_value)]=!Values.F_NaN
   
   total_active_points=fltarr(n_lead_times)
   total_suppressed_points=fltarr(n_lead_times)
   total_transactive_points=fltarr(n_lead_times)
   total_transsuppressed_points=fltarr(n_lead_times)
   total_nearzero_points=fltarr(n_lead_times)
   FOR k=0,n_lead_times-1 DO BEGIN
      FOR q=0,n_times_per_day-1 DO BEGIN
                                ; this_pt_olr=REFORM(model_olr(n,p,q:n_days*n_times_per_day-1:n_times_per_day,k))
         this_pt_pr=REFORM(model_precip(*,*,q:n_days*n_times_per_day-1:n_times_per_day,k))
         active_points=where(this_pt_pr ge model_precip_quartiles(2,q,k))
         suppressed_points=where(this_pt_pr lt model_precip_quartiles(0,q,k) and this_pt_pr ge 0.2)
         transition_active_points=where(this_pt_pr lt model_precip_quartiles(2,q,k) and $
                                        this_pt_pr ge model_precip_quartiles(1,q,k))
         transition_suppressed_points=where(this_pt_pr lt model_precip_quartiles(1,q,k) and $
                                            this_pt_pr ge model_precip_quartiles(0,q,k))
         nearzero_points=where(this_pt_pr lt 0.2)
         FOR r=0,model_nz-1 DO BEGIN
            this_pt_inc=REFORM(this_increment(*,*,r,q:n_days*n_times_per_day-1:n_times_per_day,k))
            IF TOTAL(active_points) ne -1 THEN $ 
               model_increments_active(i,r,k)=model_increments_active(i,r,k)+TOTAL(this_pt_inc[active_points],/NaN) 
            IF TOTAL(suppressed_points) ne -1 THEN $
               model_increments_suppressed(i,r,k)=model_increments_suppressed(i,r,k)+TOTAL(this_pt_inc[suppressed_points],/NaN) 
            IF TOTAL(transition_active_points) ne -1 THEN $
               model_increments_transactive(i,r,k)=model_increments_transactive(i,r,k)+$
                                                 TOTAL(this_pt_inc[transition_active_points],/NaN)
            IF TOTAL(transition_suppressed_points) ne -1 THEN $
               model_increments_transsuppressed(i,r,k)=model_increments_transsuppressed(i,r,k)+$
                                                     TOTAL(this_pt_inc[transition_suppressed_points],/NaN)
            IF TOTAL(nearzero_points) ne -1 THEN $
               model_increments_nearzero(i,r,k)=model_increments_nearzero(i,r,k)+$
                                                TOTAL(this_pt_inc[nearzero_points],/NaN)
         ENDFOR
         IF TOTAL(active_points) ne -1 THEN BEGIN
            total_active_points(k)=total_active_points(k)+N_ELEMENTS(active_points)
            total_active_precip(i,k)=total_active_precip(i,k)+TOTAL(this_pt_pr[active_points])
         ENDIF
         IF TOTAL(suppressed_points) ne -1 THEN BEGIN
            total_suppressed_points(k)=total_suppressed_points(k)+N_ELEMENTS(suppressed_points)
            total_suppressed_precip(i,k)=total_suppressed_precip(i,k)+TOTAL(this_pt_pr[suppressed_points])
         ENDIF
         IF TOTAL(transition_active_points) ne -1 THEN BEGIN
            total_transactive_points(k)=total_transactive_points(k)+N_ELEMENTS(transition_active_points)
            total_transactive_precip(i,k)=total_transactive_precip(i,k)+TOTAL(this_pt_pr[transition_active_points])
         ENDIF
         IF TOTAL(transition_suppressed_points) ne -1 THEN BEGIN
            total_transsuppressed_points(k)=total_transsuppressed_points(k)+N_ELEMENTS(transition_suppressed_points)
            total_transsuppressed_precip(i,k)=total_transsuppressed_precip(i,k)+TOTAL(this_pt_pr[transition_suppressed_points])
         ENDIF
         IF TOTAL(nearzero_points) ne -1 THEN $
            total_nearzero_points(k)=total_nearzero_points(k)+N_ELEMENTS(nearzero_points)
      ENDFOR
      model_increments_active(i,*,k)=model_increments_active(i,*,k)/FLOAT(total_active_points(k))      
      total_active_precip(i,k)=total_active_precip(i,k)/FLOAT(total_active_points(k))
      model_increments_suppressed(i,*,k)=model_increments_suppressed(i,*,k)/FLOAT(total_suppressed_points(k))
      total_suppressed_precip(i,k)=total_suppressed_precip(i,k)/FLOAT(total_suppressed_points(k))
      model_increments_transactive(i,*,k)=model_increments_transactive(i,*,k)/FLOAT(total_transactive_points(k))
      total_transactive_precip(i,k)=total_transactive_precip(i,k)/FLOAT(total_transactive_points(k))
      model_increments_transsuppressed(i,*,k)=model_increments_transsuppressed(i,*,k)/FLOAT(total_transsuppressed_points(k))
      total_transsuppressed_precip(i,k)=total_transsuppressed_precip(i,k)/FLOAT(total_transsuppressed_points(k))
      model_increments_nearzero(i,*,k)=model_increments_nearzero(i,*,k)/FLOAT(total_nearzero_points(k))
      frac_nearzero_points(i,k)=total_nearzero_points(k)/FLOAT(model_nlon*model_nlat*n_days*n_times_per_day)
   ENDFOR
ENDFOR

FOR k=0,n_lead_times-1 DO BEGIN      
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manymodels.by_quartile.'+$
          inc_type+'.'+region_name+'_active.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=inc_type+' dq/dt '+region_name+$
        ' active (precip 4th quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
   FOR m=0,n_models-1 DO BEGIN
      IF reverse_levels(m) eq 1 THEN BEGIN
         GPLOT,X=REVERSE(REFORM(model_increments_active(m,*,k))),Y=model_z,COL=FSC_COLOR(model_colors(m))
      ENDIF ELSE $
         GPLOT,X=REFORM(model_increments_active(m,*,k)),Y=model_z,COL=FSC_COLOR(model_colors(m))
   ENDFOR
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+20],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=25000,length=25
   AXES,XSTEP=xstep(0),XMINOR=xstep(0)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,/NORIGHT,/NOUPPER,ORIENTATION=30   
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=20,YMAX=60
   FOR m=0,n_models-1 DO $
      GPLOT,X=xmax(0),Y=REFORM(total_active_precip(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
   AXES,YSTEP=3,YTITLE='Mean precip in bin',/ONLYRIGHT,NDECS=1
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manymodels.by_quartile.'+$
          inc_type+'.'+region_name+'_suppressed.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(1),XMAX=xmax(1),YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=inc_type+' dq/dt '+region_name+$
        ' suppress (precip 1st quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
   FOR m=0,n_models-1 DO BEGIN
      IF reverse_levels(m) eq 1 THEN BEGIN
         GPLOT,X=REVERSE(REFORM(model_increments_suppressed(m,*,k))),Y=model_z,COL=FSC_COLOR(model_colors(m))
      ENDIF ELSE $
         GPLOT,X=REFORM(model_increments_suppressed(m,*,k)),Y=model_z,COL=FSC_COLOR(model_colors(m))
   ENDFOR
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=25000,length=25
   AXES,XSTEP=xstep(1),XMINOR=xstep(1)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,/NORIGHT,/NOUPPER,ORIENTATION=30   
   GSET,XMIN=xmin(1),XMAX=xmax(1),YMIN=0,YMAX=4
   FOR m=0,n_models-1 DO $
      GPLOT,X=xmax(1),Y=REFORM(total_suppressed_precip(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
   AXES,YSTEP=0.4,YTITLE='Mean precip in bin',/ONLYRIGHT,NDECS=1
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manymodels.by_quartile.'+$
          inc_type+'.'+region_name+'_transactive.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(2),XMAX=XMAX(2),YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=inc_type+' dq/dt '+region_name+$
        ' transact (precip 3rd quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
   FOR m=0,n_models-1 DO BEGIN
      IF reverse_levels(m) eq 1 THEN BEGIN
         GPLOT,X=REVERSE(REFORM(model_increments_transactive(m,*,k))),Y=model_z,COL=FSC_COLOR(model_colors(m))
      ENDIF ELSE $
         GPLOT,X=REFORM(model_increments_transactive(m,*,k)),Y=model_z,COL=FSC_COLOR(model_colors(m))
   ENDFOR
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=25000,length=25
   AXES,XSTEP=xstep(2),XMINOR=xstep(2)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,/NORIGHT,/NOUPPER,ORIENTATION=30
   GSET,XMIN=xmin(2),XMAX=XMAX(2),YMIN=5,YMAX=20
   FOR m=0,n_models-1 DO $
      GPLOT,X=xmax(2),Y=REFORM(total_transactive_precip(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
   AXES,YSTEP=1.5,YTITLE='Mean precip in bin',/ONLYRIGHT,NDECS=1
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manymodels.by_quartile.'+$
          inc_type+'.'+region_name+'_transsuppressed.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(3),XMAX=xmax(3),YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=inc_type+' dq/dt '+region_name+$
        ' transsup (precip 2nd quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
   FOR m=0,n_models-1 DO BEGIN
      IF reverse_levels(m) eq 1 THEN BEGIN
         GPLOT,X=REVERSE(REFORM(model_increments_transsuppressed(m,*,k))),Y=model_z,COL=FSC_COLOR(model_colors(m))
      ENDIF ELSE $
         GPLOT,X=REFORM(model_increments_transsuppressed(m,*,k)),Y=model_z,COL=FSC_COLOR(model_colors(m))
   ENDFOR
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=25000,length=25
   AXES,XSTEP=xstep(3),XMINOR=xstep(3)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,/NORIGHT,/NOUPPER,ORIENTATION=30
   GSET,XMIN=xmin(3),XMAX=xmax(3),YMIN=0,YMAX=10
   FOR m=0,n_models-1 DO $
      GPLOT,X=xmax(3),Y=REFORM(total_transsuppressed_precip(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
   AXES,YSTEP=1,YTITLE='Mean precip in bin',/ONLYRIGHT,NDECS=1
   PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q_manymodels.by_quartile.'+$
          inc_type+'.'+region_name+'_nearzero.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=200,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=90,$
          CB_WIDTH=110,SPACE2=2000,/PORTRAIT
   GSET,XMIN=xmin(4),XMAX=xmax(4),YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=inc_type+' dq/dt '+region_name+$
        ' near-zero (precip < 0.2 mm/day) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
   FOR m=0,n_models-1 DO BEGIN
      IF reverse_levels(m) eq 1 THEN BEGIN
         GPLOT,X=REVERSE(REFORM(model_increments_nearzero(m,*,k))),Y=model_z,COL=FSC_COLOR(model_colors(m))
      ENDIF ELSE $
         GPLOT,X=REFORM(model_increments_nearzero(m,*,k)),Y=model_z,COL=FSC_COLOR(model_colors(m))
   ENDFOR
   GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(model_colors)),LEGXOFFSET=4500,LEGYOFFSET=25000,length=25
   AXES,XSTEP=xstep(4),XMINOR=xstep(4)/2.,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',$
        XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,/NORIGHT,/NOUPPER,ORIENTATION=30
   GSET,XMIN=xmin(4),XMAX=xmax(4),YMIN=0,YMAX=0.5
   FOR m=0,n_models-1 DO $
      GPLOT,X=xmax(4),Y=REFORM(frac_nearzero_points(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
   AXES,YSTEP=0.05,YMINOR=0.025,/ONLYRIGHT,NDECS=1,YTITLE='Fraction of points with precip < 0.2 mm/day'

;   GSET,XMIN=xmin(4),XMAX=xmax(4),YMIN=0,YMAX=10
;   FOR m=0,n_models-1 DO $
;      GPLOT,X=xmax(4),Y=REFORM(total_nearzero_precip(m,k)),SYM=3,COL=FSC_COLOR(model_colors(m))
;   AXES,YSTEP=1,YTITLE='Mean precip in bin',/ONLYRIGHT,NDECS=1
   PSCLOSE

ENDFOR


STOP
END
