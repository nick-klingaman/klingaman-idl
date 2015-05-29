PRO mjo_diabatic_tendencies_composite_precip_q,model_names,start_date,stop_date,box,region_name,lead_times=lead_times

; Composite temperature tendencies by suppressed/transition/active MJO
; using standard deviation of OLR as delimiter

; Needs names of models to use and box within which to composite
; (e.g., W or E eqIO)

n_models=N_ELEMENTS(model_names)
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

FOR i=0,n_models-1 DO BEGIN
   print,model_names(i)
   CASE model_names(i) OF
      'ecmwf' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
         file_desc='ECMWF_IFS'
         plot_desc='ECMWF_IFS'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         increment_names=['tnhus','tnhusa','tnhusc','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e10
      END
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/mri'
         file_desc='MRI-AGCM'
         plot_desc='MRI-AGCM'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         increment_names=['tnhus','tnhusa','tnhusc','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e10
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
         file_desc='miroc5'
         plot_desc='MIROC5'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'         
         increment_names=['tnhus','tnhusa','tnhusc','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1000
      END
      'nrl' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
         file_desc='NGEM01'
         plot_desc='NRL_NavGEM01'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         increment_names=['tnhus','tnhusa','tnhusc','tnhusd','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','Other','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','dodgerblue','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=100000
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
         file_desc='GEOS5_AGCM'
         plot_desc='NASA_GEOS5'
         valid_dates=standard_valid_dates
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'         
         increment_names=['tnhus','tnhusa','tnhusc','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/giss'
         file_desc='ModelE'
         plot_desc='GISS_ModelE2'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         increment_names=['tnhus','tnhusa','tnhusc','tnhusd','tnhuslscp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','Other','LS cld+prcp','B-layer']
         increment_colors=['black','purple','red','dodgerblue','maroon','blue']
         multiplier=86400.*1000.
         precip_multiplier=1.
         level_multiplier=1.
         missing_value=1e20
      END
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
         file_desc='CanCM4'
         plot_desc='CCCma_CanCM4'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         increment_names=['tnhus','tnhusa','tnhusc','tnhusd','tnhusscpbl']
         increment_descs=['Total','Advection','Convection','Other','LS cld+prcp + Blyr']
         increment_colors=['black','purple','red','dodgerblue','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e20
      END
      'spcam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
         file_desc='SPCAM3.0'
         plot_desc='SPCAM3.0'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='lev_p'
         increment_names=['tnhus','tnhusa','tnhusp','tnhuspbl']
         increment_descs=['Total','Advection','CRM','vert_diff']
         increment_colors=['black','purple','red','blue']
         multiplier=86400.*1000
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
      END
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/metum'
         file_desc='MetUM'
         plot_desc='MetUM_GA3.0'
         valid_dates=standard_valid_dates
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'         
         increment_names=['tnhus','tnhusa','tnhusc','tnhusd','tnhuslsp','tnhuspbl']
         increment_descs=['Total','Advection','Convection','Other','LS prcp','B-layer + lscld']
         increment_colors=['black','purple','red','dodgerblue','maroon','blue']
         multiplier=72.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END
      'nicam' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20day/nicam'
         file_desc='nicam'
         plot_desc='NICAM'
         valid_dates=['20091015','20091020','20091025','20091030','20091104','20091109',$
                      '20091215','20091220','20091225','20091230','20100104','20100109']
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         increment_names=['tnhus','tnhusa','tnhuscmp','tnhuspbl']
         increment_descs=['Total','Advection','Microphys','B-layer']
         increment_colors=['black','purple','red','blue']
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e6
      END     
   ENDCASE
   n_increments=N_ELEMENTS(increment_names)

                                ; Read all available OLR data for region to compute standard deviation 
                                ; and classify into suppressed/transition/active
   grid_flag=0
   start_position=REFORM(where(standard_valid_dates eq start_date))
   date_offset=where(dates eq start_date)
   FOR j=0,n_days-1 DO BEGIN
      print,TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0))))            
      IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
         FOR k=0,n_lead_times-1 DO BEGIN    
            initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
;            print,'Examining day '+dates(j)+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)+$
;                  ' using hindcast initialised on '+initial_date
;            model_rlut_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.rlut.'+REFORM(initial_date)+'.00Z.nc'
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
;            model_olr(*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
;               OPEN_AND_EXTRACT(model_rlut_infile(0),'rlut',$
;                                offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*n_times_per_day],$
;                                count=[model_nlon,model_nlat,n_times_per_day])
            model_precip(*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
               OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                offset=[model_box_tx(1),model_box_tx(0),our_lead_times(k)*n_times_per_day],$
                                count=[model_nlon,model_nlat,n_times_per_day])*precip_multiplier
         ENDFOR
      ENDIF
   ENDFOR
   
;   IF TOTAL(where(model_olr eq 0)) ge 0 THEN $
;      model_olr[where(model_olr eq 0)]=!Values.F_NaN
;   model_olr_mean=fltarr(model_nlon,model_nlat,n_times_per_day,n_lead_times)
;   model_olr_stddev=fltarr(model_nlon,model_nlat,n_times_per_day,n_lead_times)
;   FOR j=0,model_nlon-1 DO BEGIN
;      FOR k=0,model_nlat-1 DO BEGIN
;          FOR m=0,n_times_per_day-1 DO BEGIN
;            FOR n=0,n_lead_times-1 DO BEGIN
;               model_olr_mean(j,k,m,n)=MEAN(model_olr(j,k,m:n_days*n_times_per_day-1:n_times_per_day,n),/NaN)
;               model_olr_stddev(j,k,m,n)=STDDEV(model_olr(j,k,m:n_days*n_times_per_day-1:n_times_per_day,n),/NaN)
;            ENDFOR
;         ENDFOR
;      ENDFOR
;   ENDFOR

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
   
   array_flag=0
   FOR m=0,n_increments-1 DO BEGIN
      grid_flag=0
      FOR j=0,n_days-1 DO BEGIN
         IF TOTAL(where(valid_dates eq standard_valid_dates(j+start_position(0)))) ne -1 THEN BEGIN     
            FOR k=0,n_lead_times-1 DO BEGIN    
               initial_date=valid_dates(where(valid_dates eq standard_valid_dates(j+start_position(0))))
               model_infile=indir+'/'+REFORM(initial_date)+'/'+file_desc+'.'+increment_names(m)+'.'+REFORM(initial_date)+'.00Z.nc'
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
               IF array_flag eq 0 and grid_flag eq 1 THEN BEGIN
                  model_increments_active=fltarr(n_increments,model_nz,n_lead_times)
                  model_increments_suppressed=fltarr(n_increments,model_nz,n_lead_times)
                  model_increments_transactive=fltarr(n_increments,model_nz,n_lead_times)
                  model_increments_transsuppressed=fltarr(n_increments,model_nz,n_lead_times)
                  array_flag=1
               ENDIF
               this_increment(*,*,*,j*n_times_per_day:(j+1)*n_times_per_day-1,k)=$
                  OPEN_AND_EXTRACT(model_infile(0),increment_names(m),$
                                   offset=[model_inc_box_tx(1),model_inc_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
                                   count=[model_inc_nlon,model_inc_nlat,model_nz,n_times_per_day])*multiplier
            ENDFOR
         ENDIF
      ENDFOR
      IF TOTAL(where(ABS(this_increment) ge missing_value)) ge 0 THEN $
         this_increment[where(ABS(this_increment) ge missing_value)]=!Values.F_NaN

      total_active_precip=fltarr(n_lead_times)
      total_suppressed_precip=fltarr(n_lead_times)
      total_transactive_precip=fltarr(n_lead_times)
      total_transsuppressed_precip=fltarr(n_lead_times)
      FOR k=0,n_lead_times-1 DO BEGIN
;         FOR n=0,model_nlon-1 DO BEGIN
;            FOR p=0,model_nlat-1 DO BEGIN
         FOR q=0,n_times_per_day-1 DO BEGIN
                                ; this_pt_olr=REFORM(model_olr(n,p,q:n_days*n_times_per_day-1:n_times_per_day,k))
            this_pt_pr=REFORM(model_precip(*,*,q:n_days*n_times_per_day-1:n_times_per_day,k))
            active_points=where(this_pt_pr ge model_precip_quartiles(2,q,k))
            suppressed_points=where(this_pt_pr lt model_precip_quartiles(0,q,k) and this_pt_pr ge 0.2)
            transition_active_points=where(this_pt_pr lt model_precip_quartiles(2,q,k) and $
                                           this_pt_pr ge model_precip_quartiles(1,q,k))
            transition_suppressed_points=where(this_pt_pr lt model_precip_quartiles(1,q,k) and $
                                               this_pt_pr ge model_precip_quartiles(0,q,k))
            FOR r=0,model_nz-1 DO BEGIN
               this_pt_inc=REFORM(this_increment(*,*,r,q:n_days*n_times_per_day-1:n_times_per_day,k))
               IF TOTAL(active_points) ne -1 THEN $ 
                  model_increments_active(m,r,k)=model_increments_active(m,r,k)+TOTAL(this_pt_inc[active_points],/NaN) 
               IF TOTAL(suppressed_points) ne -1 THEN $
                  model_increments_suppressed(m,r,k)=model_increments_suppressed(m,r,k)+TOTAL(this_pt_inc[suppressed_points],/NaN) 
               IF TOTAL(transition_active_points) ne -1 THEN $
                  model_increments_transactive(m,r,k)=model_increments_transactive(m,r,k)+$
                                                      TOTAL(this_pt_inc[transition_active_points],/NaN)
               IF TOTAL(transition_suppressed_points) ne -1 THEN $
                  model_increments_transsuppressed(m,r,k)=model_increments_transsuppressed(m,r,k)+$
                  TOTAL(this_pt_inc[transition_suppressed_points],/NaN)
            ENDFOR
            IF TOTAL(active_points) ne -1 THEN $
               total_active_precip(k)=total_active_precip(k)+TOTAL(this_pt_pr[active_points])
            IF TOTAL(suppressed_points) ne -1 THEN $
               total_suppressed_precip(k)=total_suppressed_precip(k)+TOTAL(this_pt_pr[suppressed_points])
            IF TOTAL(transition_active_points) ne -1 THEN $
               total_transactive_precip(k)=total_transactive_precip(k)+TOTAL(this_pt_pr[transition_active_points])
            IF TOTAL(transition_suppressed_points) ne -1 THEN $
               total_transsuppressed_precip(k)=total_transsuppressed_precip(k)+TOTAL(this_pt_pr[transition_suppressed_points])         
         ENDFOR
;         print,'Total active/suppressed/transactive/transsuppressed points: '+STRTRIM(STRING(total_active_points),1)+'/'+STRTRIM(STRING(total_suppressed_points),1)+$
;               '/'+STRTRIM(STRING(total_transition_active_points),1)+'/'+STRTRIM(STRING(total_transition_suppressed_points),1)
;         print,total_active_precip         
         model_increments_active(m,*,k)=model_increments_active(m,*,k)/FLOAT(total_active_precip(k))
         model_increments_suppressed(m,*,k)=model_increments_suppressed(m,*,k)/FLOAT(total_suppressed_precip(k))
         model_increments_transactive(m,*,k)=model_increments_transactive(m,*,k)/FLOAT(total_transactive_precip(k))
         model_increments_transsuppressed(m,*,k)=model_increments_transsuppressed(m,*,k)/FLOAT(total_transsuppressed_precip(k))
      ENDFOR      
   ENDFOR
;   print,total_active_precip,total_transactive_precip(k),total_transsuppressed_precip(k),total_suppressed_precip(k)
   FOR k=0,n_lead_times-1 DO BEGIN      
      psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q.by_quartile.'+$
             plot_desc+'.'+region_name+'_active.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=200,XOFFSET=2000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
             CB_WIDTH=110,SPACE2=2000,/PORTRAIT
      GSET,XMIN=-0.5,XMAX=0.5,YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=plot_desc+' dq/dt '+region_name+$
           ' active (precip 4th quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
      FOR m=0,n_increments-1 DO $
         GPLOT,X=REFORM(model_increments_active(m,*,k)),Y=model_z,COL=FSC_COLOR(increment_colors(m))
      GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
      GPLOT,X=-0.45,Y=40,TEXT='Mean precip in bin: '+$
            STRMID(STRTRIM(STRING(total_active_precip(k)/TOTAL(model_precip_quartiles_npts(*,k))),1),0,4)+' mm day!U-1!N',ALIGN=0.0
      GLEGEND,labels=REVERSE(increment_descs),COL=REVERSE(FSC_COLOR(increment_colors)),LEGXOFFSET=2000,LEGYOFFSET=26000
      AXES,XSTEP=0.05,XMINOR=0.025,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',XTITLE='Moistening rate (g kg!U-1!N day!U-1!N)',NDECS=2,$
           /NORIGHT,/NOUPPER,ORIENTATION=30
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q.by_quartile.'+$
             plot_desc+'.'+region_name+'_suppressed.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=200,XOFFSET=2000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
             CB_WIDTH=110,SPACE2=2000,/PORTRAIT
      GSET,XMIN=-15,XMAX=15,YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=plot_desc+' dq/dt '+region_name+$
           ' suppress (precip 1st quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
      FOR m=0,n_increments-1 DO $
         GPLOT,X=REFORM(model_increments_suppressed(m,*,k)),Y=model_z,COL=FSC_COLOR(increment_colors(m))
      GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
      GPLOT,X=-13.5,Y=40,TEXT='Mean precip in bin: '+$
            STRMID(STRTRIM(STRING(total_suppressed_precip(k)/(FLOAT(model_nlon*model_nlat*n_days/4.*n_times_per_day))),1),0,4)+' mm day!U-1!N',ALIGN=0.0
      GLEGEND,labels=REVERSE(increment_descs),COL=REVERSE(FSC_COLOR(increment_colors)),LEGXOFFSET=2000,LEGYOFFSET=26000
      AXES,XSTEP=1.5,XMINOR=0.75,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',XTITLE='Heating rate (g kg!U-1!N mm!U-1!N)',NDECS=1,$
           /NORIGHT,/NOUPPER,ORIENTATION=30
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q.by_quartile.'+$
             plot_desc+'.'+region_name+'_transactive.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=200,XOFFSET=2000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
             CB_WIDTH=110,SPACE2=2000,/PORTRAIT
      GSET,XMIN=-1.5,XMAX=1.5,YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=plot_desc+' dq/dt '+region_name+$
           ' transact (precip 3rd quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
      FOR m=0,n_increments-1 DO $
         GPLOT,X=REFORM(model_increments_transactive(m,*,k)),Y=model_z,COL=FSC_COLOR(increment_colors(m))
      GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
      GPLOT,X=-1.8,Y=40,TEXT='Mean precip in bin: '+$
            STRMID(STRTRIM(STRING(total_transactive_precip(k)/(FLOAT(model_nlon*model_nlat*n_days/4.*n_times_per_day))),1),0,4)+' mm day!U-1!N',ALIGN=0.0
      GLEGEND,labels=REVERSE(increment_descs),COL=REVERSE(FSC_COLOR(increment_colors)),LEGXOFFSET=2000,LEGYOFFSET=26000
      AXES,XSTEP=0.15,XMINOR=0.075,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',XTITLE='Heating rate (g kg!U-1!N mm!U-1!N)',NDECS=2,$
           /NORIGHT,/NOUPPER,ORIENTATION=30
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_tendencies_composite_precip_q.by_quartile.'+$
             plot_desc+'.'+region_name+'_transsuppressed.lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=200,XOFFSET=2000,YOFFSET=2000,TFONT=2,TCHARSIZE=90,$
             CB_WIDTH=110,SPACE2=2000,/PORTRAIT
      GSET,XMIN=-5,XMAX=5,YMIN=MAX(model_z)+25,YMAX=MIN(model_z),TITLE=plot_desc+' dq/dt '+region_name+$
           ' transsup (precip 2nd quart) at '+STRTRIM(STRING(our_lead_times(k)),1)+' day(s) - '+start_date+' to '+stop_date
      FOR m=0,n_increments-1 DO $
         GPLOT,X=REFORM(model_increments_transsuppressed(m,*,k)),Y=model_z,COL=FSC_COLOR(increment_colors(m))      
      GPLOT,X=[0,0],Y=[MIN(model_z),MAX(model_z)+25],COL=FSC_COLOR('black'),STYLE=1
      GPLOT,X=-4.5,Y=40,TEXT='Mean precip in bin: '+$
            STRMID(STRTRIM(STRING(total_transsuppressed_precip(k)/(FLOAT(model_nlon*model_nlat*n_days/4.*n_times_per_day))),1),0,4)+' mm day!U-1!N',ALIGN=0.0
      GLEGEND,labels=REVERSE(increment_descs),COL=REVERSE(FSC_COLOR(increment_colors)),LEGXOFFSET=2000,LEGYOFFSET=26000
      AXES,XSTEP=0.5,XMINOR=0.25,YVALS=FLOOR(model_z+0.01),YTITLE='Pressure (hPa)',XTITLE='Heating rate (g kg!U-1!N mm!U-1!N)',NDECS=1,$
           /NORIGHT,/NOUPPER,ORIENTATION=30
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR


STOP
END
