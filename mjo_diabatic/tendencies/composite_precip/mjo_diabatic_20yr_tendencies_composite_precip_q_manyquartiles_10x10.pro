PRO mjo_diabatic_20yr_tendencies_composite_precip_q_manyquartiles_10x10,model_names=model_names,inc_type,box,region_name

; Composite specific humidity tendencies by suppressed/transition/active MJO
; using quartiles of precipitation as delimiter.  Plot all quartiles on a single graph for each model.

; Needs type of increment to plot ('physics','radiation','total') and box within which to composite
; (e.g., W or E eqIO)

IF KEYWORD_SET(model_names) THEN BEGIN
   our_model_names=model_names
ENDIF ELSE $
   our_model_names=['cnrm_atmos','cam5','ecearth','giss','cancm4','miroc','nasa']

n_models=N_ELEMENTS(our_model_names)
ntpd=4

model_nz=22
model_increments=fltarr(n_models,5,4,model_nz);,n_lead_times)
model_colors=strarr(n_models)
model_names=strarr(n_models)
file_descs=strarr(n_models)
reverse_levels=intarr(n_models)

total_precip=fltarr(n_models,5,4)
;frac_nearzero_points=fltarr(n_models);,n_lead_times)

FOR i=0,n_models-1 DO BEGIN
   CASE our_model_names(i) OF
      'cancm4' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/cancm4'
         file_descs(i)='CanCM4'
         model_names(i)='CM'
         model_colors(i)='dodgerblue'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e20
         ndpy=365
         n_years=20
      END
      'giss' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/giss'
         file_descs(i)='ModelE'
         model_names(i)='GI'
         model_colors(i)='violetred'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=1
         level_multiplier=1
         missing_value=1e20
         ndpy=365
         n_years=20
      END
      'ecearth' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/ecearth'
         file_descs(i)='ecearth3'
         model_names(i)='E3'
         model_colors(i)='limegreen'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e20
         ndpy=365
         n_years=20
      END
      'cam5' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/cam5_short'
         file_descs(i)='NCAR-CAM5'
         model_names(i)='C5'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='levels'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.*1000.
         level_multiplier=1
         missing_value=1e20
         ndpy=365
         n_years=2
      END
      'cnrm_atmos' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/cnrm_atmos'
         file_descs(i)='CNRM'
         model_names(i)='CN'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1
         missing_value=1e20
         ndpy=365
         n_years=20
      END
      'nasa' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/nasa'
         file_descs(i)='GEOS5_AGCM'
         model_names(i)='NA'
         longitude_name='lon'
         latitude_name='lat'
         level_name='lev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e20
         ndpy=365
         n_years=20
      END
      'miroc' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/miroc'
         file_descs(i)='miroc5'
         model_names(i)='MI'
         longitude_name='longitude'
         latitude_name='latitude'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=-999
         ndpy=365
         n_years=20
      END      
      'metum' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/metum'
         file_descs(i)='MetUM'
         model_names(i)='MO'
         longitude_name='lon'
         latitude_name='lat'
         level_name='level'
         reverse_levels(i)=0
         multiplier=86400.;*1000.
         precip_multiplier=86400.
         level_multiplier=1.
         missing_value=1e10
         ndpy=365
         n_years=4
      END      
      'mri' : BEGIN
         indir='/home/ss901165/um_output6/mjodiab_20year/mri'
         file_descs(i)='MRI-AGCM'
         model_names(i)='MR'
         longitude_name='lon'
         latitude_name='lat'
         level_name='plev'
         reverse_levels(i)=0
         multiplier=86400.*1000.
         precip_multiplier=86400.
         level_multiplier=0.01
         missing_value=1e10
         ndpy=365
         n_years=20
      END      
   ENDCASE

   CASE inc_type OF
      'physics' : BEGIN
         increment_name='tnhus_physics'
         xmin=[-6.5,-4,-4,-4,-4]
         xmax=[6.5,4,6,6,6]
         xstep=[1.0,0.5,0.6,0.5,0.5]
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
         xmin=[-1.5,-2,-2,-1,-1]
         xmax=[1.5,2,2,1,1]
         xstep=[0.3,0.4,0.4,0.2,0.2]
         legxoffset=-7000
         legyoffset=25000
      END      
   ENDCASE
                                ; Read all available precipitation data for region to compute quartiles
                                ; and classify into suppressed/transition/active
   grid_flag=0

   model_pr_infile=indir+'/'+file_descs(i)+'.pr.1991-2010.nc'
   IF grid_flag eq 0 THEN BEGIN
      model_longitude=OPEN_AND_EXTRACT(model_pr_infile(0),longitude_name)
      model_latitude=OPEN_AND_EXTRACT(model_pr_infile(0),latitude_name)      
      DEFINE_BOUNDARIES,box,model_latitude,model_longitude,model_box_tx,/LIMIT
      model_nlon=N_ELEMENTS(model_longitude)
      model_nlat=N_ELEMENTS(model_latitude)      
   ENDIF

   model_precip=REFORM(OPEN_AND_EXTRACT(model_pr_infile(0),'pr',$
                                        offset=[model_box_tx(1),model_box_tx(0),0],$
                                        count=[model_nlon,model_nlat,ndpy*ntpd*n_years]))*precip_multiplier
   IF TOTAL(where(ABS(model_precip) ge ABS(missing_value))) ge 0 THEN $
      model_precip[where(ABS(model_precip) ge ABS(missing_value))]=!Values.F_NaN
   print,'Read precipitation'
   
   model_precip_quartiles=fltarr(3,ntpd)
   model_precip_quartiles_npts=fltarr(ntpd)
   model_precip_localtrend=fltarr(model_nlon,model_nlat,ntpd*n_years*ndpy)
   FOR m=0,ntpd-1 DO BEGIN
      temp_precip=REFORM(model_precip(*,*,m:n_years*ndpy*ntpd-1:ntpd),[model_nlon*model_nlat*n_years*ndpy])
      IF TOTAL(where(temp_precip ge 1)) ge 0 THEN BEGIN
         temp_precip=temp_precip[where(temp_precip ge 1)]
         indices=SORT(temp_precip)         
         FOR p=1,3 DO $
            model_precip_quartiles(p-1,m)=temp_precip(indices(N_ELEMENTS(temp_precip)*p/4.))
         model_precip_quartiles_npts(m)=N_ELEMENTS(temp_precip)/4.
      ENDIF ELSE BEGIN
         model_precip_quartiles(*,m)=0.
         model_precip_quartiles_npts(m,n)=0.
      ENDELSE
   ENDFOR
   
   FOR m=0,model_nlon-1 DO BEGIN
      FOR n=0,model_nlat-1 DO BEGIN
         precip_ts=REFORM(model_precip(m,n,*))
         precip_ts=SMOOTH(precip_ts,9)
         FOR r=4,ntpd*n_years*ndpy-5 DO BEGIN
            trend=REGRESS(indgen(9),precip_ts(r-4:r+4))
            correlation=CORRELATE(indgen(9)*trend(0),precip_ts(r-4:r+4))
            IF trend(0) ge 0 and correlation ge 0.5 THEN BEGIN
               model_precip_localtrend(m,n,r)=1
            ENDIF ELSE IF trend(0) le 0 and correlation ge 0.5 THEN BEGIN
               model_precip_localtrend(m,n,r)=-1
            ENDIF ELSE $
               model_precip_localtrend(m,n,r)=0
         ENDFOR
      ENDFOR
   ENDFOR

   print,'Computed quartiles and trends'
               
   grid_flag=0
   model_infile=indir+'/'+'/'+file_descs(i)+'.'+increment_name+'.1991-2010.nc'
   IF grid_flag eq 0 THEN BEGIN
      model_inc_longitude=OPEN_AND_EXTRACT(model_infile(0),longitude_name)
      model_inc_latitude=OPEN_AND_EXTRACT(model_infile(0),latitude_name)
      model_z=OPEN_AND_EXTRACT(model_infile(0),level_name)*level_multiplier
      DEFINE_BOUNDARIES,box,model_inc_latitude,model_inc_longitude,model_inc_box_tx,/LIMIT
      model_inc_nlon=N_ELEMENTS(model_longitude)
      model_inc_nlat=N_ELEMENTS(model_latitude)
      model_nz=N_ELEMENTS(model_z)                  
      grid_flag=1
      this_increment=fltarr(model_nlon,model_nlat,model_nz,n_years*ntpd*ndpy)              
   ENDIF
   this_increment=OPEN_AND_EXTRACT(model_infile(0),increment_name,$
                                   offset=[model_inc_box_tx(1),model_inc_box_tx(0),0,0],$
                                   count=[model_inc_nlon,model_inc_nlat,model_nz,ntpd*n_years*ndpy])*multiplier
   
   IF TOTAL(where(ABS(this_increment) ge ABS(missing_value))) ge 0 THEN $
      this_increment[where(ABS(this_increment) ge ABS(missing_value))]=!Values.F_NaN
   print,'Read increments'
   
   total_points=fltarr(5,4)
   
   FOR j=0,4 DO BEGIN ; Loop over quartiles
      FOR m=0,3 DO BEGIN ; Loop over precip trends (0=all, 1=down, 2=steady/not significant, 3=up) 
         FOR q=0,ntpd-1 DO BEGIN
                                ; this_pt_olr=REFORM(model_olr(n,p,q:n_days*ntpd-1:ntpd,k))
            this_pt_pr=REFORM(model_precip(*,*,q:n_years*ndpy*ntpd-1:ntpd))
            this_pt_trend=REFORM(model_precip_localtrend(*,*,q:n_years*ndpy*ntpd-1:ntpd))
            CASE j OF
               0 : BEGIN
                  pr_points=where(this_pt_pr lt model_precip_quartiles(0,q) and this_pt_pr ge 1)
               END
               1 : BEGIN
                  pr_points=where(this_pt_pr lt model_precip_quartiles(1,q) and this_pt_pr ge model_precip_quartiles(0,q))
               END
               2 : BEGIN
                  pr_points=where(this_pt_pr lt model_precip_quartiles(2,q) and this_pt_pr ge model_precip_quartiles(1,q))
               END
               3 : BEGIN
                  pr_points=where(this_pt_pr ge model_precip_quartiles(2,q))
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
                  this_pt_inc=REFORM(this_increment(*,*,r,q:n_years*ndpy*ntpd-1:ntpd))                 
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
            ENDIF               ;ELSE $
;                  model_increments(i,j,m,*)=!Values.F_NaN
         ENDFOR
      ENDFOR
   ENDFOR

   print,'Computed composite increments'

   FOR r=0,model_nz-1 DO $
      model_increments(i,*,*,r)=model_increments(i,*,*,r)/FLOAT(total_points(*,*))
   total_precip(i,*,*)=total_precip(i,*,*)/FLOAT(total_points(*,*))

;   frac_nearzero_points(i)=total_nearzero_points/FLOAT(model_nlon*model_nlat*n_days*ntpd)
ENDFOR

colors=['red','orange','cyan','blue','black']
styles=[0,1,2,3]
syms=[3,4,5,6]
FOR m=0,n_models-1 DO BEGIN
   psfile='/home/ss901165/idl/mjo_diabatic/tendencies/composite_precip/mjo_diabatic_20yr_tendencies_composite_precip_q_manyquartiles_10x10.'+$
          file_descs(m)+'.'+inc_type+'.'+region_name+'.1991-2010.ps'      
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
   GSET,XMIN=xmin(0),XMAX=xmax(0),YMIN=0,YMAX=40
   print,file_descs(m),total_precip(m,*,0)
   FOR j=0,3 DO $
      FOR k=0,0 DO $
         GPLOT,X=xmax(0),Y=total_precip(m,j,k),COL=FSC_COLOR(colors(j)),SYM=syms(k),SIZE=200
   AXES,YSTEP=2,YTITLE='Mean rainfall in bin (mm day!U-1!N)',/ONLYRIGHT,NDECS=1
   PSCLOSE
ENDFOR


STOP
END
