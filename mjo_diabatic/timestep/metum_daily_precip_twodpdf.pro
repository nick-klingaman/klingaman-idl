PRO metum_daily_precip_twodpdf

n_models=5
set_name='ga7mem'
hg3='/home/ss901165/um_output3/hadgem3_monwg'
um6='/home/ss901165/um_output6'
trmm='/home/users/npklingaman/datasets/TRMM_3B42'
cmorph='/home/users/npklingaman/datasets/CMORPH'
gc2='/home/users/npklingaman/klingaman/metum/gc2'

; For 2D PDFs
box=[-10,60,20,180]
heaviside=[1,2,4,6,9,12,16,20,25,30,40,60,90,130,180]
n_heavi=N_ELEMENTS(heaviside)

; For spatial correlations
region_xsize=12000
region_ysize=2000
box_size=1000 ; in km
max_box_npts_side=100
region_corr=fltarr(n_models,max_box_npts_side)
domain_size=7 ; in gridpoints

thresholds=[1,5,20,40,100]
n_thresholds=N_ELEMENTS(thresholds)

; For temporal correlations
time_size=168 ; in hours
max_tsteps=8
temporal_corr=fltarr(n_models,max_tsteps)
all_nsteps=intarr(n_models)

; For radius vs. lag
mylevs_radvlag=['-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
nlags=7

all_box_size=intarr(n_models)
all_dx=intarr(n_models)
all_dt=fltarr(n_models)
all_colors=strarr(n_models)
all_npts_side=intarr(n_models)
all_psfile_titles=strarr(n_models)
all_syms=intarr(n_models)
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      2 : BEGIN
         infile=gc2+'/mi-ah288/ah288.jun-sep_dmeans_ts.1989-2008.precip.nc'
         tsteps=120*20-1
         time_offset=1
         tstep_length=86400.
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precipitation_flux'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='mi-ah288_protoGA7_CTL'
         all_dx(i)=200
         all_dt(i)=24
         all_colors(i)='red'
         all_box_size(i)=1400
         all_syms(i)=2
      END
      0 : BEGIN
         infile=gc2+'/mi-ah326/ah326.jun-sep_dmeans_ts.1989-2008.precip.nc'
         tsteps=120*20-1
         time_offset=1
         tstep_length=86400.
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precipitation_flux'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='mi-ah326_GA6+6A_CTL'
         all_dx(i)=200
         all_dt(i)=24
         all_colors(i)='blue'
         all_box_size(i)=1400
         all_syms(i)=2
         missing_value=2e20
      END
      1 : BEGIN
         infile=gc2+'/mi-ai191/ai191.jun-sep_dmeans_ts.1989-2008.precip.nc'
         tsteps=120*20-1
         time_offset=1
         tstep_length=86400.
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precipitation_flux'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='mi-ai191_GA6+6A_MEM'
         all_dx(i)=200
         all_dt(i)=24
         all_colors(i)='cyan'
         all_box_size(i)=1400
         all_syms(i)=2
      END
      3 : BEGIN
         infile=gc2+'/mi-ai647/ai647.jun-sep_dmeans_ts.1989-2008.precip.nc'
         tsteps=120*20-1
         time_offset=1
         tstep_length=86400.
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precipitation_flux'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='mi-ai647_protoGA7_MEM'
         all_dx(i)=200
         all_dt(i)=24
         all_colors(i)='orange'
         all_box_size(i)=1400
         all_syms(i)=2
      END

      6 : BEGIN
         infile=gc2+'/antia/hadgem3_ga6_n96.jan-dec_dmeans_ts.years1-27.precip.nc'
         tsteps=360*27-1
         time_offset=1
         tstep_length=86400
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='GA6_N96p'
         all_dx(i)=200 ; in km
         all_dt(i)=24 ; in hours
         all_colors(i)='darkslateblue'
	 all_box_size(i)=1400
         all_syms(i)=1
      END
      7 : BEGIN
         infile=gc2+'/antib/hadgem3_ga6_n216.jan-dec_dmeans_ts.years1-27.precip.nc'
         tsteps=360*27-1
         time_offset=1
         tstep_length=86400
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='GA6_N216p'
         all_dx(i)=90 ; in km
         all_dt(i)=24 ; in hours
         all_colors(i)='dodgerblue'
	 all_box_size(i)=1400
         all_syms(i)=2
      END
      6 : BEGIN
         infile=gc2+'/anrid/hadgem3_ga6_n512.jan-dec_dmeans_ts.years1-9.precip.nc'
         tsteps=3449
         tstep_length=86400
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         psfile_title='GA6_N512p'
         all_dx(i)=40
         all_dt(i)=24
         all_colors(i)='teal'
	 all_box_size(i)=1400
         all_syms(i)=3
      END

      4 : BEGIN
;         infile=trmm+'/native_resolution/TRMM_3B42v6A.jun-sep_dmeans.1999-2010.precip.nc'
         infile=trmm+'/n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
         tsteps=365*13-1
         time_offset=1
         tstep_length=86400
         multiplier=1
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         psfile_title='TRMM_N96'
         all_dx(i)=200
         all_dt(i)=24
         all_box_size(i)=1050
         all_colors(i)='black'
         box=[-10,60,20,160]
         all_syms(i)=6
      END      
      5 : BEGIN
         infile=cmorph+'/CMORPH.jun-sep_dmeans_ts.2001-2012.precip.nc'
         tsteps=11700/8
         time_offset=0
         tstep_length=86400
         multiplier=8.
         lon_name='lon'
         lat_name='lat'
         var_name='precip'
         psfile_title='CMORPH'
         all_dx(i)=25
         all_dt(i)=24
         all_box_size(i)=1050
         all_colors(i)='purple'
         box=[-10,60,20,160]
         all_syms(i)=7
      END  
   ENDCASE   

  ; Read dimension information and find box to analyse
   lon=OPEN_AND_EXTRACT(infile,lon_name)
   lat=OPEN_AND_EXTRACT(infile,lat_name)
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)

   IF nlat lt domain_size THEN BEGIN
      print,'ERROR: Number of latitude points in region to analyse ([box]) is smaller than length of sub-domain ([domain_size]).'
      print,'You requested sub-domains of length '+STRTRIM(STRING(domain_size),1),' but there are only '+STRTRIM(STRING(nlat),1),' latitude points.'
      STOP
   ENDIF ELSE IF nlon lt domain_size THEN BEGIN
      print,'ERROR: Number of longitude points in region to analyse ([box]) is smaller than length of sub-domain ([domain_size]).'
      print,'You requested sub-domains of length '+STRTRIM(STRING(domain_size),1),' but there are only '+STRTRIM(STRING(nlat),1),' longitude points.'
      STOP
   ENDIF
  
   ; Read precipitation data
   precip = OPEN_AND_EXTRACT(infile,var_name,$
                             offset=[box_tx(1),box_tx(0),time_offset],$
                             count=[nlon,nlat,tsteps])*multiplier
   
   n_missing=N_ELEMENTS(where(precip eq missing_value))
   precip[where(precip eq missing_value)]=!Values.F_NaN

                                ; Arrays for maps of on/off and off/on
                                ; frequency.  Note these are not
                                ; currently plotted.
   spatial_freq=fltarr(nlon,nlat)
   spatial_freq_onoff=fltarr(nlon,nlat)
   spatial_freq_offon=fltarr(nlon,nlat)
   spatial_freq_on=fltarr(nlon,nlat)
   spatial_freq_off=fltarr(nlon,nlat)

                                ; Arrays for 2D PDF of daily
                                ; rainfall (t vs. t+1), as well as 1D
                                ; pdf of rainfall.
   twod_pdf_prct=fltarr(n_heavi+1,n_heavi+1)
   oned_pdf=fltarr(n_heavi+1)
   oned_pdf_prct=fltarr(n_heavi+1)
   heaviside_tstep=heaviside

   ; Loop over gridpoints
   FOR j=0,nlon-1 DO BEGIN
      FOR k=0,nlat-1 DO BEGIN
         ts=REFORM(precip(j,k,*))
         ; Compute 2D PDFs of daily rainfall and on/off, off/on frequency.
         FOR m=0,n_heavi-2 DO BEGIN
            IF TOTAL(where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1) and FINITE(ts) eq 1)) ge 0 THEN BEGIN
               valid=where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))
               oned_pdf_prct(m+1)=oned_pdf_prct(m+1)+N_ELEMENTS(valid)
               next=ts[valid+1]
               FOR n=0,n_heavi-2 DO BEGIN
                  IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1) and FINITE(next) eq 1)) ge 0 THEN $
                     twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(m+1,n+1)
               ENDFOR
               IF TOTAL(where(next lt heaviside_tstep(0) and FINITE(next) eq 1)) ge 0 THEN BEGIN
                  twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(m+1,0)
                  IF where(row_map eq m) ge 0 THEN BEGIN
                     spatial_freq_onoff(j,k)=spatial_freq_onoff(j,k)+N_ELEMENTS(where(next lt heaviside_tstep(0)))
                     spatial_freq_on(j,k)=spatial_freq_on(j,k)+N_ELEMENTS(valid)
                  ENDIF
               ENDIF
               IF TOTAL(where(next ge heaviside_tstep(n_heavi-1) and FINITE(next) eq 1)) ge 0 THEN $
                  twod_pdf_prct(m+1,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(m+1,n_heavi)
            ENDIF 
         ENDFOR
         IF TOTAL(where(ts lt heaviside_tstep(0) and FINITE(ts) eq 1)) ge 0 THEN BEGIN
            valid=where(ts lt heaviside_tstep(0))
            oned_pdf(0)=oned_pdf(0)+N_ELEMENTS(valid)
            oned_pdf_prct(0)=oned_pdf_prct(0)+N_ELEMENTS(valid)
            next=ts[valid+1]
            spatial_freq_off(j,k)=spatial_freq_off(j,k)+N_ELEMENTS(valid)
            FOR n=0,n_heavi-2 DO BEGIN
               IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1) and FINITE(next) eq 1)) ge 0 THEN BEGIN
                  twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(0,n+1)                
                  IF where(row_map eq n) ge 0 THEN $
                     spatial_freq_offon(j,k)=spatial_freq_offon(j,k)+N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))
               ENDIF
            ENDFOR
            IF TOTAL(where(next lt heaviside_tstep(0) and FINITE(next) eq 1)) ge 0 THEN $
               twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(0,0)
            IF TOTAL(where(next ge heaviside_tstep(n_heavi-1) and FINITE(next) eq 1)) ge 0 THEN $
               twod_pdf_prct(0,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(0,n_heavi)
         ENDIF           
         IF TOTAL(where(ts ge heaviside_tstep(n_heavi-1) and FINITE(ts) eq 1)) ge 0 THEN BEGIN
            valid=where(ts ge heaviside_tstep(n_heavi-1))
            oned_pdf_prct(n_heavi)=oned_pdf_prct(n_heavi)+N_ELEMENTS(valid)
            next=ts[valid+1]
            FOR n=0,n_heavi-2 DO BEGIN
               IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1) and FINITE(next) eq 1)) ge 0 THEN $
                  twod_pdf_prct(n_heavi,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(n_heavi,n+1)             
             ENDFOR
            IF TOTAL(where(next lt heaviside_tstep(0) and FINITE(next) eq 1)) ge 0 THEN $
               twod_pdf_prct(n_heavi,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(n_heavi,0)
            IF TOTAL(where(next ge heaviside_tstep(n_heavi-1) and FINITE(next) eq 1)) ge 0 THEN $
               twod_pdf_prct(n_heavi,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(n_heavi,n_heavi)
         ENDIF
      ENDFOR
   ENDFOR
   
                                ; Normalize results by number of
                                ; valid gridpoints and/or number of
                                ; days, as appropriate.
   spatial_freq=spatial_freq/FLOAT(tsteps)
   spatial_freq_on=spatial_freq_on/FLOAT(tsteps)
   spatial_freq_off=spatial_freq_off/FLOAT(tsteps)
   spatial_freq_onoff=spatial_freq_onoff/FLOAT(tsteps)
   spatial_freq_offon=spatial_freq_offon/FLOAT(tsteps)
   
   oned_pdf=oned_pdf/(FLOAT(nlon)*FLOAT(nlat)*FLOAT(tsteps))
   oned_pdf_prct=oned_pdf_prct/TOTAL(oned_pdf_prct)
   
   twod_pdf_prct_norm_col=fltarr(n_heavi+1,n_heavi+1)
   twod_pdf_prct_norm_row=fltarr(n_heavi+1,n_heavi+1)
   FOR m=0,n_heavi DO BEGIN
      twod_pdf_prct_norm_col(m,*)=twod_pdf_prct(m,*)/TOTAL(twod_pdf_prct(m,*))
      twod_pdf_prct_norm_row(*,m)=twod_pdf_prct(*,m)/TOTAL(twod_pdf_prct(*,m))
   ENDFOR
   twod_pdf_prct=twod_pdf_prct/TOTAL(twod_pdf_prct)

                                ; Plot 2D PDFs of daily rainfall as
                                ; filled contours, with 1D PDF of
                                ; rainfall as a line with symbols.
   
   
   mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
   psfile='/home/users/npklingaman/plots/mjo_diabatic/timestep/metum_daily_precip_twodpdf.'+psfile_title+'_prct_allyears.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=170,TCHARSIZE=100,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000,XOFFSET=1000,SPACE1=250
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
   LEVS,MANUAL=mylevs_prct
   CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,$
       CB_TITLE='Probability',/NOCOLBAR ;,TITLE=psfile_title
   GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
        YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
        XTITLE='Precipitation at time t (mm day!U-1!N)',YTITLE='Precipitation at time t+1 (mm day!U-1!N)',/NORIGHT,$
        ORIENTATION=35
   GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   IF TOTAL(where(oned_pdf_prct lt 0.001)) ge 0 THEN $
      oned_pdf_prct[where(oned_pdf_prct lt 0.001)]=!Values.F_NaN
   GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2,SYM=3
   AXES,YVALS=['1.0e-3','1.4e-3','2.0e-3','3.0e-3','4.5e-3','7.0e-3','1.0e-2','1.4e-2','2.0e-2','3.0e-2','4.5e-2',$
               '7.0e-2','1.0e-1','1.4e-1','2.0e-1','3.0e-1','4.5e-1','7.0e-1','1.0e+0'],$
        YTITLE='Probability of precipitation in bin',/ONLYRIGHT,NDECS=2
   PSCLOSE,/NOVIEW

      corr_plot=fltarr(domain_size/2+1,nlags)
   corr_coeffs=fltarr(domain_size,domain_size,nlags)
   valid_pts_sum=fltarr(domain_size,domain_size,nlags)
   frac_wet_values=fltarr(domain_size,domain_size,n_thresholds)
   frac_dry_values=fltarr(domain_size,domain_size,n_thresholds)
   central_count_wet=fltarr(n_thresholds)
   central_count_dry=fltarr(n_thresholds)
   avg_frac_wet=fltarr(n_thresholds)
   FOR p=0,n_thresholds-1 DO $
      avg_frac_wet(p)=N_ELEMENTS(where(precip gt thresholds(p)))/FLOAT(nlon*nlat*tsteps)
   
   ; Compute distance vs. lag plots
   bad_pts=0
                                ; Loop over sub-domains of size
                                ; [domain_size x domain_size] within
                                ; 'box' region.
   FOR j=0,nlon-domain_size,domain_size DO BEGIN
      xpt=j+domain_size/2      
      FOR k=0,nlat-domain_size,domain_size DO BEGIN
         ypt=k+domain_size/2
                                ; Obtain timeseries at central point in sub-domain.
         central_ts=REFORM(precip(xpt,ypt,*))
         FOR p=0,n_thresholds-1 DO BEGIN           
            central_ts_heaviside=fltarr((tsteps))
            IF TOTAL(where(central_ts gt thresholds(p) and FINITE(central_ts) eq 1)) ge 0 THEN $
               central_ts_heaviside[where(central_ts gt thresholds(p) and FINITE(central_ts) eq 1)]=1
            IF TOTAL(where(central_ts le thresholds(p) and FINITE(central_ts) eq 1)) ge 0 THEN $
               central_ts_heaviside[where(central_ts le thresholds(p) and FINITE(central_ts) eq 1)]=0
            central_count_wet(p)=central_count_wet(p)+TOTAL(central_ts_heaviside)
            central_count_dry(p)=central_count_dry(p)+(tsteps)-TOTAL(central_ts_heaviside)
                                ; Compute correlations between central
                                ; point and all other points.
            FOR m=xpt-domain_size/2,xpt+domain_size/2 DO BEGIN
               FOR n=ypt-domain_size/2,ypt+domain_size/2 DO BEGIN
                  thispt_ts=REFORM(precip(m,n,*))
                  thispt_ts_heaviside=fltarr((tsteps))
                  IF TOTAL(thispt_ts) ne 0 THEN BEGIN
                     IF TOTAL(where(thispt_ts gt thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts gt thresholds(p))]=1
                     IF TOTAL(where(thispt_ts le thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts le thresholds(p))]=0
                     IF p eq 0 THEN BEGIN
                        IF TOTAL(thispt_ts_heaviside) ne 0 and TOTAL(central_ts_heaviside) ne 0 THEN BEGIN
                           FOR r=0,nlags-1 DO BEGIN
                                ; Construct timeseries of points valid
                                ; at central point and at lagged point.
                              temp_central=central_ts(0:tsteps-1-r)
                              temp_distance=thispt_ts(r:tsteps-1)
                              valid=where(FINITE(temp_central) eq 1 and FINITE(temp_distance) eq 1)
                              IF N_ELEMENTS(valid) gt 2 THEN BEGIN
                                 corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                                    corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+$
                                    CORRELATE(temp_central[valid],temp_distance[valid])*N_ELEMENTS(valid)  
                                 valid_pts_sum(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                                    valid_pts_sum(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+N_ELEMENTS(valid)
                              ENDIF
                           ENDFOR
                        ENDIF ELSE $
                           bad_pts=bad_pts+1
                     ENDIF
                                ; Compute probabilities of wet|wet and
                                ; dry|dry between central point and
                                ; surrounding points.
                     frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                        frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                        (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 1)))
                     frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                        frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                        (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 0)))
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
  
   ; Normalize wet|wet and dry|dry values.
   FOR p=0,n_thresholds-1 DO BEGIN
      frac_wet_values(*,*,p)=frac_wet_values(*,*,p)/FLOAT(central_count_wet(p))
      frac_dry_values(*,*,p)=frac_dry_values(*,*,p)/FLOAT(central_count_dry(p))
   ENDFOR
                                ; Normalize correlation coefficients
                                ; by the number of points that went
                                ; into the correlations
   corr_coeffs=corr_coeffs/valid_pts_sum
     
   psfile='/home/users/npklingaman/plots/mjo_diabatic/timestep/metum_daily_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag_allyears.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
          TCHARSIZE=90,SPACE3=200
   plot_title=psfile_title
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=nlags-0.5,$
        TITLE='Correlation with central point at t=0 - '+plot_title
   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_radvlag)+1,white=[4]
   LEVS,MANUAL=mylevs_radvlag
   FOR p=0,nlags-1 DO BEGIN
      count=fltarr(domain_size/2+1)
      FOR r=0,domain_size-1 DO BEGIN
         FOR s=0,domain_size-1 DO BEGIN
            ;distance=SQRT((r-domain_size/2)^2+(s-domain_size/2)^2)
            distance=MAX([ABS(r-domain_size/2),ABS(s-domain_size/2)])
            corr_plot(distance,p)=corr_coeffs(r,s,p)+corr_plot(distance,p)
            count(distance)=count(distance)+1
         ENDFOR
      ENDFOR
      corr_plot(*,p)=corr_plot(*,p)/FLOAT(count)
   ENDFOR
   CON,X=indgen(domain_size/2+1),Y=indgen(nlags),FIELD=corr_plot,/BLOCK,/NOLINES,CB_WIDTH=115,$
       CB_TITLE='Mean correlation coefficient'
   FOR j=0,domain_size/2 DO $
      FOR k=0,nlags-1 DO $
         GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_plot(j,k)),1),0,5),ALIGN=0.5
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (same units as input data)'
   PSCLOSE,/NOVIEW

   
   ; Compute correlations with physical distance (in km) and time (in
; minutes), using information provided above about the native spatial
; (all_dx) and temporal (all_dt) resolutions of the dataset.

                                ; Compute the number of points in each sub-region.
   box_npts_side=all_box_size(i)/all_dx(i)
                                ; Test to make sure the number of
                                ; points in each sub-domain does not
                                ; exceed max_box_npts_side
   IF ODD(box_npts_side) ne 1 THEN $
      box_npts_side=box_npts_side-1
   IF box_npts_side/2 gt max_box_npts_side THEN BEGIN
      print,'ERROR: box_npts_side/2 > max_box_npts_side'
      STOP
   ENDIF   

   nregions=0
   valid_pts_sum=fltarr(max_box_npts_side)
   valid_pts_sum(*)=0.
                                ; Divide analysis region in as many
                                ; sub-regions as possible.
   FOR x=0,nlon-box_npts_side,box_npts_side DO BEGIN
      central_x=x+box_npts_side/2
      FOR y=0,nlat-box_npts_side,box_npts_side DO BEGIN
         central_y=y+box_npts_side/2
                                ; Obtain timeseries at central point
                                ; in this sub-region.
         central_point_ts=REFORM(precip(central_x,central_y,*)) 
                                ; Obtain precipitation data for the sub-region.
         region_precip=precip(central_x-box_npts_side/2:$
                              central_x+box_npts_side/2,$
                              central_y-box_npts_side/2:$
                              central_y+box_npts_side/2,*)
         distance=fltarr(box_npts_side,box_npts_side)
                                ; Loop over all points in region
         FOR s=0,box_npts_side-1 DO $
            FOR t=0,box_npts_side-1 DO $
                                ; Compute distance between this point
                                ; and the central point (note: points
                                ; that are one diagonal point away are
                                ; considered to be one point away, not
                                ; SQRT(2) points away.  Testing shows
                                ; this makes little difference to results.)
               distance(s,t)=MAX([ABS(box_npts_side/2-s),ABS(box_npts_side/2-t)])
                                ; Average precipitation over all
                                ; points that are the same distance
                                ; away from the central point, then
                                ; correlate resulting timeseries with
                                ; timeseries at the central point.
         FOR s=0,box_npts_side/2 DO BEGIN
            mean_precip=fltarr(tsteps)
            FOR t=0,tsteps-1 DO BEGIN
               temp_precip=REFORM(region_precip(*,*,t))
               mean_precip(t)=MEAN(temp_precip[where(distance eq s)])  ; ,/NaN) 
                                ; Add the /NaN flag to the MEAN()
                                ; function if you want to include in
                                ; the distance correlations  
                                ; areas where there are not a full set
                                ; of valid points.  Without /NaN, if
                                ; any one point in the region is
                                ; missing, the correlation will not be
                                ; computed. 
            ENDFOR
            valid=where(FINITE(central_point_ts) eq 1 and FINITE(mean_precip) eq 1)
            ;print,N_ELEMENTS(where(FINITE(central_point_ts) eq 1)),N_ELEMENTS(where(FINITE(mean_precip) eq 1))
            valid_pts=N_ELEMENTS(valid)
            IF valid_pts gt 2 THEN BEGIN
               region_corr(i,s)=region_corr(i,s)+CORRELATE(central_point_ts[valid],mean_precip[valid])*FLOAT(valid_pts)
               valid_pts_sum(s)=valid_pts_sum(s)+valid_pts
            ENDIF
         ENDFOR
         nregions=nregions+1
      ENDFOR
   ENDFOR
                                ; Normalize resulting correlations by
                                ; the number of valid points that went
                                ; into the correlations.
   FOR s=0,max_box_npts_side-1 DO BEGIN
      IF valid_pts_sum(s) gt 0 THEN BEGIN
         region_corr(i,s)=region_corr(i,s)/FLOAT(valid_pts_sum(s))
      ENDIF ELSE $
         region_corr(i,s)=!Values.F_NaN
   ENDFOR
   all_npts_side(i)=box_npts_side
   all_psfile_titles(i)=psfile_title

   all_nsteps(i)=FLOOR(time_size/all_dt(i))
   valid_pts=0
   FOR x=0,nlon-1 DO BEGIN
       FOR y=0,nlat-1 DO BEGIN
          thispt_precip=REFORM(precip(x,y,*))
          IF TOTAL(precip(x,y,*),/NaN) gt 0 and TOTAL(where(FINITE(precip(x,y,*)) eq 1)) gt 0 THEN BEGIN
             FOR t=0,all_nsteps(i) DO BEGIN
                temp_central=thispt_precip(0:tsteps-t-1)
                temp_lagged=thispt_precip(t:tsteps-1)
                valid=where(FINITE(temp_central) eq 1 and FINITE(temp_lagged) eq 1)
                temporal_corr(i,t)=CORRELATE(temp_central[valid],temp_lagged[valid])+$
                                   temporal_corr(i,t)
             ENDFOR
             valid_pts=valid_pts+1
          ENDIF
       ENDFOR
    ENDFOR
   IF valid_pts eq 0 THEN BEGIN
      print,'There are no points with valid rainfall data in your region.  Valid points must have a total rainfall > 0.'
      STOP
   ENDIF
   temporal_corr(i,0:all_nsteps(i))=temporal_corr(i,0:all_nsteps(i))/FLOAT(valid_pts)
   print,'------------------------------------'
   print,'Model: '+psfile_title
   print,'Number of longitude x latitude points in region: '+STRTRIM(STRING(nlon),1)+' x '+STRTRIM(STRING(nlat),1)
   print,'Length of sub-domain for correlations with distance: '+STRTRIM(STRING(box_npts_side),1)
   print,'Correlations have been computed up to '+STRTRIM(STRING(box_npts_side/2),1)+' points away from the central point.'
   print,'Number of sub-domains in your chosen region: '+STRTRIM(STRING(nregions),1)
   print,'Number of days for correlations with time: '+STRTRIM(STRING(all_nsteps(i)),1)
   print,'Number of missing values in dataset: '+STRTRIM(STRING(n_missing),1)  
   print,'For distance correlations, percent of sample discarded because of missing data: '+STRTRIM(STRING((1-TOTAL(FLOAT(valid_pts_sum(0:box_npts_side/2)))/(FLOAT(box_npts_side/2+1)*FLOAT(tsteps)*FLOAT(nregions)))*100.),1)+'%'
ENDFOR


psfile='/home/users/npklingaman/plots/mjo_diabatic/timestep/metum_daily_precip_twodpdf.many_distance_tstep_allyears.'+set_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=MAX(all_box_size)/2.,YMIN=0.0,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_npts_side(i)/2+1)*all_dx(i),Y=REFORM(region_corr(i,0:all_npts_side(i)/2)),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=all_syms(i)
   ;GPLOT,X=(indgen(all_npts_side(i)/2)+1)*all_dx(i),Y=REFORM(region_corr(i,1:all_npts_side(i)/2))-0.03,$
   ;      TEXT=STRTRIM(STRING(indgen(all_npts_side(i)/2)+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,MAX(all_box_size)/2],Y=[0,0],STYLE=1
GLEGEND,labels=all_psfile_titles,COL=FSC_COLOR(all_colors),length=0,SYM=all_syms,LEGPOS=9
AXES,XSTEP=100,XMINOR=50,XTITLE='Distance (km)',$
     YTITLE='Correlation of daily-mean precipitation with central point (x=0)',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/users/npklingaman/plots/mjo_diabatic/timestep/metum_daily_precip_twodpdf.many_time_tstep_allyears.'+set_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=time_size/24,YMIN=-0.2,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_nsteps(i)+1)*all_dt(i)/24.,Y=REFORM(temporal_corr(i,0:all_nsteps(i))),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=all_syms(i)
                                ;GPLOT,X=(indgen(all_nsteps(i))+1)*all_dt(i)*60.,Y=REFORM(temporal_corr(i,1:all_nsteps(i)))-0.03,$
                                ;      TEXT=STRTRIM(STRING(indgen(all_nsteps(i))+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,time_size/24],Y=[0,0],STYLE=1
GLEGEND,labels=all_psfile_titles,COL=FSC_COLOR(all_colors),length=0,SYM=all_syms,LEGPOS=9
AXES,XSTEP=1,XMINOR=0.5,XTITLE='Time (day)',$
     YTITLE='Lag correlation of gridpoint timestep precipitation',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

STOP
END
