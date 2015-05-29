PRO memory_timestep_precip_twodpdf_2p5

n_models=5
hg3='/home/ss901165/um_output3/hadgem3_monwg'
um6='/home/ss901165/um_output6'
gc2='/home/ss901165/um_output6/gc2'

; For 2D PDFs
box=[-10,60,10,180]
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
time_size=3 ; in hours
max_tsteps=40
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
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         infile=gc2+'/dlhbi/dlhbi.jun-sep_tsmeans.precip.2p5x2p5.nc'
         tsteps=8639
         tstep_length=1200
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='dlhbi_control'
         all_dx(i)=300 ; in km
         all_dt(i)=1/3. ; in hours
         all_colors(i)='red'
	 all_box_size(i)=1600
      END
      1 : BEGIN
         infile=gc2+'/dlhbu/dlhbu.jun-sep_tsmeans.precip.2p5x2p5.nc'         
         tsteps=8639
         tstep_length=1200
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='dlhbu_memprog'
         all_dx(i)=300 ; in km
         all_dt(i)=1/3. ; in hours
         all_colors(i)='orange'
	 all_box_size(i)=1600
      END
      2 : BEGIN
         infile=gc2+'/dlufc/dlufc.jun-sep_tsmeans.precip.2p5x2p5.nc'         
         tsteps=8639
         tstep_length=1200
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='dlufc_nodp_nosh'
         all_dx(i)=300 ; in km
         all_dt(i)=1/3. ; in hours
         all_colors(i)='blue'
	 all_box_size(i)=1600
      END
      3 : BEGIN
         infile=gc2+'/dlufd/dlufd.jun-sep_tsmeans.precip.2p5x2p5.nc'         
         tsteps=8639
         tstep_length=1200
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='dlufd_2xent'
         all_dx(i)=300 ; in km
         all_dt(i)=1/3. ; in hours
         all_colors(i)='purple'
	 all_box_size(i)=1600
      END      
      4 : BEGIN
         infile=gc2+'/dlufp/dlufp.jun-sep_tsmeans.precip.2p5x2p5.nc'         
         tsteps=11519
         tstep_length=900
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         var_name='precip'
         row_map=[11,12,13,14]
         column_map=[0]
         psfile_title='dlufp_mement'
         all_dx(i)=300 ; in km
         all_dt(i)=1/4. ; in hours
         all_colors(i)='violetred'
	 all_box_size(i)=1600
      END         
   ENDCASE

   lon=OPEN_AND_EXTRACT(infile,lon_name)
   lat=OPEN_AND_EXTRACT(infile,lat_name)
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)

   precip = OPEN_AND_EXTRACT(infile,var_name,$
                             offset=[box_tx(1),box_tx(0),1],$
                             count=[nlon,nlat,tsteps])*multiplier

   spatial_freq=fltarr(nlon,nlat)
   spatial_freq_onoff=fltarr(nlon,nlat)
   spatial_freq_offon=fltarr(nlon,nlat)
   spatial_freq_on=fltarr(nlon,nlat)
   spatial_freq_off=fltarr(nlon,nlat)

   twod_pdf_prct=fltarr(n_heavi+1,n_heavi+1)
   oned_pdf=fltarr(n_heavi+1)
   oned_pdf_prct=fltarr(n_heavi+1)
   heaviside_tstep=heaviside

   FOR j=0,nlon-1 DO BEGIN
      FOR k=0,nlat-1 DO BEGIN
         ts=REFORM(precip(j,k,*))
         FOR m=0,n_heavi-2 DO BEGIN
            IF TOTAL(where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))) ge 0 THEN BEGIN
               valid=where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))
               oned_pdf_prct(m+1)=oned_pdf_prct(m+1)+N_ELEMENTS(valid)
               next=ts[valid+1]
               FOR n=0,n_heavi-2 DO BEGIN
                  IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                     twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(m+1,n+1)
               ENDFOR
               IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN BEGIN
                  twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(m+1,0)
                  IF where(row_map eq m) ge 0 THEN BEGIN
                     spatial_freq_onoff(j,k)=spatial_freq_onoff(j,k)+N_ELEMENTS(where(next lt heaviside_tstep(0)))
                     spatial_freq_on(j,k)=spatial_freq_on(j,k)+N_ELEMENTS(valid)
                  ENDIF
               ENDIF
               IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                  twod_pdf_prct(m+1,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(m+1,n_heavi)
            ENDIF 
         ENDFOR
         IF TOTAL(where(ts lt heaviside_tstep(0))) ge 0 THEN BEGIN
            valid=where(ts lt heaviside_tstep(0))
            oned_pdf(0)=oned_pdf(0)+N_ELEMENTS(valid)
            oned_pdf_prct(0)=oned_pdf_prct(0)+N_ELEMENTS(valid)
            next=ts[valid+1]
            spatial_freq_off(j,k)=spatial_freq_off(j,k)+N_ELEMENTS(valid)
            FOR n=0,n_heavi-2 DO BEGIN
               IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN BEGIN
                  twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(0,n+1)                
                  IF where(row_map eq n) ge 0 THEN $
                     spatial_freq_offon(j,k)=spatial_freq_offon(j,k)+N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))
               ENDIF
            ENDFOR
            IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
               twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(0,0)
            IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
               twod_pdf_prct(0,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(0,n_heavi)
         ENDIF           
         IF TOTAL(where(ts ge heaviside_tstep(n_heavi-1))) ge 0 THEN BEGIN
            valid=where(ts ge heaviside_tstep(n_heavi-1))
            oned_pdf_prct(n_heavi)=oned_pdf_prct(n_heavi)+N_ELEMENTS(valid)
            next=ts[valid+1]
            FOR n=0,n_heavi-2 DO BEGIN
               IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                  twod_pdf_prct(n_heavi,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(n_heavi,n+1)             
             ENDFOR
            IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
               twod_pdf_prct(n_heavi,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(n_heavi,0)
            IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
               twod_pdf_prct(n_heavi,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(n_heavi,n_heavi)
         ENDIF
      ENDFOR
   ENDFOR
   
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

   ;mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
   ;psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_precip_twodpdf.'+psfile_title+'_prct.ps'
   ;PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=100,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
   ;GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
   ;CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
   ;LEVS,MANUAL=mylevs_prct
   ;CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Probability',TITLE=psfile_title
   ;GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
   ;AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
   ;     YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
   ;     XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
   ;     XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT    
   ;GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
   ;IF TOTAL(where(oned_pdf_prct lt 0.001)) ge 0 THEN $
   ;   oned_pdf_prct[where(oned_pdf_prct lt 0.001)]=!Values.F_NaN
   ;GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2,SYM=3
   ;AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
   ;PSCLOSE,/NOVIEW
   
   mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
   psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_precip_twodpdf_2p5.'+psfile_title+'_prct.ps'
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
        XTITLE='Precipitation at timestep t (mm day!U-1!N)',YTITLE='Precipitation at timestep t+1 (mm day!U-1!N)',/NORIGHT,$
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
   frac_wet_values=fltarr(domain_size,domain_size,n_thresholds)
   frac_dry_values=fltarr(domain_size,domain_size,n_thresholds)
   central_count_wet=fltarr(n_thresholds)
   central_count_dry=fltarr(n_thresholds)
   avg_frac_wet=fltarr(n_thresholds)
   FOR p=0,n_thresholds-1 DO $
      avg_frac_wet(p)=N_ELEMENTS(where(precip gt thresholds(p)))/FLOAT(nlon*nlat*tsteps)
   
   
   bad_pts=0
   FOR j=0,nlon-domain_size,domain_size DO BEGIN
      xpt=j+domain_size/2      
      FOR k=0,nlat-domain_size,domain_size DO BEGIN
         ypt=k+domain_size/2
         central_ts=REFORM(precip(xpt,ypt,*))
         FOR p=0,n_thresholds-1 DO BEGIN
            central_ts_heaviside=fltarr((tsteps))
            IF TOTAL(where(central_ts gt thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts gt thresholds(p))]=1
            IF TOTAL(where(central_ts le thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts le thresholds(p))]=0
            central_count_wet(p)=central_count_wet(p)+TOTAL(central_ts_heaviside)
            central_count_dry(p)=central_count_dry(p)+(tsteps)-TOTAL(central_ts_heaviside)
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
                           FOR r=0,nlags-1 DO $
                              corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                              corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+$
                              CORRELATE(central_ts(0:(tsteps)-1-r),$
                                        thispt_ts(r:(tsteps)-1))
                        ENDIF ELSE $
                           bad_pts=bad_pts+1
                     ENDIF
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
  
   FOR p=0,n_thresholds-1 DO BEGIN
      frac_wet_values(*,*,p)=frac_wet_values(*,*,p)/FLOAT(central_count_wet(p))
      frac_dry_values(*,*,p)=frac_dry_values(*,*,p)/FLOAT(central_count_dry(p))
   ENDFOR
   corr_coeffs=corr_coeffs/FLOAT(corr_coeffs(domain_size/2,domain_size/2))
      

  ; psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_spatialscale.'+psfile_title+'.'+$
  ;        STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag.ps'
  ; PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
  ;        TCHARSIZE=90,SPACE3=200
  ; plot_title=psfile_title
  ; GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=nlags-0.5,$
  ;      TITLE='Correlation with central point at t=0 - '+plot_title
  ; CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_radvlag)+1,white=[4]
  ; LEVS,MANUAL=mylevs_radvlag
  ; FOR p=0,nlags-1 DO BEGIN
  ;    count=fltarr(domain_size/2+1)
  ;    FOR r=0,domain_size-1 DO BEGIN
  ;       FOR s=0,domain_size-1 DO BEGIN
  ;          ;distance=SQRT((r-domain_size/2)^2+(s-domain_size/2)^2)
  ;          distance=MAX([ABS(r-domain_size/2),ABS(s-domain_size/2)])
  ;          corr_plot(distance,p)=corr_coeffs(r,s,p)+corr_plot(distance,p)
  ;          count(distance)=count(distance)+1
  ;       ENDFOR
  ;    ENDFOR
  ;    corr_plot(*,p)=corr_plot(*,p)/FLOAT(count)
  ; ENDFOR
  ; CON,X=indgen(domain_size/2+1),Y=indgen(nlags),FIELD=corr_plot,/BLOCK,/NOLINES,CB_WIDTH=115,$
  ;     CB_TITLE='Mean correlation coefficient'
  ; FOR j=0,domain_size/2 DO $
  ;    FOR k=0,nlags-1 DO $
  ;       GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_plot(j,k)),1),0,5),ALIGN=0.5
  ; AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (same units as input data)'
  ; PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_spatialscale_2p5.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=230,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
          TCHARSIZE=90,SPACE3=200
   plot_title=psfile_title
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=nlags-0.5 ;,$
                                ;TITLE='Correlation with central point at t=0 - '+plot_title
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs_radvlag)+1
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
   CON,X=indgen(domain_size/2+1),Y=indgen(nlags),FIELD=corr_plot,/BLOCK,/NOLINES,/NOCOLBAR; ,CB_WIDTH=115,$
                                ;CB_TITLE='Mean correlation coefficient'
   black=FSC_COLOR('black',30)
   white=FSC_COLOR('white',31)
   FOR j=0,domain_size/2 DO BEGIN
      FOR k=0,nlags-1 DO BEGIN
         IF corr_plot(j,k) ge 0.6 THEN BEGIN
            color=31
         ENDIF ELSE $
            color=30
         GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_plot(j,k)),1),0,5),ALIGN=0.5,VALIGN=0.5,COL=color
      ENDFOR
   ENDFOR
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (timesteps)'
   PSCLOSE,/NOVIEW

   box_npts_side=all_box_size(i)/all_dx(i)
   IF ODD(box_npts_side) ne 1 THEN $
      box_npts_side=box_npts_side-1
   IF box_npts_side/2 gt max_box_npts_side THEN BEGIN
      print,'ERROR: box_npts_side/2 > max_box_npts_side'
      STOP
   ENDIF   

   nregions=0
   FOR x=0,nlon-box_npts_side,box_npts_side DO BEGIN
      central_x=x+box_npts_side/2
      FOR y=0,nlat-box_npts_side,box_npts_side DO BEGIN
         central_y=y+box_npts_side/2
         central_point_ts=REFORM(precip(central_x,central_y,*))      
         region_precip=precip(central_x-box_npts_side/2:$
                              central_x+box_npts_side/2,$
                              central_y-box_npts_side/2:$
                              central_y+box_npts_side/2,*)
         distance=fltarr(box_npts_side,box_npts_side)
         FOR s=0,box_npts_side-1 DO $
            FOR t=0,box_npts_side-1 DO $
               distance(s,t)=MAX([ABS(box_npts_side/2-s),ABS(box_npts_side/2-t)])
         FOR s=0,box_npts_side/2 DO BEGIN
            mean_precip=fltarr(tsteps)
            FOR t=0,tsteps-1 DO BEGIN
               temp_precip=REFORM(region_precip(*,*,t))
               mean_precip(t)=MEAN(temp_precip[where(distance eq s)])
            ENDFOR
            IF TOTAL(mean_precip) gt 0 and TOTAL(central_point_ts) gt 0 THEN $
               region_corr(i,s)=region_corr(i,s)+CORRELATE(central_point_ts,mean_precip)
         ENDFOR                  
         nregions=nregions+1
      ENDFOR
   ENDFOR
   region_corr(i,*)=region_corr(i,*)/FLOAT(nregions)   
   all_npts_side(i)=box_npts_side
   all_psfile_titles(i)=psfile_title

   all_nsteps(i)=FLOOR(time_size/all_dt(i))
   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN
         IF TOTAL(precip(x,y,*)) gt 0 THEN $
            temporal_corr(i,0:all_nsteps(i))=temporal_corr(i,0:all_nsteps(i))+$
                                             A_CORRELATE(REFORM(precip(x,y,*)),indgen(all_nsteps(i)+1))
      ENDFOR
   ENDFOR
   temporal_corr(i,0:all_nsteps(i,*))=temporal_corr(i,0:all_nsteps(i))/FLOAT(nlon*nlat)
   print,psfile_title,nlon,nlat,box_npts_side,nregions,all_nsteps(i)
ENDFOR


psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_precip_twodpdf_2p5.many_distance_tstep.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=MAX(all_box_size)/2.,YMIN=0.0,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_npts_side(i)/2+1)*all_dx(i),Y=REFORM(region_corr(i,0:all_npts_side(i)/2)),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=3
   ;GPLOT,X=(indgen(all_npts_side(i)/2)+1)*all_dx(i),Y=REFORM(region_corr(i,1:all_npts_side(i)/2))-0.03,$
   ;      TEXT=STRTRIM(STRING(indgen(all_npts_side(i)/2)+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,MAX(all_box_size)/2],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_psfile_titles),COL=REVERSE(FSC_COLOR(all_colors)),length=0,SYM=REPLICATE(3,n_models),LEGPOS=9
AXES,XSTEP=100,XMINOR=50,XTITLE='Distance (km)',$
     YTITLE='Correlation of timestep precipitation with central point (x=0)',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/timestep/memory_timestep_precip_twodpdf_2p5.many_time_tstep.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=time_size*60.,YMIN=0.0,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_nsteps(i)+1)*all_dt(i)*60.,Y=REFORM(temporal_corr(i,0:all_nsteps(i))),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=3
                                ;GPLOT,X=(indgen(all_nsteps(i))+1)*all_dt(i)*60.,Y=REFORM(temporal_corr(i,1:all_nsteps(i)))-0.03,$
                                ;      TEXT=STRTRIM(STRING(indgen(all_nsteps(i))+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,time_size*60],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(all_psfile_titles),COL=REVERSE(FSC_COLOR(all_colors)),length=0,SYM=REPLICATE(3,n_models),LEGPOS=9
AXES,XSTEP=30,XMINOR=15,XTITLE='Time (minutes)',$
     YTITLE='Lag correlation of gridpoint timestep precipitation',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

STOP
END
