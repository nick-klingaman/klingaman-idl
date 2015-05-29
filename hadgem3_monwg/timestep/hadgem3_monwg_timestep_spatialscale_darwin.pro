PRO hadgem3_monwg_timestep_spatialscale_darwin

darwin='/home/ss901165/datasets/DARWIN_RADAR'

n_sets=3
thresholds=[1,5,20,40,100]
n_thresholds=N_ELEMENTS(thresholds)
domain_size=7 ; Ideally an odd number

FOR i=2,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      ; 6 : BEGIN
      ;    infile=answa+'/wp.nc'
      ;    varname='cvrain'
      ;    plot_title='answa (GA6 N1024 param) timestep cvrain - WPac'
      ;    start_read=1
      ;    n_time=30000
      ;    tsteps_per_day=15*24.
      ;    multiplier=86400.;/tsteps_per_day
      ;    psfile_title='answa_cvrain_WPac'
      ;    mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
      ;    row_map=[11,12,13,14]
      ;    column_map=[0]
      ;    box=[0,130,20,160]
      ;    use_map=1
      ; END
      0 : BEGIN
         infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon_5km_aavg.nc'
         varname='rr'
         plot_title='Darwin radar on 4km grid and 10min - 2005-06'
         start_read=0
         n_time=LONG(24346)
         tsteps_per_day=6*24.
         multiplier=1.
         psfile_title='darwin_0506_4km_10min'
         xname='longitude'
         yname='latitude'
      END
      1: BEGIN
         infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon_n1024_aavg.nc'
         varname='rr'
         plot_title='Darwin radar on N1024 grid and 10min - 2005-06'
         start_read=0
         n_time=LONG(24346)
         tsteps_per_day=6*24.
         multiplier=1.
         psfile_title='darwin_0506_n1024_10min'
      END
      2: BEGIN
         infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon.nc'
         varname='rr'
         plot_title='Darwin radar on 2.5km grid and 10min - 2005-06'
         start_read=0
         n_time=LONG(24346)
         tsteps_per_day=6*24.
         multiplier=24.
         psfile_title='darwin_0506_2p5km_10min'
         xname='x'
         yname='y'        
      END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,xname)   
   latitude=OPEN_AND_EXTRACT(infile,yname)
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
   precip=OPEN_AND_EXTRACT(infile,varname,$
                           offset=[0,0,start_read],$
                           count=[n_lon,n_lat,n_time])*multiplier
   IF TOTAL(where(precip lt 0)) ge 0 THEN $
      precip[where(precip lt 0)]=!Values.F_NaN
   IF TOTAL(where(precip gt 1e10)) ge 0 THEN $
      precip[where(precip gt 1e10)]=!Values.F_NaN
   precip_mean=fltarr(n_lon,n_lat)
   FOR j=0,n_lon-1 DO BEGIN
      xdist=300./FLOAT(n_lon-1)*(j-n_lon/2)
      FOR k=0,n_lat-1 DO BEGIN
         ydist=300./FLOAT(n_lat-1)*(k-n_lat/2)
         dist=SQRT(xdist^2+ydist^2)
         IF dist gt 150. THEN $
            precip(j,k,*)=!Values.F_NaN
      ENDFOR
   ENDFOR

   n_lags=7 ; Including zero
   corr_coeffs=fltarr(domain_size,domain_size,n_lags)
   frac_wet_values=fltarr(domain_size,domain_size,n_thresholds)
   frac_dry_values=fltarr(domain_size,domain_size,n_thresholds)
   central_count_wet=fltarr(n_thresholds)
   central_count_dry=fltarr(n_thresholds)
   avg_frac_wet=fltarr(n_thresholds)
   FOR p=0,n_thresholds-1 DO $
      avg_frac_wet(p)=N_ELEMENTS(where(precip gt thresholds(p)))/FLOAT(n_lon*n_lat*n_time)

   bad_pts=0
   FOR j=0,n_lon-domain_size,domain_size DO BEGIN
      xpt=j+domain_size/2      
      FOR k=0,n_lat-domain_size,domain_size DO BEGIN
         ypt=k+domain_size/2
         central_ts=REFORM(precip(xpt,ypt,*))
         FOR p=0,n_thresholds-1 DO BEGIN
            central_ts_heaviside=fltarr(n_time)
            IF TOTAL(where(central_ts gt thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts gt thresholds(p))]=1
            IF TOTAL(where(central_ts le thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts le thresholds(p))]=0
            central_count_wet(p)=central_count_wet(p)+TOTAL(central_ts_heaviside)
            central_count_dry(p)=central_count_dry(p)+n_time-TOTAL(central_ts_heaviside)
            FOR m=xpt-domain_size/2,xpt+domain_size/2 DO BEGIN
               FOR n=ypt-domain_size/2,ypt+domain_size/2 DO BEGIN               
                  thispt_ts=REFORM(precip(m,n,*))
                  thispt_ts_heaviside=fltarr(n_time)
                  IF TOTAL(thispt_ts) ne 0 THEN BEGIN
                     IF TOTAL(where(thispt_ts gt thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts gt thresholds(p))]=1
                     IF TOTAL(where(thispt_ts le thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts le thresholds(p))]=0
                     IF p eq 0 THEN BEGIN
                        IF TOTAL(thispt_ts_heaviside) ne 0 and TOTAL(central_ts_heaviside) ne 0 THEN BEGIN
                           FOR r=0,n_lags-1 DO BEGIN
                              thiscorr=CORRELATE(central_ts(0:n_time-1-r),thispt_ts(r:n_time-1))
                              IF FINITE(thiscorr) eq 1 THEN $
                                 corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                                 corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+$
                                 thiscorr
                           ENDFOR
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
   

   mylevs_corr=['0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30','0.34','0.38','0.42']
   mylevs_frac=['0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
   mylevs_ratio=['0.20','0.25','0.33','0.50','0.75','0.90','1.11','1.50','2.00','3.00','4.00','5.00']

   FOR p=0,n_lags-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_lag'+STRTRIM(STRING(p),1)+'.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Correlation of tstep precip at (x,y) and lag '+STRTRIM(STRING(p),1)+' with (0,0) - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_corr)+1
      LEVS,MANUAL=mylevs_corr
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(corr_coeffs(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Correlation coefficient'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(corr_coeffs(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      IF p eq 0 THEN BEGIN
         PSCLOSE
      ENDIF ELSE $
         PSCLOSE,/NOVIEW
   ENDFOR
   
   mylevs_radvlag=['-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
          TCHARSIZE=90,SPACE3=200
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=n_lags-0.5,TITLE='Correlation with central point at t=0 - '+plot_title
   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_radvlag)+1,white=[4]
   LEVS,MANUAL=mylevs_radvlag
   corr_plot=fltarr(domain_size/2+1,n_lags)
   FOR p=0,n_lags-1 DO BEGIN
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
   CON,X=indgen(domain_size/2+1),Y=indgen(n_lags),FIELD=corr_plot,/BLOCK,/NOLINES,CB_WIDTH=115,$
       CB_TITLE='Mean correlation coefficient'
   FOR j=0,domain_size/2 DO $
      FOR k=0,n_lags-1 DO $
         GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_plot(j,k)),1),0,5),ALIGN=0.5
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (timesteps)'
   PSCLOSE,/NOVIEW

   FOR p=0,n_thresholds-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.frac_wet_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given wet (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_frac)+1
      LEVS,MANUAL=mylevs_frac
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_wet_values(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Fraction of timesteps'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_wet_values(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
              STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.ratio_wet_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given wet (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[8],/REV
      LEVS,MANUAL=mylevs_ratio
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_wet_values(*,*,p)/avg_frac_wet(p)),$
          /NOLINES,/BLOCK,CB_WIDTH=110,CB_TITLE='Ratio to mean frequency'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_wet_values(j,k,p)/avg_frac_wet(p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.frac_dry_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given dry (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_frac)+1
      LEVS,MANUAL=mylevs_frac
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_dry_values(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Fraction of timesteps'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_dry_values(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.ratio_dry_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given dry (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[8],/REV
      LEVS,MANUAL=mylevs_ratio
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_dry_values(*,*,p)/avg_frac_wet(p)),$
          /NOLINES,/BLOCK,CB_WIDTH=110,CB_TITLE='Ratio to mean frequency'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_dry_values(j,k,p)/avg_frac_wet(p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW
   ENDFOR

ENDFOR

STOP
END

