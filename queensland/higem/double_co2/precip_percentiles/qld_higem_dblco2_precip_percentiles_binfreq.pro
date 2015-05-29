PRO qld_higem_dblco2_precip_percentiles_binfreq
  
; Bin daily rainfall from the 2x CO2 simulation by the percentiles 
; of the control simulation.

; Input files
higem_ctl_percentiles_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.aus_domain.nc'
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box approximating Queensland
box=[-10,138,-30,154]
box_name='queensland'

; Read land/sea mask
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

; Read HiGEM latitude and longitude
ctl_longitude=OPEN_AND_EXTRACT(higem_ctl_percentiles_infile,'longitude')
ctl_latitude=OPEN_AND_EXTRACT(higem_ctl_percentiles_infile,'latitude')
DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
ctl_nlon=N_ELEMENTS(ctl_longitude)
ctl_nlat=N_ELEMENTS(ctl_latitude)

percentiles=OPEN_AND_EXTRACT(higem_ctl_percentiles_infile,'percentile')
n_percentiles=N_ELEMENTS(percentiles)
                                ; Read the climatological percentiles of daily rainfall
ctl_percentile=REFORM(OPEN_AND_EXTRACT(higem_ctl_percentiles_infile,'amount_at_percentile',$
                                       offset=[ctl_box_tx(1),ctl_box_tx(0),0],$
                                       count=[ctl_nlon,ctl_nlat,n_percentiles]))
;ctl_percentile[where(ctl_percentile eq 2e20)]=!Values.F_NaN

n_seasons=5
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.dec-feb_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='dec-feb'
         extreme_frac_max=0.015
         extreme_count_max=0.55
         extreme_count_step=0.05
         extreme_ratio_max=1.8
         extreme_ratio_step=0.1
      END
      1 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.mar-may_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='mar-may'
         extreme_frac_max=0.015
         extreme_count_max=0.3
         extreme_count_step=0.03
         extreme_ratio_max=1.4
         extreme_ratio_step=0.1
      END
      2 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jun-aug_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jun-aug_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='jun-aug'
         extreme_frac_max=0.015
         extreme_count_max=0.1
         extreme_count_step=0.01
         extreme_ratio_max=3.0
         extreme_ratio_step=0.2
      END
      3 : BEGIN
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=90
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.sep-nov_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=90
         higem_2xco2_nyears=31
         season_name='sep-nov'
         extreme_frac_max=0.015
         extreme_count_max=0.1
         extreme_count_step=0.01
         extreme_ratio_max=1.4
         extreme_ratio_step=0.1
      END
      4 : BEGIN         
         higem_ctl_precip_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip.global_domain.nc'
         higem_ctl_ndays=360
         higem_ctl_nyears=149
         higem_2xco2_precip_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.global_domain.nc'
         higem_2xco2_ndays=360
         higem_2xco2_nyears=31
         season_name='may-apr'
         extreme_frac_max=0.015
         extreme_count_max=1
         extreme_count_step=0.1
         extreme_ratio_max=1.4
         extreme_ratio_step=0.1
      END
   ENDCASE
                                ; Read HiGEM latitude and longitude
   higem_ctl_longitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'longitude')
   higem_ctl_latitude=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_ctl_latitude,higem_ctl_longitude,higem_ctl_box_tx,/LIMIT
   higem_ctl_nlon=N_ELEMENTS(higem_ctl_longitude)
   higem_ctl_nlat=N_ELEMENTS(higem_ctl_latitude)

                                ; Read HiGEM latitude and longitude
   higem_2xco2_longitude=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'longitude')
   higem_2xco2_latitude=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'latitude')
   DEFINE_BOUNDARIES,box,higem_2xco2_latitude,higem_2xco2_longitude,higem_2xco2_box_tx,/LIMIT
   higem_2xco2_nlon=N_ELEMENTS(higem_2xco2_longitude)
   higem_2xco2_nlat=N_ELEMENTS(higem_2xco2_latitude)
   
                                ; Read the daily rainfall amounts for all days and all years
   print,'Reading daily rainfall ...'
   higem_2xco2_daily_rainfall=OPEN_AND_EXTRACT(higem_2xco2_precip_infile,'precip',$
                                               offset=[higem_2xco2_box_tx(1),higem_2xco2_box_tx(0),0,0],$
                                               count=[higem_2xco2_nlon,higem_2xco2_nlat,higem_2xco2_ndays,higem_2xco2_nyears])*86400.
   higem_ctl_daily_rainfall=OPEN_AND_EXTRACT(higem_ctl_precip_infile,'precip',$
                                             offset=[higem_ctl_box_tx(1),higem_ctl_box_tx(0),0,0],$
                                             count=[higem_ctl_nlon,higem_ctl_nlat,higem_ctl_ndays,higem_ctl_nyears])*86400.
   print,'... done reading'
  
   n_chunks=5
   nyears_per_chunk=29
   
   higem_ctl_wet_days=fltarr(higem_ctl_nlon,higem_ctl_nlat)
   higem_ctl_wet_days_chunks=fltarr(n_chunks,higem_ctl_nlon,higem_ctl_nlat)
   higem_2xco2_wet_days=fltarr(higem_2xco2_nlon,higem_2xco2_nlat)   
   higem_ctl_rainfall_binned=fltarr(n_percentiles+1)
   higem_ctl_rainfall_binned_chunks=fltarr(n_chunks,n_percentiles+1)
   higem_2xco2_rainfall_binned=fltarr(n_percentiles+1)
   midpoints=fltarr(n_percentiles+1)
   land_points=0
   FOR j=0,higem_ctl_nlon-1 DO BEGIN
      FOR k=0,higem_ctl_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            thispt_2xco2_daily_rainfall=REFORM(higem_2xco2_daily_rainfall(j,k,*,*))
            thispt_ctl_daily_rainfall=REFORM(higem_ctl_daily_rainfall(j,k,*,*))

            thispt_ctl_daily_rainfall_chunks=fltarr(n_chunks,nyears_per_chunk*higem_ctl_ndays)
            FOR m=0,n_chunks-1 DO BEGIN
               thispt_ctl_daily_rainfall_chunks(m,*)=REFORM(higem_ctl_daily_rainfall(j,k,*,m*nyears_per_chunk:(m+1)*nyears_per_chunk-1))
               higem_ctl_wet_days_chunks(m,j,k)=N_ELEMENTS(where(thispt_ctl_daily_rainfall_chunks(m,*) ge 1.))
            ENDFOR

            higem_2xco2_wet_days(j,k)=N_ELEMENTS(where(thispt_2xco2_daily_rainfall ge 1.))
            higem_ctl_wet_days(j,k)=N_ELEMENTS(where(thispt_ctl_daily_rainfall ge 1.))
            land_points=land_points+1
            FOR m=0,n_percentiles DO BEGIN
               IF m eq 0 THEN BEGIN
                  lower_bound=1
                  upper_bound=ctl_percentile(j,k,0)
                  midpoints(m)=percentiles(0)/2.
               ENDIF ELSE IF m eq n_percentiles THEN BEGIN
                  lower_bound=ctl_percentile(j,k,n_percentiles-1)
                  upper_bound=10000
                  midpoints(m)=(100+percentiles(n_percentiles-1))/2.
               ENDIF ELSE BEGIN
                  lower_bound=ctl_percentile(j,k,m-1)
                  upper_bound=ctl_percentile(j,k,m)
                  midpoints(m)=(percentiles(m-1)+percentiles(m))/2.
               ENDELSE
               IF TOTAL(where(thispt_2xco2_daily_rainfall ge lower_bound and thispt_2xco2_daily_rainfall lt upper_bound)) ge 0 THEN $
                  higem_2xco2_rainfall_binned(m)=higem_2xco2_rainfall_binned(m)+$
                                                 N_ELEMENTS(where(thispt_2xco2_daily_rainfall ge lower_bound and thispt_2xco2_daily_rainfall lt upper_bound))
               IF TOTAL(where(thispt_ctl_daily_rainfall ge lower_bound and thispt_ctl_daily_rainfall lt upper_bound)) ge 0 THEN $
                  higem_ctl_rainfall_binned(m)=higem_ctl_rainfall_binned(m)+$
                                                 N_ELEMENTS(where(thispt_ctl_daily_rainfall ge lower_bound and thispt_ctl_daily_rainfall lt upper_bound))
               FOR n=0,n_chunks-1 DO $
                  IF TOTAL(where(thispt_ctl_daily_rainfall_chunks(n,*) ge lower_bound and thispt_ctl_daily_rainfall_chunks(n,*) lt upper_bound)) ge 0 THEN $
                     higem_ctl_rainfall_binned_chunks(n,m)=higem_ctl_rainfall_binned_chunks(n,m)+$
                  N_ELEMENTS(where(thispt_ctl_daily_rainfall_chunks(n,*) ge lower_bound and thispt_ctl_daily_rainfall_chunks(n,*) lt upper_bound))               
            ENDFOR
         ENDIF
      ENDFOR      
   ENDFOR

   higem_2xco2_rainfall_count=higem_2xco2_rainfall_binned/(FLOAT(land_points)*FLOAT(higem_2xco2_nyears))
   higem_ctl_rainfall_count=higem_ctl_rainfall_binned/(FLOAT(land_points)*FLOAT(higem_ctl_nyears))
   higem_ctl_rainfall_count_chunks=fltarr(n_chunks,n_percentiles+1)
   FOR n=0,n_chunks-1 DO $
      higem_ctl_rainfall_count_chunks(n,*)=higem_ctl_rainfall_binned_chunks(n,*)/(FLOAT(land_points)*FLOAT(nyears_per_chunk))
   
   higem_2xco2_rainfall_binned=higem_2xco2_rainfall_binned/FLOAT(TOTAL(higem_2xco2_wet_days))
   higem_ctl_rainfall_binned=higem_ctl_rainfall_binned/FLOAT(TOTAL(higem_ctl_wet_days))
   FOR n=0,n_chunks-1 DO $
      higem_ctl_rainfall_binned_chunks(n,*)=higem_ctl_rainfall_binned_chunks(n,*)/FLOAT(TOTAL(higem_ctl_wet_days_chunks(n,*,*)))
   
   print,higem_2xco2_rainfall_binned
   print,TOTAL(higem_ctl_wet_days)/FLOAT(higem_ctl_nyears),TOTAL(higem_2xco2_wet_days)/FLOAT(higem_2xco2_nyears)

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_binfreq.'+box_name+'.'+season_name+'.1-90pct.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=89,YMIN=0.03,YMAX=0.09,TITLE='Frequency of '+season_name+' rainfall from HiGEM 2xCO2 and control fit into bins from all months of control - '+box_name
   HIST,X=midpoints+0.5,Y=higem_2xco2_rainfall_binned,WIDTH=60,FILLCOL=FSC_COLOR('red')
   HIST,X=midpoints-0.5,Y=higem_ctl_rainfall_binned,WIDTH=60,FILLCOL=FSC_COLOR('black')
   FOR n=0,NEAREST(percentiles,90) DO $  
      EBAR,X=midpoints(n)-0.5,Y=higem_ctl_rainfall_binned(n),ERROR_Y=[ABS(higem_ctl_rainfall_binned(n)-MIN(higem_ctl_rainfall_binned_chunks(*,n))),$
                                                                      MAX(higem_ctl_rainfall_binned_chunks(*,n)-higem_ctl_rainfall_binned(n))],$
           COL=FSC_COLOR('cyan')
   AXES,XSTEP=5,XMINOR=2.5,YSTEP=0.01,YMINOR=0.005,NDECS=3,XTITLE='Precipitation percentile (%) from HiGEM control simulation (using all months)',$
        YTITLE='Fraction of wet days in '+season_name,/NORIGHT
   GPLOT,X=[0,90],Y=[0.05,0.05],STYLE=2
   GSET,XMIN=0,XMAX=89,YMIN=0.6,YMAX=1.4
   ratio=higem_2xco2_rainfall_binned/higem_ctl_rainfall_binned
   GPLOT,X=midpoints[where(midpoints lt 90)],Y=ratio[where(midpoints lt 90)],COL=FSC_COLOR('blue')
   FOR n=0,n_chunks-1 DO BEGIN
      ratio=higem_2xco2_rainfall_binned/higem_ctl_rainfall_binned_chunks(n,*)
      GPLOT,X=midpoints[where(midpoints lt 90)],Y=ratio[where(midpoints lt 90)],COL=FSC_COLOR('blue'),STYLE=2
   ENDFOR
   GPLOT,X=[0,90],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=0.1,YMINOR=0.05,YTITLE='Ratio of normalised frequencies: 2xCO2 to CTL',/ONLYRIGHT,NDECS=1
   GLEGEND,LEGPOS=9,LABELS=REVERSE(['HiGEM CTL (150 years)','HiGEM 2xCO2 (30 years)','CTL 30 year segments']),COL=REVERSE(FSC_COLOR(['black','red','cyan'])),TYPE=2
   GLEGEND,LEGPOS=5,LABELS=REVERSE(['Clim freq in CTL (all months)','Ratio: 2xCO2 to CTL (150 years)','Ratio: 2xCO2 to CTL (30 years)']),$
           COL=REVERSE(FSC_COLOR(['black','blue','blue'])),STYLE=REVERSE([2,0,2])
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_binfreq.'+box_name+'.'+season_name+'.90-100pct.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=3000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,XMIN=90,XMAX=100,YMIN=0,YMAX=extreme_frac_max,TITLE='Frequency of '+season_name+' rainfall from HiGEM 2xCO2 and CTL fit into bins from all months of CTL - '+box_name
   HIST,X=midpoints+0.05,Y=higem_2xco2_rainfall_binned,WIDTH=60,FILLCOL=FSC_COLOR('red')
   HIST,X=midpoints-0.05,Y=higem_ctl_rainfall_binned,WIDTH=60,FILLCOL=FSC_COLOR('black')
   FOR n=NEAREST(percentiles,90)+1,n_percentiles DO $  
      EBAR,X=midpoints(n)-0.05,Y=higem_ctl_rainfall_binned(n),ERROR_Y=[ABS(higem_ctl_rainfall_binned(n)-MIN(higem_ctl_rainfall_binned_chunks(*,n))),$
                                                                      MAX(higem_ctl_rainfall_binned_chunks(*,n)-higem_ctl_rainfall_binned(n))],$
           COL=FSC_COLOR('cyan')
   AXES,XSTEP=1,YSTEP=0.001,YMINOR=0.0005,XMINOR=0.5,NDECS=4,XTITLE='Precipitation percentile (%) from HiGEM CTL simulation (using all months)',$
        YTITLE='Fraction of wet days in '+season_name,/NORIGHT   
   GPLOT,X=[90,100],Y=[0.01,0.01],STYLE=2
   GSET,XMIN=90,XMAX=100,YMIN=0.6,YMAX=extreme_ratio_max
   ratio=higem_2xco2_rainfall_binned/higem_ctl_rainfall_binned
   GPLOT,X=midpoints[where(midpoints gt 90)],Y=ratio[where(midpoints gt 90)],COL=FSC_COLOR('blue')
   FOR n=0,n_chunks-1 DO BEGIN
      ratio=higem_2xco2_rainfall_binned/higem_ctl_rainfall_binned_chunks(n,*)
      GPLOT,X=midpoints[where(midpoints gt 90)],Y=ratio[where(midpoints gt 90)],COL=FSC_COLOR('blue'),STYLE=2
   ENDFOR
   GPLOT,X=[90,100],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=extreme_ratio_step,YMINOR=extreme_ratio_step/2.,$
        YTITLE='Ratio of normalised frequencies: 2xCO2 to CTL',/ONLYRIGHT,NDECS=1
   GLEGEND,LEGPOS=5,LABELS=REVERSE(['HiGEM CTL (150 years)','HiGEM 2xCO2 (30 years)','CTL 30 year segments']),COL=REVERSE(FSC_COLOR(['black','red','cyan'])),TYPE=2
   GLEGEND,LEGPOS=1,LABELS=REVERSE(['Clim freq in CTL (all months)','Ratio: 2xCO2 to CTL (150 years)','Ratio: 2xCO2 to CTL (30 years)']),$
           COL=REVERSE(FSC_COLOR(['black','blue','blue'])),STYLE=REVERSE([2,0,2])
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_binfreq.'+box_name+$
          '.'+season_name+'.1-90pct_count.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=2000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=1000,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=89,YMIN=0.,YMAX=1.25*MAX([higem_2xco2_rainfall_count,higem_ctl_rainfall_count]),$
        TITLE='Frequency of '+season_name+' rainfall from HiGEM 2xCO2 and CTL fit into bins from all months of CTL - '+box_name
   HIST,X=midpoints+0.5,Y=higem_2xco2_rainfall_count,WIDTH=60,FILLCOL=FSC_COLOR('red')
   HIST,X=midpoints-0.5,Y=higem_ctl_rainfall_count,WIDTH=60,FILLCOL=FSC_COLOR('black')
   FOR n=0,NEAREST(percentiles,90) DO $  
      EBAR,X=midpoints(n)-0.5,Y=higem_ctl_rainfall_count(n),ERROR_Y=[ABS(higem_ctl_rainfall_count(n)-MIN(higem_ctl_rainfall_count_chunks(*,n))),$
                                                                     MAX(higem_ctl_rainfall_count_chunks(*,n)-higem_ctl_rainfall_count(n))],$
           COL=FSC_COLOR('cyan')
   AXES,XSTEP=5,XMINOR=2.5,YSTEP=0.2,YMINOR=0.1,NDECS=2,XTITLE='Precipitation percentile (%) from HiGEM CTL simulation (using all months)',$
        YTITLE='Days per gridpoint per '+season_name+' period',/NORIGHT
;   GPLOT,X=[0,90],Y=[0.05,0.05]*TOTAL(higem_ctl_wet_days)/(FLOAT(land_points)*FLOAT(higem_ctl_nyears)),STYLE=2
   GSET,XMIN=0,XMAX=89,YMIN=0.6,YMAX=1.4
   ratio=higem_2xco2_rainfall_count/higem_ctl_rainfall_count
   GPLOT,X=midpoints[where(midpoints lt 90)],Y=ratio[where(midpoints lt 90)],COL=FSC_COLOR('blue')
   FOR n=0,n_chunks-1 DO BEGIN
      ratio=higem_2xco2_rainfall_count/higem_ctl_rainfall_count_chunks(n,*)
      GPLOT,X=midpoints[where(midpoints lt 90)],Y=ratio[where(midpoints lt 90)],COL=FSC_COLOR('blue'),STYLE=2
   ENDFOR
   GPLOT,X=[0,90],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=0.1,YMINOR=0.05,YTITLE='Ratio of counts: 2xCO2 to CTL',/ONLYRIGHT,NDECS=1
   GLEGEND,LEGPOS=9,LABELS=REVERSE(['HiGEM CTL (150 years)','HiGEM 2xCO2 (30 years)','CTL 30 year segments']),COL=REVERSE(FSC_COLOR(['black','red','cyan'])),TYPE=2
   GLEGEND,LEGPOS=5,LABELS=REVERSE(['Clim freq in CTL (all months)','Ratio: 2xCO2 to CTL (150 years)','Ratio: 2xCO2 to CTL (30 years)']),$
           COL=REVERSE(FSC_COLOR(['black','blue','blue'])),STYLE=REVERSE([2,0,2])
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_binfreq.'+box_name+'.'+season_name+'.90-100pct_count.ps'
   PSOPEN,file=psfile,FONT=3,CHARSIZE=100,MARGIN=3000,SPACE1=100,SPACE2=500,XOFFSET=0,YOFFSET=0,TFONT=3,$
          TCHARSIZE=100,SPACE3=500
   GSET,XMIN=90,XMAX=100,YMIN=0,YMAX=extreme_count_max,$
        TITLE='Frequency of '+season_name+' rainfall from HiGEM 2xCO2 and CTL fit into bins from all months of CTL - '+box_name
   HIST,X=midpoints+0.05,Y=higem_2xco2_rainfall_count,WIDTH=60,FILLCOL=FSC_COLOR('red')
   HIST,X=midpoints-0.05,Y=higem_ctl_rainfall_count,WIDTH=60,FILLCOL=FSC_COLOR('black')
   FOR n=NEAREST(percentiles,90)+1,n_percentiles DO $
      EBAR,X=midpoints(n)-0.05,Y=higem_ctl_rainfall_count(n),ERROR_Y=[ABS(higem_ctl_rainfall_count(n)-MIN(higem_ctl_rainfall_count_chunks(*,n))),$
                                                                     MAX(higem_ctl_rainfall_count_chunks(*,n)-higem_ctl_rainfall_count(n))],$
           COL=FSC_COLOR('cyan')
   AXES,XSTEP=1,YSTEP=extreme_count_step,YMINOR=extreme_count_step/2.,XMINOR=0.5,NDECS=3,$
        XTITLE='Precipitation percentile (%) from HiGEM CTL simulation (using all months)',$
        YTITLE='Days per gridpoint per '+season_name+' period',/NORIGHT   
;   GPLOT,X=[90,100],Y=[0.01,0.01],STYLE=2
   GSET,XMIN=90,XMAX=100,YMIN=0.6,YMAX=extreme_ratio_max
   ratio=higem_2xco2_rainfall_count/higem_ctl_rainfall_count
   GPLOT,X=midpoints[where(midpoints gt 90)],Y=ratio[where(midpoints gt 90)],COL=FSC_COLOR('blue')
   FOR n=0,n_chunks-1 DO BEGIN
      ratio=higem_2xco2_rainfall_count/higem_ctl_rainfall_count_chunks(n,*)
      GPLOT,X=midpoints[where(midpoints gt 90)],Y=ratio[where(midpoints gt 90)],COL=FSC_COLOR('blue'),STYLE=2
   ENDFOR
   GPLOT,X=[90,100],Y=[1,1],COL=FSC_COLOR('blue'),STYLE=1
   AXES,YSTEP=extreme_ratio_step,YMINOR=extreme_ratio_step/2.,YTITLE='Ratio of counts: 2xCO2 to CTL',/ONLYRIGHT,NDECS=1
   GLEGEND,LEGPOS=1,LABELS=REVERSE(['HiGEM 2x CO2 (30 years)','HiGEM CTL (150 years)']),COL=REVERSE(FSC_COLOR(['red','black'])),TYPE=2
   GLEGEND,LEGPOS=5,LABELS=REVERSE(['Ratio of 2xCO2 to CTL','Ratio = 1']),$
           COL=REVERSE(FSC_COLOR(['blue','blue'])),STYLE=REVERSE([0,2])
   PSCLOSE,/NOVIEW

ENDFOR
   
STOP
END
