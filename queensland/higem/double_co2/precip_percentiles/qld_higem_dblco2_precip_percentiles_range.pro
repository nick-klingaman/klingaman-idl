PRO qld_higem_dblco2_precip_percentiles_range

box=[-32,136,-8,156]
ranges=[1,2];,3]
n_ranges=N_ELEMENTS(ranges)
n_seasons=5

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

;n_days=90
ctl_nyears=149
stbl_nyears=31

mylevs_freq=[['0.01','0.02','0.03','0.05','0.07','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60'],$
             ['0.005','0.01','0.015','0.02','0.025','0.03','0.04','0.05','0.06','0.08','0.10','0.12','0.14','0.16','0.18','0.20']]
mylevs_diff=['-0.022','-0.018','-0.014','-0.010','-0.006','-0.002','0.002','0.006','0.010','0.014','0.018','0.022']

FOR s=0,n_seasons-1 DO BEGIN

   CASE s OF 
      0 : BEGIN
         ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.dec-feb_dmeans.o2-r3.precip.global_domain.nc'
         season_name='dec-feb'
         n_days=90
      END
      1 : BEGIN
         ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.mar-may_dmeans.o2-r3.precip.global_domain.nc'
         season_name='mar-may'
         n_days=90
      END
      2 : BEGIN
         ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jun-aug_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jun-aug_dmeans.o2-r3.precip.global_domain.nc'
         season_name='jun-aug'
         n_days=90
      END
      3 : BEGIN
         ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.sep-nov_dmeans.o2-r3.precip.global_domain.nc'
         season_name='sep-nov'
         n_days=90
      END
      4 : BEGIN
         ctl_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.may-apr_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
         stbl_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.may-apr_dmeans.o2-r3.precip.global_domain.nc'
         season_name='may-apr'
         n_days=360
      END
   ENDCASE
         
   stbl_longitude=OPEN_AND_EXTRACT(stbl_infile,'longitude')
   stbl_latitude=OPEN_AND_EXTRACT(stbl_infile,'latitude')
   DEFINE_BOUNDARIES,box,stbl_latitude,stbl_longitude,stbl_box_tx,/LIMIT
   stbl_nlon=N_ELEMENTS(stbl_longitude)
   stbl_nlat=N_ELEMENTS(stbl_latitude)
   
   ctl_longitude=OPEN_AND_EXTRACT(ctl_infile,'longitude')
   ctl_latitude=OPEN_AND_EXTRACT(ctl_infile,'latitude')
   DEFINE_BOUNDARIES,box,ctl_latitude,ctl_longitude,ctl_box_tx,/LIMIT
   ctl_nlon=N_ELEMENTS(ctl_longitude)
   ctl_nlat=N_ELEMENTS(ctl_latitude)
   
   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
   
   percentiles=OPEN_AND_EXTRACT(ctl_infile,'percentile')
   n_percentiles=N_ELEMENTS(percentiles)
   
   ctl_precip_percentiles=REFORM(OPEN_AND_EXTRACT(ctl_infile,'percentile_of_daily_rain',$
                                                  offset=[ctl_box_tx(1),ctl_box_tx(0),0,0],$
                                                  count=[ctl_nlon,ctl_nlat,ctl_nyears,n_days]),[ctl_nlon,ctl_nlat,LONG(ctl_nyears)*LONG(n_days)])
   ctl_precip_percentiles[where(ctl_precip_percentiles gt 2e19)]=-999.99
   
   ctl_percentiles=REFORM(OPEN_AND_EXTRACT(ctl_infile,'amount_at_percentile',$
                                           offset=[ctl_box_tx(1),ctl_box_tx(0),0],$
                                           count=[ctl_nlon,ctl_nlat,n_percentiles]))
   
   stbl_precip=REFORM(OPEN_AND_EXTRACT(stbl_infile,'precip',$
                                       offset=[stbl_box_tx(1),stbl_box_tx(0),0,0],$
                                       count=[stbl_nlon,stbl_nlat,n_days,stbl_nyears]),[stbl_nlon,stbl_nlat,stbl_nyears*n_days])*86400.
   stbl_precip_percentiles=fltarr(stbl_nlon,stbl_nlat,stbl_nyears*n_days)
   FOR i=0,stbl_nlon-1 DO BEGIN
      FOR j=0,stbl_nlat-1 DO BEGIN
         thispt_stbl_precip=REFORM(stbl_precip(i,j,*))
         temp=fltarr(stbl_nyears*n_days)
         temp(*)=-999.99
         FOR k=0,n_percentiles DO BEGIN
            IF k eq 0 THEN BEGIN
               lower_bound=1
               upper_bound=ctl_percentiles(i,j,k)
               percentile=0
            ENDIF ELSE IF k eq n_percentiles THEN BEGIN
               lower_bound=ctl_percentiles(i,j,n_percentiles-1)
               upper_bound=10000
               percentile=percentiles(n_percentiles-1)
            ENDIF ELSE BEGIN
               lower_bound=ctl_percentiles(i,j,k-1)
               upper_bound=ctl_percentiles(i,j,k)
               percentile=percentiles(k-1)
            ENDELSE
            test=where(thispt_stbl_precip ge lower_bound and thispt_stbl_precip lt upper_bound)
            IF TOTAL(test) ne -1 THEN $
               temp[where(thispt_stbl_precip ge lower_bound and thispt_stbl_precip lt upper_bound)]=percentile+0.01
         ENDFOR
         stbl_precip_percentiles(i,j,*)=temp
      ENDFOR
   ENDFOR
   
   ctl_range_count=fltarr(n_ranges,n_percentiles,(MAX(ranges)*2+1)^2)
   stbl_range_count=fltarr(n_ranges,n_percentiles,(MAX(ranges)*2+1)^2)

   FOR i=0,n_ranges-1 DO BEGIN
      ctl_total_valid_days=fltarr(n_percentiles)
      stbl_total_valid_days=fltarr(n_percentiles)
      FOR j=ranges(i),ctl_nlon-ranges(i)-1 DO BEGIN
         FOR k=ranges(i),ctl_nlat-ranges(i)-1 DO BEGIN
            IF mask(j,k) eq 1 THEN BEGIN
               thispt_ctl_precip_percentiles=REFORM(ctl_precip_percentiles(j,k,*))
               thispt_stbl_precip_percentiles=REFORM(stbl_precip_percentiles(j,k,*))
               FOR m=0,n_percentiles-1 DO BEGIN
                  
                  ctl_valid_days=where(thispt_ctl_precip_percentiles ge percentiles(m))
                  stbl_valid_days=where(thispt_stbl_precip_percentiles ge percentiles(m))
                  
                  ctl_total_valid_days(m)=ctl_total_valid_days(m)+N_ELEMENTS(ctl_valid_days)
                  stbl_total_valid_days(m)=stbl_total_valid_days(m)+N_ELEMENTS(stbl_valid_days)
                  
                  FOR n=0,N_ELEMENTS(ctl_valid_days)-1 DO BEGIN
                     rangepts=REFORM(ctl_precip_percentiles(j-ranges(i):j+ranges(i),k-ranges(i):k+ranges(i),ctl_valid_days(n)))
                     test=where(rangepts ge percentiles(m))
                     IF TOTAL(test) ne -1 THEN $
                        ctl_range_count(i,m,N_ELEMENTS(where(rangepts ge percentiles(m)))-1)=$
                        ctl_range_count(i,m,N_ELEMENTS(where(rangepts ge percentiles(m)))-1)+1
                  ENDFOR
                  
                  IF TOTAL(stbl_valid_days) ne -1 THEN BEGIN
                     FOR n=0,N_ELEMENTS(stbl_valid_days)-1 DO BEGIN
                        rangepts=REFORM(stbl_precip_percentiles(j-ranges(i):j+ranges(i),k-ranges(i):k+ranges(i),stbl_valid_days(n)))
                        test=where(rangepts ge percentiles(m))
                        IF TOTAL(test) ne -1 THEN $
                           stbl_range_count(i,m,N_ELEMENTS(where(rangepts ge percentiles(m)))-1)=$
                           stbl_range_count(i,m,N_ELEMENTS(where(rangepts ge percentiles(m)))-1)+1
                     ENDFOR
                  ENDIF
               ENDFOR
            ENDIF
         ENDFOR
      ENDFOR
      FOR m=0,n_percentiles-1 DO BEGIN
         ctl_range_count(i,m,*)=ctl_range_count(i,m,*)/FLOAT(ctl_total_valid_days(m))
         stbl_range_count(i,m,*)=stbl_range_count(i,m,*)/FLOAT(stbl_total_valid_days(m))
      ENDFOR
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_range.higem_ctl.'+season_name+'_range'+STRTRIM(STRING(ranges(i)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(REFORM(mylevs_freq(*,i)))+1,/REV
      LEVS,MANUAL=REFORM(mylevs_freq(*,i))
      GSET,YMIN=0,YMAX=(ranges(i)*2+1)^2,XMIN=0,XMAX=n_percentiles
      CON,Y=indgen((ranges(i)*2+1)^2)+0.5,X=indgen(n_percentiles)+0.5,FIELD=REFORM(ctl_range_count(i,*,0:(ranges(i)*2+1)^2-1)),/BLOCK,$
          TITLE='Estimate of size of daily precip events from HiGEM CTL (150 yrs) - '+season_name+' - Queensland',$
          CB_TITLE='Fractional frequency',/NOLINES
      AXES,YVALS=indgen((ranges(i)*2+1)^2)+0.5,YLABELS=STRTRIM(STRING(indgen((ranges(i)*2+1)^2)),1),$
           YMINOR=0.5,XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),ORIENTATION=45,$
           YTITLE='Number of points other than the central point with precip >= percentile',$
           XTITLE='Percentile of daily rainfall for '+season_name+' (days >= 1 mm only) from control simulation',NDECS=1
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_range.higem_stbl.'+season_name+'_range'+STRTRIM(STRING(ranges(i)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(REFORM(mylevs_freq(*,i)))+1,/REV
      LEVS,MANUAL=REFORM(mylevs_freq(*,i))
      GSET,YMIN=0,YMAX=(ranges(i)*2+1)^2,XMIN=0,XMAX=n_percentiles
      CON,Y=indgen((ranges(i)*2+1)^2)+0.5,X=indgen(n_percentiles)+0.5,FIELD=REFORM(stbl_range_count(i,*,0:(ranges(i)*2+1)^2-1)),/BLOCK,$
          TITLE='Estimate of size of daily precip events from HiGEM 2xCO2 (31 yrs) - '+season_name+' - Queensland',$
          CB_TITLE='Fractional frequency',/NOLINES
      AXES,YVALS=indgen((ranges(i)*2+1)^2)+0.5,YLABELS=STRTRIM(STRING(indgen((ranges(i)*2+1)^2)),1),$
           YMINOR=0.5,XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),ORIENTATION=45,$
           YTITLE='Number of points other than the central point with precip >= percentile',$
           XTITLE='Percentile of daily rainfall for '+season_name+' (days >= 1 mm only) from control simulation',NDECS=1
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip_percentiles/qld_higem_dblco2_precip_percentiles_range.higem_stbl-minus-higem_ctl.'+season_name+'_range'+STRTRIM(STRING(ranges(i)),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE2=2000,XOFFSET=500,YOFFSET=1500,TFONT=2,$
             TCHARSIZE=90,CB_WIDTH=100,/PORTRAIT,XSIZE=17000,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV
      LEVS,MANUAL=mylevs_diff
      GSET,YMIN=0,YMAX=(ranges(i)*2+1)^2,XMIN=0,XMAX=n_percentiles
      CON,Y=indgen((ranges(i)*2+1)^2)+0.5,X=indgen(n_percentiles)+0.5,$
          FIELD=REFORM(stbl_range_count(i,*,0:(ranges(i)*2+1)^2-1))-REFORM(ctl_range_count(i,*,0:(ranges(i)*2+1)^2-1)),/BLOCK,$
          TITLE='Diff in freq of size of daily precip events for HiGEM 2xCO2 minus CTL - '+season_name+' - Queensland',$
          CB_TITLE='Difference in fractional frequency',/NOLINES
      AXES,YVALS=indgen((ranges(i)*2+1)^2)+0.5,YLABELS=STRTRIM(STRING(indgen((ranges(i)*2+1)^2)),1),$
           YMINOR=0.5,XVALS=indgen(n_percentiles)+0.5,XLABELS=STRMID(STRTRIM(STRING(percentiles),1),0,4),ORIENTATION=45,$
           YTITLE='Number of points other than the central point with precip >= percentile',$
           XTITLE='Percentile of daily rainfall for '+season_name+' (days >= 1 mm only) from control simulation',NDECS=1
      PSCLOSE,/NOVIEW      
   ENDFOR
ENDFOR

STOP
END
