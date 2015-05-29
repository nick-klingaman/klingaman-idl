PRO qld_mjo_rmm_indices

; Test out some indices for MJO activity based on the Wheeler/Hendon
; RMM1/RMM2 values.

period='nov-apr'
type='smean'
n_days_per_year=182

; File containing historical MJO RMM1/RMM2/phase/amplitude values
rmm_infile='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.'+period+'_dmeans.1975-2009.index_values.nc'
start_year=1979
stop_year=2007
n_years=(stop_year-start_year)+1

; File containing SILO rainfall
silo_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_precip.'+period+'_'+type+'s.1900-2008.0.25x0.25.qld_region_aavg.nc'
;silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
;silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
;qld_box=[-10,138,-29,154]
;DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,qld_box_tx,/LIMIT
;silo_nlon=N_ELEMENTS(silo_longitude)
;silo_nlat=N_ELEMENTS(silo_latitude)
;silo_weights=fltarr(silo_nlat)
;FOR i=0,silo_nlat-1 DO $
;   silo_weights(i)=COS(silo_latitude(i)*3.14159/180.)
;silo_weights=silo_weights/TOTAL(silo_weights)
silo_time_offset=start_year-1900
silo_nyears=n_years

; Land/sea mask
;mask_infile='/home/ss901165/datasets_mango/SILO/one_quarter_lsm.nc'
;mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
;mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
;DEFINE_BOUNDARIES,qld_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
;mask_nlon=N_ELEMENTS(mask_longitude)
;mask_nlat=N_ELEMENTS(mask_latitude)

; SILO region region_mask
;region_mask_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO_qld_region_mask.0.25x0.25.may-apr.nc'
;region_mask_longitude=OPEN_AND_EXTRACT(region_mask_infile,'longitude')
;region_mask_latitude=OPEN_AND_EXTRACT(region_mask_infile,'latitude')
;DEFINE_BOUNDARIES,qld_box,region_mask_latitude,region_mask_longitude,region_mask_box_tx,/LIMIT
;region_mask_nlon=N_ELEMENTS(region_mask_longitude)
;region_mask_nlat=N_ELEMENTS(region_mask_latitude)

; Read amplitude for all days, all years
rmm_amplitude=REFORM(OPEN_AND_EXTRACT(rmm_infile,'amplitude',$
                                      offset=[start_year-1975,0],count=[n_years,n_days_per_year]))
rmm_phase=REFORM(OPEN_AND_EXTRACT(rmm_infile,'phase',$
                                  offset=[start_year-1975,0],count=[n_years,n_days_per_year]))
rmm_amplitude[where(rmm_amplitude eq -9999)]=!Values.F_NaN
rmm_phase[where(rmm_phase eq -9999)]=!Values.F_NaN

n_phases=8
n_regions=5

rmm_amplitude_stddev=fltarr(n_years)
rmm_amplitude_stddev_byphase=fltarr(n_phases,n_years)
rmm_amplitude_nstrongdays=fltarr(n_years)
rmm_amplitude_stddev_strongdays=fltarr(n_years)
rmm_amplitude_stddev_strongdays_byphase=fltarr(n_phases,n_years)
rmm_amplitude_mean=fltarr(n_years)
rmm_amplitude_mean_strongdays=fltarr(n_years)
rmm_amplitude_mean_byphase=fltarr(n_phases,n_years)
rmm_amplitude_mean_strongdays_byphase=fltarr(n_phases,n_years)
rmm_amplitude_nvaliddays=fltarr(n_years)
rmm_phase_freq=fltarr(n_phases,n_years)
rmm_phase_freq_strong=fltarr(n_phases,n_years)
FOR i=0,n_years-1 DO BEGIN
   rmm_amplitude_thisyear=REFORM(rmm_amplitude(i,*))
   rmm_phase_thisyear=REFORM(rmm_phase(i,*))
   rmm_amplitude_stddev(i)=STDDEV(rmm_amplitude_thisyear,/NaN)
   rmm_amplitude_nstrongdays(i)=N_ELEMENTS(where(rmm_amplitude_thisyear ge 1))
   rmm_amplitude_stddev_strongdays(i)=STDDEV(rmm_amplitude_thisyear[where(rmm_amplitude_thisyear ge 1)],/NaN)
   rmm_amplitude_nvaliddays(i)=N_ELEMENTS(where(FINITE(rmm_amplitude_thisyear) eq 1))   
   rmm_amplitude_mean(i)=MEAN(rmm_amplitude_thisyear,/NaN)
   rmm_amplitude_mean_strongdays(i)=MEAN(rmm_amplitude_thisyear[where(rmm_amplitude_thisyear ge 1)],/NaN)
   FOR j=0,n_phases-1 DO BEGIN
      rmm_phase_freq(j,i)=N_ELEMENTS(where(rmm_phase_thisyear eq j+1))
      rmm_phase_freq_strong(j,i)=N_ELEMENTS(where(rmm_phase_thisyear eq j+1 and rmm_amplitude_thisyear ge 1))
      rmm_amplitude_stddev_byphase(j,i)=STDDEV(rmm_amplitude_thisyear[where(rmm_phase_thisyear eq j+1)])
      rmm_amplitude_stddev_strongdays_byphase(j,i)=STDDEV(rmm_amplitude_thisyear[where(rmm_phase_thisyear eq j+1 and rmm_amplitude_thisyear ge 1)])
      rmm_amplitude_mean_byphase(j,i)=MEAN(rmm_amplitude_thisyear[where(rmm_phase_thisyear eq j+1)])
      rmm_amplitude_mean_strongdays_byphase(j,i)=MEAN(rmm_amplitude_thisyear[where(rmm_phase_thisyear eq j+1 and rmm_amplitude_thisyear ge 1)])      
   ENDFOR  
ENDFOR

; Read QLD rainfall, take area-weighted average
silo_rainfall_qld_aavg=REFORM(OPEN_AND_EXTRACT(silo_infile,'rain_aavg_allqld',$
                                               offset=[silo_time_offset],$
                                               count=[silo_nyears]))
;silo_mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
;                                  offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
;                                  count=[mask_nlon,mask_nlat,1,1]))

;silo_rainfall_qld_aavg=fltarr(silo_nyears)
;FOR i=0,silo_nyears-1 DO BEGIN
;   silo_thisyear_rainfall_masked=REFORM(silo_rainfall(*,*,i))
;   silo_thisyear_rainfall_masked[where(silo_mask lt 1 or silo_thisyear_rainfall_masked ge 1E20)]=!Values.F_NaN
;   FOR j=0,silo_nlon-1 DO $
;      silo_rainfall_qld_aavg(i)=silo_rainfall_qld_aavg(i)+TOTAL(REFORM(silo_thisyear_rainfall_masked(j,*))*$
;                                                                silo_weights,/NaN)*1./FLOAT(silo_nlon)
;ENDFOR

correlate_allqld_mjo_phase=fltarr(n_phases)
correlate_allqld_mjo_phase_strong=fltarr(n_phases)
correlate_allqld_mjo_amp_stddev_byphase=fltarr(n_phases)
correlate_allqld_mjo_amp_stddev_byphase_strong=fltarr(n_phases)
correlate_allqld_mjo_amp_mean_byphase=fltarr(n_phases)
correlate_allqld_mjo_amp_mean_byphase_strong=fltarr(n_phases)

correlate_regqld_mjo_phase=fltarr(n_phases,n_regions)
correlate_regqld_mjo_phase_strong=fltarr(n_phases,n_regions)
correlate_regqld_mjo_amp_stddev=fltarr(n_regions)
correlate_regqld_mjo_amp_stddev_strong=fltarr(n_regions)
correlate_regqld_mjo_amp_stddev_byphase=fltarr(n_phases,n_regions)
correlate_regqld_mjo_amp_stddev_byphase_strong=fltarr(n_phases,n_regions)
correlate_regqld_mjo_amp_mean_byphase=fltarr(n_phases,n_regions)
correlate_regqld_mjo_amp_mean_byphase_strong=fltarr(n_phases,n_regions)
correlate_regqld_mjo_amp_mean=fltarr(n_regions)
correlate_regqld_mjo_amp_mean_strong=fltarr(n_regions)

silo_rainfall_region_aavg=fltarr(n_regions,silo_nyears)
FOR j=0,n_regions-1 DO BEGIN
;   silo_region_mask=REFORM(OPEN_AND_EXTRACT(region_mask_infile,'mask_region'+STRTRIM(STRING(j+1),1),$
;                                            offset=[region_mask_box_tx(1),region_mask_box_tx(0)],$
;                                            count=[region_mask_nlon,region_mask_nlat]))
;   FOR k=0,silo_nyears-1 DO BEGIN
;      silo_thisyear_rainfall_masked=REFORM(silo_rainfall(*,*,k))
;      silo_thisyear_rainfall_masked[where(silo_region_mask lt 1 or silo_thisyear_rainfall_masked ge 1E20)]=!Values.F_NaN
;      FOR m=0,silo_nlon-1 DO $
;         silo_rainfall_region_aavg(j,k)=silo_rainfall_region_aavg(j,k)+TOTAL(REFORM(silo_thisyear_rainfall_masked(m,*))*$
;                                                                           silo_weights,/NaN)*1./FLOAT(silo_nlon)
;   ENDFOR
   silo_rainfall_region_aavg(j,*)=REFORM(OPEN_AND_EXTRACT(silo_infile,'rain_aavg_region',$
                                                          offset=[j,silo_time_offset],count=[1,silo_nyears]))
   FOR i=0,n_phases-1 DO BEGIN
      correlate_regqld_mjo_phase(i,j)=CORRELATE($
                                      REFORM(rmm_phase_freq(i,*)),$
                                      REFORM(silo_rainfall_region_aavg(j,*)))
      correlate_regqld_mjo_phase_strong(i,j)=CORRELATE($
                                             REFORM(rmm_phase_freq_strong(i,*)),$
                                             REFORM(silo_rainfall_region_aavg(j,*)))
      correlate_regqld_mjo_amp_stddev_byphase(i,j)=CORRELATE($
         REFORM(rmm_amplitude_stddev_byphase(i,*)),$
         REFORM(silo_rainfall_region_aavg(j,*)))
      correlate_regqld_mjo_amp_stddev_byphase_strong(i,j)=CORRELATE($
         REFORM(rmm_amplitude_stddev_strongdays_byphase(i,*)),$
         REFORM(silo_rainfall_region_aavg(j,*)))
      correlate_regqld_mjo_amp_mean_byphase(i,j)=CORRELATE($
         REFORM(rmm_amplitude_mean_byphase(i,*)),$
         REFORM(silo_rainfall_region_aavg(j,*)))
      correlate_regqld_mjo_amp_mean_byphase_strong(i,j)=CORRELATE($
         REFORM(rmm_amplitude_mean_strongdays_byphase(i,*)),$
         REFORM(silo_rainfall_region_aavg(j,*)))
   ENDFOR
   correlate_regqld_mjo_amp_mean(j)=CORRELATE(rmm_amplitude_mean,$
                                              REFORM(silo_rainfall_region_aavg(j,*)))
   correlate_regqld_mjo_amp_mean_strong(j)=CORRELATE(rmm_amplitude_mean_strongdays,$
                                                     REFORM(silo_rainfall_region_aavg(j,*)))
   correlate_regqld_mjo_amp_stddev(j)=CORRELATE(rmm_amplitude_stddev,$
                                                REFORM(silo_rainfall_region_aavg(j,*)))
   correlate_regqld_mjo_amp_stddev_strong(j)=CORRELATE(rmm_amplitude_stddev_strongdays,$
                                                       REFORM(silo_rainfall_region_aavg(j,*)))
ENDFOR
FOR i=0,n_phases-1 DO BEGIN
   correlate_allqld_mjo_phase(i)=CORRELATE(REFORM(rmm_phase_freq(i,*)),silo_rainfall_qld_aavg)
   correlate_allqld_mjo_phase_strong(i)=CORRELATE(REFORM(rmm_phase_freq_strong(i,*)),silo_rainfall_qld_aavg)
   correlate_allqld_mjo_amp_stddev_byphase(i)=CORRELATE(REFORM(rmm_amplitude_stddev_byphase(i,*)),silo_rainfall_qld_aavg)
   correlate_allqld_mjo_amp_stddev_byphase_strong(i)=CORRELATE(REFORM(rmm_amplitude_stddev_strongdays_byphase(i,*)),silo_rainfall_qld_aavg)
   correlate_allqld_mjo_amp_mean_byphase(i)=CORRELATE(REFORM(rmm_amplitude_mean_byphase(i,*)),silo_rainfall_qld_aavg)
   correlate_allqld_mjo_amp_mean_byphase_strong(i)=CORRELATE(REFORM(rmm_amplitude_mean_strongdays_byphase(i,*)),silo_rainfall_qld_aavg)
ENDFOR

psfile='/home/ss901165/idl/queensland/mjo/qld_mjo_rmm_indices.corr_'+period+'_'+type+'s_phasefreq-rain_byregion.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=0.5,XMAX=n_phases+1.5,YMIN=-0.6,YMAX=0.8,TITLE='Corr '+Period+' QLD rain with freq of MJO phase (solid) and strong MJO phase (amp > 1; dashed), 1979-2008'
CS,SCALE=26,NCOLS=n_regions+2,/REV
black=FSC_COLOR("black",2)
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_phase,COL=3,THICK=150,SYM=6
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_phase_strong,COL=3,THICK=150,STYLE=2,SYM=4
GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_nstrongdays,silo_rainfall_qld_aavg),COL=3,THICK=150,SYM=4
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(-0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0,n_phases+2),COL=2,THICK=100,STYLE=1
FOR i=0,n_regions-1 DO BEGIN
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_phase(*,i)),COL=4+i,THICK=150,SYM=6
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_phase_strong(*,i)),COL=4+i,THICK=150,STYLE=2,SYM=4
   GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_nstrongdays,REFORM(silo_rainfall_region_aavg(i,*))),COL=4+i,THICK=150,SYM=4,/NOLINES
ENDFOR
AXES,XVALS=[1,2,3,4,5,6,7,8,9],YSTEP=0.1,XTITLE='MJO phase (RMM indices, WH04)',YTITLE='Correlation coefficient',NDECS=2,$
     XLABELS=['1','2','3','4','5','6','7','8','All']
items=['Reg 5 (SE QLD)',$
       'Reg 4 (SW QLD)',$
       'Reg 3 (Coastal)',$
       'Reg 2 (NW QLD)',$
       'Reg 1 (N Penin)',$
       'All Queensland']
LEGEND,labels=items,COL=REVERSE(indgen(n_regions+1)+3),LEGPOS=9
PSCLOSE

psfile='/home/ss901165/idl/queensland/mjo/qld_mjo_rmm_indices.corr_'+period+'_'+type+'s_ampstddev-rain_byregion.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=0.5,XMAX=n_phases+1.5,YMIN=-0.8,YMAX=0.8,TITLE='Corr '+Period+' QLD rain with stddev of MJO amp (solid) and of MJO amp on days with amp > 1 (dashed), 1979-2008'
CS,SCALE=26,NCOLS=n_regions+2,/REV
black=FSC_COLOR("black",2)
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_amp_stddev_byphase,COL=3,THICK=150,SYM=6
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_amp_stddev_byphase_strong,COL=3,THICK=150,STYLE=2,SYM=4
GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_stddev,silo_rainfall_qld_aavg),COL=3,THICK=150,SYM=6
GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_stddev_strongdays,silo_rainfall_qld_aavg),COL=3,THICK=150,SYM=4
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(-0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0,n_phases+2),COL=2,THICK=100,STYLE=1
FOR i=0,n_regions-1 DO BEGIN
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_amp_stddev_byphase(*,i)),COL=4+i,THICK=150,SYM=6
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_amp_stddev_byphase_strong(*,i)),COL=4+i,THICK=150,STYLE=2,SYM=4
   GPLOT,X=n_phases+1,Y=correlate_regqld_mjo_amp_stddev(i),COL=4+i,THICK=150,SYM=6,/NOLINES
   GPLOT,X=n_phases+1,Y=correlate_regqld_mjo_amp_stddev_strong(i),COL=4+i,THICK=150,SYM=4,/NOLINES
ENDFOR
AXES,XVALS=[1,2,3,4,5,6,7,8,9],YSTEP=0.1,XTITLE='MJO phase (RMM indices, WH04)',YTITLE='Correlation coefficient',NDECS=2,$
     XLABELS=['1','2','3','4','5','6','7','8','All']
items=['Reg 5 (SE QLD)',$
       'Reg 4 (SW QLD)',$
       'Reg 3 (Coastal)',$
       'Reg 2 (NW QLD)',$
       'Reg 1 (N Penin)',$
       'All Queensland']
LEGEND,labels=items,COL=REVERSE(indgen(n_regions+1)+3),LEGPOS=9
PSCLOSE

psfile='/home/ss901165/idl/queensland/mjo/qld_mjo_rmm_indices.corr_'+period+'_'+type+'s_ampmean-rain_byregion.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=0.5,XMAX=n_phases+1.5,YMIN=-0.8,YMAX=0.8,TITLE='Corr '+Period+' QLD rain with mean of MJO amp (solid) and of MJO amp on days with amp > 1 (dashed), 1979-2008'
CS,SCALE=26,NCOLS=n_regions+2,/REV
black=FSC_COLOR("black",2)
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_amp_mean_byphase,COL=3,THICK=150,SYM=6
GPLOT,X=indgen(n_phases)+1,Y=correlate_allqld_mjo_amp_mean_byphase_strong,COL=3,THICK=150,STYLE=2,SYM=4
GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_mean,silo_rainfall_qld_aavg),COL=3,THICK=150,SYM=6
GPLOT,X=n_phases+1,Y=CORRELATE(rmm_amplitude_mean_strongdays,silo_rainfall_qld_aavg),COL=3,THICK=150,SYM=4
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(-0.367,n_phases+2),COL=2,THICK=100,STYLE=1
GPLOT,X=indgen(n_phases+2)+0.5,Y=REPLICATE(0,n_phases+2),COL=2,THICK=100,STYLE=1
FOR i=0,n_regions-1 DO BEGIN
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_amp_mean_byphase(*,i)),COL=4+i,THICK=150,SYM=6
   GPLOT,X=indgen(n_phases)+1,Y=REFORM(correlate_regqld_mjo_amp_mean_byphase_strong(*,i)),COL=4+i,THICK=150,STYLE=2,SYM=4
   GPLOT,X=n_phases+1,Y=correlate_regqld_mjo_amp_mean(i),COL=4+i,THICK=150,SYM=6,/NOLINES
   GPLOT,X=n_phases+1,Y=correlate_regqld_mjo_amp_mean_strong(i),COL=4+i,THICK=15,SYM=4,/NOLINES
ENDFOR
AXES,XVALS=[1,2,3,4,5,6,7,8,9],YSTEP=0.1,XTITLE='MJO phase (RMM indices, WH04)',YTITLE='Correlation coefficient',NDECS=2,$
     XLABELS=['1','2','3','4','5','6','7','8','All']
items=['Reg 5 (SE QLD)',$
       'Reg 4 (SW QLD)',$
       'Reg 3 (Coastal)',$
       'Reg 2 (NW QLD)',$
       'Reg 1 (N Penin)',$
       'All Queensland']
LEGEND,labels=items,COL=REVERSE(indgen(n_regions+1)+3),LEGPOS=9
PSCLOSE

STOP

END

