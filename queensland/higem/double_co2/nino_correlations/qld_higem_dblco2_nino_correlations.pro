PRO qld_higem_dblco2_nino_correlations

; Timeseries of NINO indices from HiGEM (monthly)
higem_eadwu_nino_infile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jan-dec_mmeans.o2-r3.nino_indices.nc'
higem_eafee_nino_infile='/home/ss901165/higem_qccce/es_2pctco2_eafee/higem_eafee.jan-dec_mmeans.k9-u7.nino_indices.nc'

; Timeseries of Australian rainfall from HiGEM (monthly)
higem_eadwu_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
higem_eafee_indir='/home/ss901165/higem_qccce/es_2pctco2_eafee'
higem_eadwu_nyears=32
higem_eafee_nyears=59

; To splice eafee (transient) and eadwu (fixed 2xCO2)
higem_eafee_splice_offset=20 ; Where to start first segment (as offset)
higem_eafee_splice_split=31  ; Where to stop first segment (as offset)
higem_eafee_splice_nyears=20 ; Total number of years

higem_splice_nyears=higem_eadwu_nyears+higem_eafee_splice_nyears

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

; Box to plot
box_plot=[-44,112,-10,154]
box_read=[-45,110,-10,155]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
months_long=['January','February','March','April','May','June','July','August','September','October','November','December']
n_months=12

seasons=['mar-may','jun-aug','sep-nov','dec-feb']
seasons_long=['MAM','JJA','SON','DJF']
n_seasons=4

mylevs=['-0.375','-0.325','-0.275','-0.225','-0.175','-0.125','-0.075','-0.025','0.025','0.075','0.125','0.175','0.225','0.275','0.325','0.375']
mylevs_annual=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
mylevs_seasonal=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55']

; Get NINO timeseries from HiGEM, use 13th position for annual-mean value
higem_eadwu_nino3_ts=OPEN_AND_EXTRACT(higem_eadwu_nino_infile,'nino3')
higem_eadwu_nino4_ts=OPEN_AND_EXTRACT(higem_eadwu_nino_infile,'nino4')
higem_eadwu_nino34_ts=OPEN_AND_EXTRACT(higem_eadwu_nino_infile,'nino34')
higem_eafee_nino3_ts=OPEN_AND_EXTRACT(higem_eafee_nino_infile,'nino3')
higem_eafee_nino4_ts=OPEN_AND_EXTRACT(higem_eafee_nino_infile,'nino4')
higem_eafee_nino34_ts=OPEN_AND_EXTRACT(higem_eafee_nino_infile,'nino34')
higem_eadwu_nino3_ts_bymonth=fltarr(13,higem_eadwu_nyears)
higem_eadwu_nino4_ts_bymonth=fltarr(13,higem_eadwu_nyears)
higem_eadwu_nino34_ts_bymonth=fltarr(13,higem_eadwu_nyears)
higem_eafee_nino3_ts_bymonth=fltarr(13,higem_eafee_nyears)
higem_eafee_nino4_ts_bymonth=fltarr(13,higem_eafee_nyears)
higem_eafee_nino34_ts_bymonth=fltarr(13,higem_eafee_nyears)
higem_eafee_nino3_ts_byseason=fltarr(4,higem_eafee_nyears)
higem_eafee_nino4_ts_byseason=fltarr(4,higem_eafee_nyears)
higem_eafee_nino34_ts_byseason=fltarr(4,higem_eafee_nyears)
higem_eadwu_nino3_ts_byseason=fltarr(4,higem_eadwu_nyears)
higem_eadwu_nino4_ts_byseason=fltarr(4,higem_eadwu_nyears)
higem_eadwu_nino34_ts_byseason=fltarr(4,higem_eadwu_nyears)

higem_splice_nino3_ts_bymonth=fltarr(13,higem_splice_nyears)
higem_splice_nino4_ts_bymonth=fltarr(13,higem_splice_nyears)
higem_splice_nino34_ts_bymonth=fltarr(13,higem_splice_nyears)
higem_splice_nino3_ts_byseason=fltarr(4,higem_splice_nyears)
higem_splice_nino4_ts_byseason=fltarr(4,higem_splice_nyears)
higem_splice_nino34_ts_byseason=fltarr(4,higem_splice_nyears)

FOR i=0,higem_eadwu_nyears-1 DO BEGIN
   higem_eadwu_nino3_ts_bymonth(0:11,i)=higem_eadwu_nino3_ts(i*12:(i+1)*12-1)   
   higem_eadwu_nino4_ts_bymonth(0:11,i)=higem_eadwu_nino4_ts(i*12:(i+1)*12-1)  
   higem_eadwu_nino34_ts_bymonth(0:11,i)=higem_eadwu_nino34_ts(i*12:(i+1)*12-1)
   IF i ne higem_eadwu_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO BEGIN
         higem_eadwu_nino3_ts_byseason(j,i)=MEAN(higem_eadwu_nino3_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eadwu_nino4_ts_byseason(j,i)=MEAN(higem_eadwu_nino4_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eadwu_nino34_ts_byseason(j,i)=MEAN(higem_eadwu_nino34_ts(i*12+j*3+2:i*12+(j+1)*3+1))
      ENDFOR
   ENDIF
ENDFOR
FOR i=0,higem_eadwu_nyears-2 DO BEGIN
   higem_eadwu_nino3_ts_bymonth(12,i)=MEAN(higem_eadwu_nino3_ts(i*12+4:(i+1)*12+3))
   higem_eadwu_nino4_ts_bymonth(12,i)=MEAN(higem_eadwu_nino4_ts(i*12+4:(i+1)*12+3))
   higem_eadwu_nino34_ts_bymonth(12,i)=MEAN(higem_eadwu_nino34_ts(i*12+4:(i+1)*12+3)) 
ENDFOR

FOR i=0,higem_eafee_nyears-1 DO BEGIN
   higem_eafee_nino3_ts_bymonth(0:11,i)=higem_eafee_nino3_ts(i*12:(i+1)*12-1)
   higem_eafee_nino4_ts_bymonth(0:11,i)=higem_eafee_nino4_ts(i*12:(i+1)*12-1)
   higem_eafee_nino34_ts_bymonth(0:11,i)=higem_eafee_nino34_ts(i*12:(i+1)*12-1)
   IF i ne higem_eafee_nyears-1 THEN BEGIN
      FOR j=0,n_seasons-1 DO BEGIN
         higem_eafee_nino3_ts_byseason(j,i)=MEAN(higem_eafee_nino3_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eafee_nino4_ts_byseason(j,i)=MEAN(higem_eafee_nino4_ts(i*12+j*3+2:i*12+(j+1)*3+1))
         higem_eafee_nino34_ts_byseason(j,i)=MEAN(higem_eafee_nino34_ts(i*12+j*3+2:i*12+(j+1)*3+1))
      ENDFOR
   ENDIF
ENDFOR
FOR i=0,higem_eafee_nyears-2 DO BEGIN
   higem_eafee_nino3_ts_bymonth(12,i)=MEAN(higem_eafee_nino3_ts(i*12+4:(i+1)*12+3))
   higem_eafee_nino4_ts_bymonth(12,i)=MEAN(higem_eafee_nino4_ts(i*12+4:(i+1)*12+3))
   higem_eafee_nino34_ts_bymonth(12,i)=MEAN(higem_eafee_nino34_ts(i*12+4:(i+1)*12+3))   
ENDFOR

FOR i=0,n_months-1 DO BEGIN
   temp=[REFORM(higem_eafee_nino3_ts_bymonth(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino3_ts_bymonth(i,*)),$
         REFORM(higem_eafee_nino3_ts_bymonth(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears),temp)
   higem_splice_nino3_ts_bymonth(i,*)=temp-indgen(higem_splice_nyears)*splice_trend(0)

   temp=[REFORM(higem_eafee_nino34_ts_bymonth(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino34_ts_bymonth(i,*)),$
         REFORM(higem_eafee_nino34_ts_bymonth(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears),temp)
   higem_splice_nino34_ts_bymonth(i,*)=temp-indgen(higem_splice_nyears)*splice_trend(0)

   temp=[REFORM(higem_eafee_nino4_ts_bymonth(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino4_ts_bymonth(i,*)),$
         REFORM(higem_eafee_nino4_ts_bymonth(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears),temp)
   higem_splice_nino4_ts_bymonth(i,*)=temp-indgen(higem_splice_nyears)*splice_trend(0)
ENDFOR
temp=[REFORM(higem_eafee_nino3_ts_bymonth(12,higem_eafee_splice_offset:higem_eafee_splice_split)),$
      REFORM(higem_eadwu_nino3_ts_bymonth(12,0:higem_eadwu_nyears-2)),$
      REFORM(higem_eafee_nino3_ts_bymonth(12,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
higem_splice_nino3_ts_bymonth(12,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears-1)*splice_trend(0)

temp=[REFORM(higem_eafee_nino34_ts_bymonth(12,higem_eafee_splice_offset:higem_eafee_splice_split)),$
      REFORM(higem_eadwu_nino34_ts_bymonth(12,0:higem_eadwu_nyears-2)),$
      REFORM(higem_eafee_nino34_ts_bymonth(12,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
higem_splice_nino34_ts_bymonth(12,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears-1)*splice_trend(0)

temp=[REFORM(higem_eafee_nino4_ts_bymonth(12,higem_eafee_splice_offset:higem_eafee_splice_split)),$
      REFORM(higem_eadwu_nino4_ts_bymonth(12,0:higem_eadwu_nyears-2)),$
      REFORM(higem_eafee_nino4_ts_bymonth(12,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
higem_splice_nino4_ts_bymonth(12,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears-1)*splice_trend(0)

FOR i=0,n_seasons-1 DO BEGIN
   temp=[REFORM(higem_eafee_nino3_ts_byseason(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino3_ts_byseason(i,0:higem_eadwu_nyears-2)),$
         REFORM(higem_eafee_nino3_ts_byseason(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
   higem_splice_nino3_ts_byseason(i,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears)*splice_trend(0)

   temp=[REFORM(higem_eafee_nino34_ts_byseason(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino34_ts_byseason(i,0:higem_eadwu_nyears-2)),$
         REFORM(higem_eafee_nino34_ts_byseason(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
   higem_splice_nino34_ts_byseason(i,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears)*splice_trend(0)

   temp=[REFORM(higem_eafee_nino4_ts_byseason(i,higem_eafee_splice_offset:higem_eafee_splice_split)),$
         REFORM(higem_eadwu_nino4_ts_byseason(i,0:higem_eadwu_nyears-2)),$
         REFORM(higem_eafee_nino4_ts_byseason(i,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
   splice_trend=REGRESS(indgen(higem_splice_nyears-1),temp)
   higem_splice_nino4_ts_byseason(i,0:higem_splice_nyears-2)=temp-indgen(higem_splice_nyears)*splice_trend(0)
ENDFOR

FOR i=0,n_months-1 DO BEGIN
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears),REFORM(higem_eadwu_nino3_ts_bymonth(i,*)))
   higem_eadwu_nino3_ts_bymonth(i,*)=higem_eadwu_nino3_ts_bymonth(i,*)-indgen(higem_eadwu_nyears)*eadwu_trend(0)
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears),REFORM(higem_eadwu_nino34_ts_bymonth(i,*)))
   higem_eadwu_nino34_ts_bymonth(i,*)=higem_eadwu_nino34_ts_bymonth(i,*)-indgen(higem_eadwu_nyears)*eadwu_trend(0)
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears),REFORM(higem_eadwu_nino4_ts_bymonth(i,*)))
   higem_eadwu_nino4_ts_bymonth(i,*)=higem_eadwu_nino4_ts_bymonth(i,*)-indgen(higem_eadwu_nyears)*eadwu_trend(0)

   eafee_trend=REGRESS(indgen(higem_eafee_nyears),REFORM(higem_eafee_nino3_ts_bymonth(i,*)))
   higem_eafee_nino3_ts_bymonth(i,*)=higem_eafee_nino3_ts_bymonth(i,*)-indgen(higem_eafee_nyears)*eafee_trend(0)
   eafee_trend=REGRESS(indgen(higem_eafee_nyears),REFORM(higem_eafee_nino34_ts_bymonth(i,*)))
   higem_eafee_nino34_ts_bymonth(i,*)=higem_eafee_nino34_ts_bymonth(i,*)-indgen(higem_eafee_nyears)*eafee_trend(0)
   eafee_trend=REGRESS(indgen(higem_eafee_nyears),REFORM(higem_eafee_nino4_ts_bymonth(i,*)))
   higem_eafee_nino4_ts_bymonth(i,*)=higem_eafee_nino4_ts_bymonth(i,*)-indgen(higem_eafee_nyears)*eafee_trend(0)
ENDFOR
eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino3_ts_bymonth(12,0:higem_eadwu_nyears-2)))
higem_eadwu_nino3_ts_bymonth(12,0:higem_eadwu_nyears-2)=higem_eadwu_nino3_ts_bymonth(12,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)
eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino34_ts_bymonth(12,0:higem_eadwu_nyears-2)))
higem_eadwu_nino34_ts_bymonth(12,0:higem_eadwu_nyears-2)=higem_eadwu_nino34_ts_bymonth(12,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)
eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino4_ts_bymonth(12,0:higem_eadwu_nyears-2)))
higem_eadwu_nino4_ts_bymonth(12,0:higem_eadwu_nyears-2)=higem_eadwu_nino4_ts_bymonth(12,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)

eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino3_ts_bymonth(12,0:higem_eafee_nyears-2)))
higem_eafee_nino3_ts_bymonth(12,0:higem_eafee_nyears-2)=higem_eafee_nino3_ts_bymonth(12,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)
eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino34_ts_bymonth(12,0:higem_eafee_nyears-2)))
higem_eafee_nino34_ts_bymonth(12,0:higem_eafee_nyears-2)=higem_eafee_nino34_ts_bymonth(12,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)
eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino4_ts_bymonth(12,0:higem_eafee_nyears-2)))
higem_eafee_nino4_ts_bymonth(12,0:higem_eafee_nyears-2)=higem_eafee_nino4_ts_bymonth(12,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)

FOR i=0,n_seasons-1 DO BEGIN
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino3_ts_byseason(i,0:higem_eadwu_nyears-2)))
   higem_eadwu_nino3_ts_byseason(i,0:higem_eadwu_nyears-2)=higem_eadwu_nino3_ts_byseason(i,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino34_ts_byseason(i,0:higem_eadwu_nyears-2)))
   higem_eadwu_nino34_ts_byseason(i,0:higem_eadwu_nyears-2)=higem_eadwu_nino34_ts_byseason(i,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)
   eadwu_trend=REGRESS(indgen(higem_eadwu_nyears-1),REFORM(higem_eadwu_nino4_ts_byseason(i,0:higem_eadwu_nyears-2)))
   higem_eadwu_nino4_ts_byseason(i,0:higem_eadwu_nyears-2)=higem_eadwu_nino4_ts_byseason(i,0:higem_eadwu_nyears-2)-indgen(higem_eadwu_nyears-1)*eadwu_trend(0)

   eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino3_ts_byseason(i,0:higem_eafee_nyears-2)))
   higem_eafee_nino3_ts_byseason(i,0:higem_eafee_nyears-2)=higem_eafee_nino3_ts_byseason(i,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)
   eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino34_ts_byseason(i,0:higem_eafee_nyears-2)))
   higem_eafee_nino34_ts_byseason(i,0:higem_eafee_nyears-2)=higem_eafee_nino34_ts_byseason(i,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)
   eafee_trend=REGRESS(indgen(higem_eafee_nyears-1),REFORM(higem_eafee_nino4_ts_byseason(i,0:higem_eafee_nyears-2)))
   higem_eafee_nino4_ts_byseason(i,0:higem_eafee_nyears-2)=higem_eafee_nino4_ts_byseason(i,0:higem_eafee_nyears-2)-indgen(higem_eafee_nyears-1)*eafee_trend(0)
ENDFOR

FOR i=0,n_months-1 DO BEGIN
   higem_eadwu_infile=higem_eadwu_indir+'/higem_eadwu.'+months(i)+'_mmeans.o2-r3.precip.global_domain.nc'
   higem_eafee_infile=higem_eafee_indir+'/higem_eafee.'+months(i)+'_mmeans.k9-u7.precip.global_domain.nc'
   IF i eq 0 THEN BEGIN
      higem_latitude=OPEN_AND_EXTRACT(higem_eadwu_infile,'latitude')
      higem_longitude=OPEN_AND_EXTRACT(higem_eadwu_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,higem_latitude,higem_longitude,higem_box_tx,/LIMIT
      higem_nlon=N_ELEMENTS(higem_longitude)
      higem_nlat=N_ELEMENTS(higem_latitude)
      
      mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
      mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
      DEFINE_BOUNDARIES,box_read,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
      mask_nlon=N_ELEMENTS(mask_longitude)
      mask_nlat=N_ELEMENTS(mask_latitude)
      mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                   offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                   count=[mask_nlon,mask_nlat,1,1]))
   ENDIF

   higem_eadwu_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eadwu_infile,'precip',$
                                              offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                    count=[higem_nlon,higem_nlat,1,higem_eadwu_nyears,1]))*86400.
   higem_eafee_mmean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafee_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                                    count=[higem_nlon,higem_nlat,1,higem_eafee_nyears,1]))*86400.

   higem_splice_mmean_precip=fltarr(higem_nlon,higem_nlat,higem_splice_nyears)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         temp=[REFORM(higem_eafee_mmean_precip(j,k,higem_eafee_splice_offset:higem_eafee_splice_split)),$
               REFORM(higem_eadwu_mmean_precip(j,k,*)),$
               REFORM(higem_eafee_mmean_precip(j,k,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
         trend=REGRESS(indgen(higem_splice_nyears),temp)
         higem_splice_mmean_precip(j,k,*)=temp-trend(0)*indgen(higem_splice_nyears)
;         higem_splice_mmean_precip(j,k,*)=temp
      ENDFOR
   ENDFOR

   higem_eadwu_nino3_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eadwu_nino4_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eadwu_nino34_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafee_nino3_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafee_nino4_correlation=fltarr(higem_nlon,higem_nlat)
   higem_eafee_nino34_correlation=fltarr(higem_nlon,higem_nlat)   
   higem_splice_nino3_correlation=fltarr(higem_nlon,higem_nlat)
   higem_splice_nino4_correlation=fltarr(higem_nlon,higem_nlat)
   higem_splice_nino34_correlation=fltarr(higem_nlon,higem_nlat)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            higem_eadwu_nino3_correlation(j,k)=CORRELATE(higem_eadwu_mmean_precip(j,k,*),higem_eadwu_nino3_ts_bymonth(i,*))
            higem_eadwu_nino4_correlation(j,k)=CORRELATE(higem_eadwu_mmean_precip(j,k,*),higem_eadwu_nino4_ts_bymonth(i,*))
            higem_eadwu_nino34_correlation(j,k)=CORRELATE(higem_eadwu_mmean_precip(j,k,*),higem_eadwu_nino34_ts_bymonth(i,*))

            higem_eafee_nino3_correlation(j,k)=CORRELATE(higem_eafee_mmean_precip(j,k,*),higem_eafee_nino3_ts_bymonth(i,*))
            higem_eafee_nino4_correlation(j,k)=CORRELATE(higem_eafee_mmean_precip(j,k,*),higem_eafee_nino4_ts_bymonth(i,*))
            higem_eafee_nino34_correlation(j,k)=CORRELATE(higem_eafee_mmean_precip(j,k,*),higem_eafee_nino34_ts_bymonth(i,*))
                        
            higem_splice_nino3_correlation(j,k)=CORRELATE(higem_splice_mmean_precip(j,k,*),higem_splice_nino3_ts_bymonth(i,*))
            higem_splice_nino4_correlation(j,k)=CORRELATE(higem_splice_mmean_precip(j,k,*),higem_splice_nino4_ts_bymonth(i,*))
            higem_splice_nino34_correlation(j,k)=CORRELATE(higem_splice_mmean_precip(j,k,*),higem_splice_nino34_ts_bymonth(i,*))
         ENDIF ELSE BEGIN
            higem_eadwu_nino3_correlation(j,k)=!Values.F_NaN
            higem_eadwu_nino4_correlation(j,k)=!Values.F_NaN
            higem_eadwu_nino34_correlation(j,k)=!Values.F_NaN
            higem_eafee_nino3_correlation(j,k)=!Values.F_NaN
            higem_eafee_nino4_correlation(j,k)=!Values.F_NaN
            higem_eafee_nino34_correlation(j,k)=!Values.F_NaN            
            higem_splice_nino3_correlation(j,k)=!Values.F_NaN
            higem_splice_nino4_correlation(j,k)=!Values.F_NaN
            higem_splice_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR
   print,higem_splice_nino3_correlation(20,20)

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino3.higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eadwu_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino4.higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eadwu_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO4 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino34.higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eadwu_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3.4 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino3.higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_splice_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino4.higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_splice_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO4 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino34.higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_splice_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3.4 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino3.higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafee_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino4.higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafee_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO4 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+months(i)+'_nino34.higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV,white=[10,11]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs
   CON,FIELD=higem_eafee_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=months_long(i)+' corr HiGEM NINO3.4 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=2
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR

FOR i=0,n_seasons-1 DO BEGIN
   ; Read in seasonal-mean rainfall for seasonal mean correlations
   higem_eafee_infile=higem_eafee_indir+'/higem_eafee.'+seasons(i)+'_smeans.k9-u7.precip.global_domain.nc'

   higem_eafee_smean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafee_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                                    count=[higem_nlon,higem_nlat,higem_eafee_nyears-1]))*86400.

   higem_eadwu_infile=higem_eadwu_indir+'/higem_eadwu.'+seasons(i)+'_smeans.o2-r3.precip.global_domain.nc'

   higem_eadwu_smean_precip=REFORM(OPEN_AND_EXTRACT(higem_eadwu_infile,'precip',$
                                                    offset=[higem_box_tx(1),higem_box_tx(0),0],$
                                                    count=[higem_nlon,higem_nlat,higem_eadwu_nyears-1]))*86400.
   
   higem_splice_smean_precip=fltarr(higem_nlon,higem_nlat,higem_splice_nyears-1)
   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         temp=[REFORM(higem_eafee_smean_precip(j,k,higem_eafee_splice_offset:higem_eafee_splice_split)),$
               REFORM(higem_eadwu_smean_precip(j,k,*)),$
               REFORM(higem_eafee_smean_precip(j,k,higem_eafee_splice_split+1:higem_eafee_splice_offset+higem_eafee_splice_nyears-1))]
         trend=REGRESS(indgen(higem_splice_nyears-1),temp)
         higem_splice_smean_precip(j,k,*)=temp-trend(0)*indgen(higem_splice_nyears)
;         higem_splice_smean_precip(j,k,*)=temp
      ENDFOR
   ENDFOR

   FOR j=0,higem_nlon-1 DO BEGIN
      FOR k=0,higem_nlat-1 DO BEGIN
         IF mask(j,k) eq 1 THEN BEGIN
            higem_eafee_nino3_correlation(j,k)=CORRELATE(higem_eafee_smean_precip(j,k,*),higem_eafee_nino3_ts_byseason(i,0:higem_eafee_nyears-2))
            higem_eafee_nino4_correlation(j,k)=CORRELATE(higem_eafee_smean_precip(j,k,*),higem_eafee_nino4_ts_byseason(i,0:higem_eafee_nyears-2))
            higem_eafee_nino34_correlation(j,k)=CORRELATE(higem_eafee_smean_precip(j,k,*),higem_eafee_nino34_ts_byseason(i,0:higem_eafee_nyears-2))           
            higem_eadwu_nino3_correlation(j,k)=CORRELATE(higem_eadwu_smean_precip(j,k,*),higem_eadwu_nino3_ts_byseason(i,0:higem_eadwu_nyears-2))
            higem_eadwu_nino4_correlation(j,k)=CORRELATE(higem_eadwu_smean_precip(j,k,*),higem_eadwu_nino4_ts_byseason(i,0:higem_eadwu_nyears-2))
            higem_eadwu_nino34_correlation(j,k)=CORRELATE(higem_eadwu_smean_precip(j,k,*),higem_eadwu_nino34_ts_byseason(i,0:higem_eadwu_nyears-2))                  
            higem_splice_nino3_correlation(j,k)=CORRELATE(higem_splice_smean_precip(j,k,*),higem_splice_nino3_ts_byseason(i,0:higem_splice_nyears-2))
            higem_splice_nino4_correlation(j,k)=CORRELATE(higem_splice_smean_precip(j,k,*),higem_splice_nino4_ts_byseason(i,0:higem_splice_nyears-2))
            higem_splice_nino34_correlation(j,k)=CORRELATE(higem_splice_smean_precip(j,k,*),higem_splice_nino34_ts_byseason(i,0:higem_splice_nyears-2))
         ENDIF ELSE BEGIN
            higem_eafee_nino3_correlation(j,k)=!Values.F_NaN
            higem_eafee_nino4_correlation(j,k)=!Values.F_NaN
            higem_eafee_nino34_correlation(j,k)=!Values.F_NaN
            higem_eadwu_nino3_correlation(j,k)=!Values.F_NaN
            higem_eadwu_nino4_correlation(j,k)=!Values.F_NaN
            higem_eadwu_nino34_correlation(j,k)=!Values.F_NaN            
            higem_splice_nino3_correlation(j,k)=!Values.F_NaN
            higem_splice_nino4_correlation(j,k)=!Values.F_NaN
            higem_splice_nino34_correlation(j,k)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino3_higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafee_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino4_higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafee_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N4 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'.nino34_higem_eafee.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eafee_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3.4 and rainfall (eafee, 99 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino3_higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eadwu_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino4_higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eadwu_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N4 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'.nino34_higem_eadwu.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_eadwu_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3.4 and rainfall (eadwu, 32 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino3_higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_splice_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'_smean.nino4_higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_splice_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N4 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.'+seasons(i)+'.nino34_higem_eadwu_eafee_splice.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_seasonal)+1,/REV,white=[8]
   MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
   LEVS,MANUAL=mylevs_seasonal
   CON,FIELD=higem_splice_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
       TITLE=seasons_long(i)+' corr N3.4 and rainfall (eadwu splice eafee, 52 years)',/NOLINES,$
       CB_NTH=1
   AXES,XSTEP=5,YSTEP=5
   PSCLOSE,/NOVIEW

ENDFOR
      
; Read in annual-mean rainfall for annual mean correlations

higem_eadwu_infile=higem_eadwu_indir+'/higem_eadwu.may-apr_ameans.o2-r3.precip.global_domain.nc'
higem_eafee_infile=higem_eafee_indir+'/higem_eafee.may-apr_ameans.k9-u7.precip.global_domain.nc'

;higem_eadwu_infile=higem_eadwu_indir+'/higem_eadwu.may-apr_ameans.h9-t5.precip.global_domain.nc'
;higem_eafee_infile=higem_eafee_indir+'/higem_eafee.may-apr_ameans.h9-w8.precip.global_domain.nc'
;silo_n144_infile=silo_n144_indir+'/SILO_precip.may-apr_ameans.1900-2007.n144.nc'


higem_eadwu_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_eadwu_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,higem_eadwu_nyears-1]))*86400.
higem_eafee_amean_precip=REFORM(OPEN_AND_EXTRACT(higem_eafee_infile,'precip',$
                                           offset=[higem_box_tx(1),higem_box_tx(0),0,0,0],$
                                           count=[higem_nlon,higem_nlat,higem_eafee_nyears-1]))*86400.

FOR j=0,higem_nlon-1 DO BEGIN
   FOR k=0,higem_nlat-1 DO BEGIN
      IF mask(j,k) eq 1 THEN BEGIN
         higem_eadwu_nino3_correlation(j,k)=CORRELATE(higem_eadwu_amean_precip(j,k,*),higem_eadwu_nino3_ts_bymonth(12,0:higem_eadwu_nyears-2))   
         higem_eadwu_nino4_correlation(j,k)=CORRELATE(higem_eadwu_amean_precip(j,k,*),higem_eadwu_nino4_ts_bymonth(12,0:higem_eadwu_nyears-2))
         higem_eadwu_nino34_correlation(j,k)=CORRELATE(higem_eadwu_amean_precip(j,k,*),higem_eadwu_nino34_ts_bymonth(12,0:higem_eadwu_nyears-2))

         higem_eafee_nino3_correlation(j,k)=CORRELATE(higem_eafee_amean_precip(j,k,*),higem_eafee_nino3_ts_bymonth(12,0:higem_eafee_nyears-2))   
         higem_eafee_nino4_correlation(j,k)=CORRELATE(higem_eafee_amean_precip(j,k,*),higem_eafee_nino4_ts_bymonth(12,0:higem_eafee_nyears-2))
         higem_eafee_nino34_correlation(j,k)=CORRELATE(higem_eafee_amean_precip(j,k,*),higem_eafee_nino34_ts_bymonth(12,0:higem_eafee_nyears-2))
      ENDIF ELSE BEGIN
         higem_eadwu_nino3_correlation(j,k)=!Values.F_NaN
         higem_eadwu_nino4_correlation(j,k)=!Values.F_NaN
         higem_eadwu_nino34_correlation(j,k)=!Values.F_NaN

         higem_eafee_nino3_correlation(j,k)=!Values.F_NaN
         higem_eafee_nino4_correlation(j,k)=!Values.F_NaN
         higem_eafee_nino34_correlation(j,k)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino3.higem_eadwu.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eadwu_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3 and HiGEM eadwu rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino4.higem_eadwu.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eadwu_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO4 and HiGEM eadwu rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino34.higem_eadwu.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eadwu_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3.4 and HiGEM eadwu rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW


psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino3.higem_eafee.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafee_nino3_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3 and HiGEM eafee rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino4.higem_eafee.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafee_nino4_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO4 and HiGEM eafee rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/queensland/higem/double_co2/nino_correlations/qld_higem_dblco2_nino_correlations.annmean_may-apr_nino34.higem_eafee.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1200,XOFFSET=200,YOFFSET=4000,TFONT=2,TCHARSIZE=100,CB_WIDTH=115,XSIZE=15000
CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_annual)+1,/REV,white=[9]
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
LEVS,MANUAL=mylevs_annual
CON,FIELD=higem_eafee_nino34_correlation,X=higem_longitude,Y=higem_latitude,$
    TITLE='Annual-mean (May-Apr) corr NINO3.4 and HiGEM eafee rainfall',/NOLINES,$
    CB_NTH=1
AXES,XSTEP=5,YSTEP=5
PSCLOSE,/NOVIEW

STOP

END

