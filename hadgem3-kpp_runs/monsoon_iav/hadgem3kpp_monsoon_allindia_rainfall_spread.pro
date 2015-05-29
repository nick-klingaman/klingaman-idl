PRO hadgem3kpp_monsoon_allindia_rainfall_spread
  
; Compare the evolution of all-India rainfall from the FOAM/AMIP2, the
; global FOAM, and the global AMIP2 integrations.

; File containing daily-mean rainfall from the global FOAM ensemble
foam_dmean_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74/hadgem3a_foam_ctl_vn74.mar-sep_dmeans.years1-31.precip.nc'
;rerun_dmean_infile='/home/ss901165/um_output3/hadgem3a_foam_rerun_vn74/hadgem3a_foam_rerun_vn74.mar-sep_dmeans.years1-5.precip.nc'
blended_dmean_infile='/home/ss901165/um_output3/hadgem3a_foam_control_vn74_blendsst/hadgem3a_foam_ctl_vn74_blendsst.mar-oct_dmeans.years1-35.precip.nc'
amip2_dmean_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.mar-oct_dmeans.years1-20.precip.nc'

mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'

; Box over which to area-average rainfall
box=[10,70,30,90]

; Find box in each file
longitude=OPEN_AND_EXTRACT(foam_dmean_infile,'longitude')
latitude=OPEN_AND_EXTRACT(foam_dmean_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

; Find box in mask file
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
n_valid_pts=N_ELEMENTS(where(mask eq 1))
n96_weights=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlon-1 DO BEGIN
   FOR j=0,mask_nlat-1 DO BEGIN
      IF mask(i,j) eq 1 THEN BEGIN
         n96_weights(i,j)=COS(3.14159*mask_latitude(j)/180.)/FLOAT(n_valid_pts)
      ENDIF ELSE $
         n96_weights(i,j)=0.
   ENDFOR
ENDFOR
n96_weights=n96_weights/TOTAL(n96_weights)

; Number of years for each ensemble member (necessary for selection of
; first year of each member)
foam_nyears=[7,7,7,7,3]
blended_nyears=[7,7,7,7,7]
amip2_nyears=[4,4,4,4,4]
rerun_nyears=[1,1,1,1,1]

n_days=210 ; Seven months - March to September

foam_allindia_ts=fltarr(5,n_days)
blended_allindia_ts=fltarr(5,n_days)
amip2_allindia_ts=fltarr(5,n_days)
rerun_allindia_ts=fltarr(5,n_days)

FOR i=0,2 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile=foam_dmean_infile
         nyears_per_member=foam_nyears
         initial_offset=0
      END
      1: BEGIN
         infile=blended_dmean_infile
         nyears_per_member=blended_nyears
         initial_offset=0
      END
      2: BEGIN
         infile=amip2_dmean_infile
         nyears_per_member=amip2_nyears
      END
      3: BEGIN
         infile=rerun_dmean_infile
         nyears_per_member=rerun_nyears
      END
   ENDCASE
   allindia_ts=fltarr(5,n_days)
   year_offset=initial_offset
   FOR j=0,4 DO BEGIN
      thisyear_precip=REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                       offset=[box_tx(1),box_tx(0),0,year_offset],$
                                             count=[n_lon,n_lat,n_days,1]))*86400.
      FOR k=0,n_days-1 DO BEGIN
         today_allindia=TOTAL(REFORM(thisyear_precip(*,*,k))*n96_weights)
         allindia_ts(j,k)=today_allindia
      ENDFOR
      year_offset=year_offset+nyears_per_member(j)
   ENDFOR
   CASE i OF
      0 : BEGIN
         foam_allindia_ts=allindia_ts
      END
      1 : BEGIN
         blended_allindia_ts=allindia_ts
      END
      2 : BEGIN
         amip2_allindia_ts=allindia_ts
      END
      3 : BEGIN
         rerun_allindia_ts=allindia_ts
      END
   ENDCASE
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/monsoon_iav/hadgem3kpp_monsoon_allindia_rainfall_spread.compare_foam_amip2.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=200,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100
CS,SCALE=26,NCOLS=5
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=16
red=FSC_COLOR("red",3)
blue=FSC_COLOR("blue",4)
purple=FSC_COLOR("purple",5)
pink=FSC_COLOR("pink",6)
;FOR i=0,4 DO BEGIN
;   GPLOT,X=indgen(n_days),Y=REFORM(foam_allindia_ts(i,*)),COL=3,THICK=100
;   GPLOT,X=indgen(n_days),Y=REFORM(amip2_allindia_ts(i,*)),COL=4,THICK=100
;   GPLOT,X=indgen(n_days),Y=REFORM(blended_allindia_ts(i,*)),COL=5,THICK=100
;ENDFOR
foam_allindia_ts_ensmean=fltarr(n_days)
foam_allindia_ts_stddev=fltarr(n_days)
amip2_allindia_ts_ensmean=fltarr(n_days)
amip2_allindia_ts_stddev=fltarr(n_days)
blended_allindia_ts_ensmean=fltarr(n_days)
blended_allindia_ts_stddev=fltarr(n_days)
rerun_allindia_ts_ensmean=fltarr(n_days)
rerun_allindia_ts_stddev=fltarr(n_days)
FOR i=0,n_days-1 DO BEGIN
   foam_allindia_ts_ensmean(i)=MEAN(foam_allindia_ts(*,i))
   foam_allindia_ts_stddev(i)=STDDEV(foam_allindia_ts(*,i))
   amip2_allindia_ts_ensmean(i)=MEAN(amip2_allindia_ts(*,i))
   amip2_allindia_ts_stddev(i)=STDDEV(amip2_allindia_ts(*,i))
   blended_allindia_ts_ensmean(i)=MEAN(blended_allindia_ts(*,i))
   blended_allindia_ts_stddev(i)=STDDEV(blended_allindia_ts(*,i))
;   rerun_allindia_ts_ensmean(i)=MEAN(rerun_allindia_ts(*,i))
ENDFOR
GPLOT,X=indgen(n_days),Y=foam_allindia_ts_ensmean,COL=3,THICK=200
GPLOT,X=indgen(n_days),Y=foam_allindia_ts_ensmean+foam_allindia_ts_stddev,COL=3,THICK=100,STYLE=1
GPLOT,X=indgen(n_days),Y=foam_allindia_ts_ensmean-foam_allindia_ts_stddev,COL=3,THICK=100,STYLE=1
GPLOT,X=indgen(n_days),Y=amip2_allindia_ts_ensmean,COL=4,THICK=200
GPLOT,X=indgen(n_days),Y=amip2_allindia_ts_ensmean+amip2_allindia_ts_stddev,COL=4,THICK=100,STYLE=1
GPLOT,X=indgen(n_days),Y=amip2_allindia_ts_ensmean-amip2_allindia_ts_stddev,COL=4,THICK=100,STYLE=1
GPLOT,X=indgen(n_days),Y=blended_allindia_ts_ensmean,COL=5,THICK=200
GPLOT,X=indgen(n_days),Y=blended_allindia_ts_ensmean-blended_allindia_ts_stddev,COL=5,THICK=100,STYLE=1
GPLOT,X=indgen(n_days),Y=blended_allindia_ts_ensmean+blended_allindia_ts_stddev,COL=5,THICK=100,STYLE=1
;GPLOT,X=indgen(n_days),Y=rerun_allindia_ts_ensmean,COL=6,THICK=200
;GPLOT,X=indgen(n_days),Y=rerun_allindia_ts_ensmean+rerun_allindia_ts_stddev,COL=6,THICK=100,STYLE=1
;GPLOT,X=indgen(n_days),Y=rerun_allindia_ts_ensmean-rerun_allindia_ts_stddev,COL=6,THICK=100,STYLE=1

GPLOT,X=n_days+5,Y=MEAN(foam_allindia_ts_ensmean(91:209)),COL=3,SYM=4
GPLOT,X=n_days+5,Y=MEAN(amip2_allindia_ts_ensmean(91:209)),COL=4,SYM=5
GPLOT,X=n_days+5,Y=MEAN(blended_allindia_ts_ensmean(91:209)),COL=5,SYM=6

AXES,XSTEP=20,YSTEP=1,XTITLE='Days since 1 March',YTITLE='Precipitation (mm day!U-1!N)'

PSCLOSE

STOP

END
