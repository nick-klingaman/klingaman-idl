PRO qld_decadal_regress_monthly_precip

; File containing timeseries of monthly mean SILO rainfall
silo_basedir='/home/ss901165/datasets_mango/SILO/one_quarter/'

; Box over which to area-average
box=[-10,138,-30,154]

start_year=1945
offset=45
n_years=(2011-start_year)

silo_amean_infile=silo_basedir+'/SILO_precip.nov-apr_smeans.1900-2011.0.25x0.25.nc'
silo_mask_infile='/home/ss901165/datasets/SILO/one_quarter_lsm.nc'

; Read latitude and longitude, find box of interest
silo_longitude=OPEN_AND_EXTRACT(silo_amean_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_amean_infile,'latitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

mask_longitude=OPEN_AND_EXTRACT(silo_mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(silo_mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(silo_mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

silo_ameans=OPEN_AND_EXTRACT(silo_amean_infile,'rain',$
                             offset=[silo_box_tx(1),silo_box_tx(0),offset],$
                             count=[silo_nlon,silo_nlat,n_years])

silo_ameans_aavg=fltarr(n_years)
silo_ameans_aavg_7yr=fltarr(n_years-10)
FOR i=0,n_years-1 DO BEGIN
   temp=REFORM(silo_ameans(*,*,i))
   temp[where(mask eq 0 or temp eq 2e20)]=!Values.F_NaN
   silo_ameans(*,*,i)=temp
   silo_ameans_aavg(i)=MEAN(silo_ameans(*,*,i),/NaN)*182.
ENDFOR
temp=SMOOTH(silo_ameans_aavg,7)
silo_ameans_aavg_7yr=temp(3:n_years-4)

psfile='/home/ss901165/idl/queensland/precip_ts/qld_decadal_regress_monthly_precip.silo_nov-apr_amean_ts_1945-2011_7yr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=2000,YOFFSET=500,TFONT=2,$
       TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=start_year,XMAX=start_year+n_years,YMIN=-400,YMAX=600
red=FSC_COLOR('red',10)
black=FSC_COLOR('black',11)
HIST,X=indgen(n_years)+start_year+0.5,Y=silo_ameans_aavg-MEAN(silo_ameans_aavg),WIDTH=50,FILLCOL=10
GPLOT,X=indgen(n_years-7)+start_year+5.5,Y=silo_ameans_aavg_7yr-MEAN(silo_ameans_aavg),COL=11,THICK=225
AXES,XSTEP=10,XMINOR=5,YSTEP=100,YMINOR=50,YTITLE='November-April precipitation anomaly (mm)',XTITLE='Year in November'
PSCLOSE

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
n_months=N_ELEMENTS(months)
mylevs_regress=['-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3']
mmean_box=[-10,112,-40,154]
FOR i=0,n_months-1 DO BEGIN
   silo_mmean_infile=silo_basedir+'/SILO.'+months(i)+'_mmeans.1900-2011.precip.0.25x0.25.nc'
   silo_mmean_longitude=OPEN_AND_EXTRACT(silo_mmean_infile,'longitude')
   silo_mmean_latitude=OPEN_AND_EXTRACT(silo_mmean_infile,'latitude')
   DEFINE_BOUNDARIES,mmean_box,silo_mmean_latitude,silo_mmean_longitude,silo_mmean_box_tx,/LIMIT
   silo_mmean_nlon=N_ELEMENTS(silo_mmean_longitude)
   silo_mmean_nlat=N_ELEMENTS(silo_mmean_latitude)

   mask_longitude=OPEN_AND_EXTRACT(silo_mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(silo_mask_infile,'latitude')
   DEFINE_BOUNDARIES,mmean_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)
   mask=REFORM(OPEN_AND_EXTRACT(silo_mask_infile,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
   
   silo_mmeans=OPEN_AND_EXTRACT(silo_mmean_infile,'rain',$
                                offset=[silo_mmean_box_tx(1),silo_mmean_box_tx(0),offset],$
                                count=[silo_mmean_nlon,silo_mmean_nlat,n_years])*30.
   silo_mmeans_aavg=fltarr(n_years)
   silo_mmeans_aavg_7yr=fltarr(n_years-10)
   FOR j=0,n_years-1 DO BEGIN
      temp=REFORM(silo_mmeans(*,*,j))
      temp[where(mask eq 0 or temp eq 2e20)]=!Values.F_NaN
      silo_mmeans(*,*,j)=temp
      silo_mmeans_aavg(j)=MEAN(silo_mmeans(*,*,j),/NaN)
   ENDFOR

   regress_mmean_smoothed=fltarr(silo_mmean_nlon,silo_mmean_nlat)
   corr_mmean_smoothed=fltarr(silo_mmean_nlon,silo_mmean_nlat)
   FOR j=0,silo_mmean_nlon-1 DO BEGIN
      FOR k=0,silo_mmean_nlat-1 DO BEGIN
         IF FINITE(silo_mmeans(j,k,0)) eq 1 THEN BEGIN
            regress_mmean_smoothed(j,k)=REGRESS(silo_ameans_aavg_7yr,REFORM(silo_mmeans(j,k,3:n_years-4)))
            corr_mmean_smoothed(j,k)=CORRELATE(silo_ameans_aavg_7yr,REFORM(silo_mmeans(j,k,3:n_years-4)))
         ENDIF
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/precip_ts/qld_decadal_regress_monthly_precip.silo_regress_'+months(i)+'_nov-apr7yr_1945-2011.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=2000,YOFFSET=500,TFONT=2,$
          TCHARSIZE=100,CB_WIDTH=110
   MAP,LONMIN=mmean_box(1),LONMAX=mmean_box(3),LATMIN=mmean_box(2),LATMAX=mmean_box(0)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_regress)+1,/REV
   LEVS,MANUAL=mylevs_regress
   regress_mmean_smoothed[where(ABS(corr_mmean_smoothed) lt 0.20)]=!Values.F_NaN
   CON,X=silo_mmean_longitude,Y=silo_mmean_latitude,FIELD=regress_mmean_smoothed,/BLOCK,/NOLINES
   AXES
   PSCLOSE,/NOVIEW
ENDFOR

STOP

END
