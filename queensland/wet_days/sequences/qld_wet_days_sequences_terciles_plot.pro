PRO qld_wet_days_sequences_terciles_plot

; Input file containing the number of sequences of wet days
silo_025_infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.nov-apr_dmeans.1900-2008.wet_day_sequences.by_tercile.0.25x0.25.nc'

; Box to consider
box=[-10,140,-30,152]

; Read latitude and longitude
silo_025_longitude=OPEN_AND_EXTRACT(silo_025_infile,'longitude')
silo_025_latitude=OPEN_AND_EXTRACT(silo_025_infile,'latitude')
silo_025_rain_thresholds=OPEN_AND_EXTRACT(silo_025_infile,'rain_threshold')
silo_025_time_thresholds=OPEN_AND_EXTRACT(silo_025_infile,'time_threshold')
DEFINE_BOUNDARIES,box,silo_025_latitude,silo_025_longitude,silo_025_box_tx,/LIMIT
silo_025_nlon=N_ELEMENTS(silo_025_longitude)
silo_025_nlat=N_ELEMENTS(silo_025_latitude)
silo_025_nrain_thresholds=N_ELEMENTS(silo_025_rain_thresholds)
silo_025_ntime_thresholds=N_ELEMENTS(silo_025_time_thresholds)

; Read mean number of sequences for upper, middle, lower terciles and
; area-average.
silo_025_nsequences_lower=OPEN_AND_EXTRACT(silo_025_infile,'sequences_lower_years',$
                                         offset=[silo_025_box_tx(1),silo_025_box_tx(0),0,0],$
                                         count=[silo_025_nlon,silo_025_nlat,silo_025_nrain_thresholds,silo_025_ntime_thresholds])

silo_025_nsequences_middle=OPEN_AND_EXTRACT(silo_025_infile,'sequences_middle_years',$
                                            offset=[silo_025_box_tx(1),silo_025_box_tx(0),0,0],$
                                            count=[silo_025_nlon,silo_025_nlat,silo_025_nrain_thresholds,silo_025_ntime_thresholds])

silo_025_nsequences_upper=OPEN_AND_EXTRACT(silo_025_infile,'sequences_upper_years',$
                                           offset=[silo_025_box_tx(1),silo_025_box_tx(0),0,0],$
                                           count=[silo_025_nlon,silo_025_nlat,silo_025_nrain_thresholds,silo_025_ntime_thresholds])

silo_025_nsequences_upper_aavg=fltarr(silo_025_nrain_thresholds,silo_025_ntime_thresholds)
silo_025_nsequences_middle_aavg=fltarr(silo_025_nrain_thresholds,silo_025_ntime_thresholds)
silo_025_nsequences_lower_aavg=fltarr(silo_025_nrain_thresholds,silo_025_ntime_thresholds)

FOR j=0,silo_025_nrain_thresholds-1 DO BEGIN
   FOR k=0,silo_025_ntime_thresholds-1 DO BEGIN
      silo_025_nsequences_upper_aavg(j,k)=MEAN(silo_025_nsequences_upper(*,*,j,k))
      silo_025_nsequences_middle_aavg(j,k)=MEAN(silo_025_nsequences_middle(*,*,j,k))
      silo_025_nsequences_lower_aavg(j,k)=MEAN(silo_025_nsequences_lower(*,*,j,k))
   ENDFOR
ENDFOR

mylevs=['0.0','0.25','0.5','0.75','1.0','1.33','1.66','2.00','2.5','3.0','4.0','5.0','6.0']

psfile='/home/ss901165/idl/queensland/wet_days/sequences/qld_wet_days_sequences_terciles.nsequences_aavg_lower.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=MIN(silo_025_rain_thresholds),XMAX=MAX(silo_025_rain_thresholds),YMIN=MIN(silo_025_time_thresholds),YMAX=MAX(silo_025_time_thresholds)
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",2)
LEVS,MANUAL=mylevs
CON,FIELD=silo_025_nsequences_lower_aavg,X=silo_025_rain_thresholds,Y=silo_025_time_thresholds,/BLOCK,/NOLINES
AXES,XVALS=FLOOR(silo_025_rain_thresholds),YVALS=FLOOR(silo_025_time_thresholds),NDECS=2
PSCLOSE

psfile='/home/ss901165/idl/queensland/wet_days/sequences/qld_wet_days_sequences_terciles.nsequences_aavg_middle.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=MIN(silo_025_rain_thresholds),XMAX=MAX(silo_025_rain_thresholds),YMIN=MIN(silo_025_time_thresholds),YMAX=MAX(silo_025_time_thresholds)
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",2)
LEVS,MANUAL=mylevs
CON,FIELD=silo_025_nsequences_middle_aavg,X=silo_025_rain_thresholds,Y=silo_025_time_thresholds,/BLOCK,/NOLINES
AXES,XVALS=FLOOR(silo_025_rain_thresholds),YVALS=FLOOR(silo_025_time_thresholds),NDECS=2
PSCLOSE

psfile='/home/ss901165/idl/queensland/wet_days/sequences/qld_wet_days_sequences_terciles.nsequences_aavg_upper.silo025.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
GSET,XMIN=MIN(silo_025_rain_thresholds),XMAX=MAX(silo_025_rain_thresholds),YMIN=MIN(silo_025_time_thresholds),YMAX=MAX(silo_025_time_thresholds)
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
;white=FSC_COLOR("white",2)
LEVS,MANUAL=mylevs
CON,FIELD=silo_025_nsequences_upper_aavg,X=silo_025_rain_thresholds,Y=silo_025_time_thresholds,/BLOCK,/NOLINES
AXES,XVALS=FLOOR(silo_025_rain_thresholds),YVALS=FLOOR(silo_025_time_thresholds),NDECS=2
PSCLOSE

STOP

END

