PRO qld_wet_days_percentiles_convert_era40

; Convert daily rain amounts into percentiles, based on the
; distribution of rain amounts in all years.  Use a threshold (1
; mm/day?) to identify wet days.  Make the percentiles relative to wet
; days only, to eliminate zero values.

era40_infile='/home/ss901165/datasets/ERA40/PRECIP/era40.mar-feb_dmeans.1958-2001.precip.aus_domain.nc.rearrange'
mask_infile='/home/ss901165/datasets/ERA40/era40_lsm.nc'
era40_nyears=44
era40_ndays_per_year=365

box=[-10,112,-44,154]
;box=[-10,138,-30,154]

; Percentiles to analyze (amount of rain and probability of wet days)
percentiles=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,91,92,93,94,95,96,97,98,99]
n_percentiles=N_ELEMENTS(percentiles)

era40_latitude=OPEN_AND_EXTRACT(era40_infile,'latitude')
era40_longitude=OPEN_AND_EXTRACT(era40_infile,'longitude')
DEFINE_BOUNDARIES,box,era40_latitude,era40_longitude,era40_box_tx,/LIMIT
era40_nlat=N_ELEMENTS(era40_latitude)
era40_nlon=N_ELEMENTS(era40_longitude)

mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlat=N_ELEMENTS(mask_latitude)
mask_nlon=N_ELEMENTS(mask_longitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'LSM',$
                             offset=[mask_box_tx(1),mask_box_tx(0)],$
                             count=[mask_nlon,mask_nlat]))

outfile='/home/ss901165/datasets/ERA40/PRECIP/era40.mar-feb_dmeans.1958-2001.precip_percentiles.aus_domain.nc'
dimids=intarr(5)
varids=intarr(8)

id=NCDF_CREATE(outfile,/CLOBBER)
dimids(0)=NCDF_DIMDEF(id,'longitude',era40_nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',era40_nlat)
dimids(2)=NCDF_DIMDEF(id,'year',era40_nyears)
dimids(3)=NCDF_DIMDEF(id,'time',era40_ndays_per_year)
dimids(4)=NCDF_DIMDEF(id,'percentile',n_percentiles)

varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'year',[dimids(2)])
varids(3)=NCDF_VARDEF(id,'time',[dimids(3)])
varids(4)=NCDF_VARDEF(id,'percentile',[dimids(4)])
varids(5)=NCDF_VARDEF(id,'percentile_of_daily_rain',[dimids(0),dimids(1),dimids(2),dimids(3)])
varids(6)=NCDF_VARDEF(id,'amount_at_percentile',[dimids(0),dimids(1),dimids(4)])
;varids(7)=NCDF_VARDEF(id,'prob_wet_given_wet',[dimids(0),dimids(1),dimids(4)])
;varids(5)=NCDF_VARDEF(id,'percentile_of_daily_rain',[dimids(3),dimids(2),dimids(1),dimids(0)])
;varids(6)=NCDF_VARDEF(id,'amount_at_percentile',[dimids(4),dimids(1),dimids(0)])
;varids(7)=NCDF_VARDEF(id,'prob_wet_given_wet',[dimids(4),dimids(1),dimids(0)])

NCDF_ATTPUT,id,varids(5),'missing_value',2E20
NCDF_ATTPUT,id,varids(6),'missing_value',2E20
;NCDF_ATTPUT,id,varids(7),'missing_value',2E20
;NCDF_ATTPUT,id,varids(2),'units','years since 1900'
;NCDF_ATTPUT,id,varids(3),'units','days since 1 Jan'
;NCDF_ATTPUT,id,varids(4),'units','percentile of daily rain amounts (all years, all months)'
;NCDF_ATTPUT,id,varids(6),'units','mm/day'

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),era40_longitude
NCDF_VARPUT,id,varids(1),era40_latitude
NCDF_VARPUT,id,varids(2),indgen(era40_nyears)+1958
NCDF_VARPUT,id,varids(3),indgen(era40_ndays_per_year)
NCDF_VARPUT,id,varids(4),percentiles

total_days=FLOAT(era40_nyears)*FLOAT(era40_ndays_per_year)
FOR i=0,era40_nlon-1 DO BEGIN
   FOR j=0,era40_nlat-1 DO BEGIN
      print,i,j,mask(i,j)
      thispt_amount_at_percentile=fltarr(n_percentiles)
;      thispt_precip=REFORM(OPEN_AND_EXTRACT(era40_infile,'rain',$
;                                            offset=[era40_box_tx(1)+i,era40_box_tx(0)+j,0,0],$
;                                            count=[1,1,era40_ndays_per_year,era40_nyears]),[total_days])
      print,'Reading ...'
      thispt_precip=REFORM(OPEN_AND_EXTRACT(era40_infile,'precip',$
                                            offset=[0,0,era40_box_tx(0)+j,era40_box_tx(1)+i],$
                                            count=[era40_nyears,era40_ndays_per_year,1,1]))*1000.
      print,mask(i,j),thispt_precip(0,0)
      IF mask(i,j) ne 0 and ABS(thispt_precip(0,0)) lt 1E6 THEN BEGIN
         print,'Reforming ...'
         thispt_precip_ts=fltarr(LONG(era40_nyears)*era40_ndays_per_year)
         FOR k=0,era40_nyears-1 DO $
            thispt_precip_ts(k*LONG(era40_ndays_per_year):(k+1)*LONG(era40_ndays_per_year)-1)=$
            thispt_precip(k,*)         
         print,'Computing percentiles ...'     
         wet_days=[where(thispt_precip_ts gt 1)]
         dry_days=[where(thispt_precip_ts le 1)]
         n_wet_days=N_ELEMENTS(wet_days)
         thispt_precip_percentiles=fltarr(total_days)
         thispt_precip_percentiles[dry_days]=2E20
         thispt_precip_sorted_indices=SORT(thispt_precip_ts[wet_days])
         FOR k=0,n_wet_days-1 DO $
            thispt_precip_percentiles[wet_days(thispt_precip_sorted_indices(k))]=k/FLOAT(n_wet_days)*100.
         done=n_wet_days+1
         FOR k=0,n_wet_days-1 DO BEGIN
            IF TOTAL(done eq FLOAT(k)) eq 0 THEN BEGIN
               same_pts=(where(thispt_precip_ts eq thispt_precip_ts[wet_days(k)] and $
                               thispt_precip_percentiles ne thispt_precip_percentiles[wet_days(k)]))
               n_same_pts=N_ELEMENTS(same_pts)
               IF n_same_pts gt 1 THEN $
                  thispt_precip_percentiles[same_pts]=MEAN(thispt_precip_percentiles[same_pts])
               done=[done,FLOAT(same_pts)]
            ENDIF
         ENDFOR
         print,'Writing percentiles ...'
         FOR k=0,era40_nyears-1 DO BEGIN
            lower_bound=LONG(k*FLOAT(era40_ndays_per_year))
            upper_bound=LONG((k+1)*FLOAT(era40_ndays_per_year)-1)            
;            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles(lower_bound:upper_bound),$
;                        offset=[0,k,j,i],count=[era40_ndays_per_year,1,1,1]
            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles(lower_bound:upper_bound),$
                        offset=[i,j,k,0],count=[1,1,1,era40_ndays_per_year]
         ENDFOR
         FOR k=0,n_percentiles-1 DO $
            thispt_amount_at_percentile(k)=thispt_precip_ts(NEAREST(thispt_precip_percentiles,percentiles(k)))
         print,'Writing rainfall amounts ...',thispt_amount_at_percentile(15)
;         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[0,j,i],count=[n_percentiles,1,1]
         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[i,j,0],count=[1,1,n_percentiles]
         ;IF i eq 1 and j eq 6 THEN BEGIN
         ;  NCDF_CLOSE,id
         ;   STOP
         ;ENDIF
      ENDIF ELSE BEGIN
         thispt_amount_at_percentile(*)=2E20
         print,'Writing percentiles ...'
         FOR k=0,era40_nyears-1 DO BEGIN
            thispt_precip_percentiles=fltarr(era40_ndays_per_year)
            thispt_precip_percentiles(*)=2E20
;            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles,offset=[0,k,j,i],$
;                        count=[era40_ndays_per_year,1,1,1]
            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles,offset=[i,j,k,0],$
                        count=[1,1,1,era40_ndays_per_year]
         ENDFOR
         print,'Writing rainfall amounts ...'
;         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[0,j,i],count=[n_percentiles,1,1]
         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[i,j,0],count=[1,1,n_percentiles]
      ENDELSE
   ENDFOR   
ENDFOR

NCDF_CLOSE,id

STOP

END
