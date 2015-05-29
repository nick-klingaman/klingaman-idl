PRO qld_wet_days_percentiles_convert_higem_eafeb_djf

; Convert daily rain amounts into percentiles, based on the
; distribution of rain amounts in all years.  Use a threshold (1
; mm/day?) to identify wet days.  Make the percentiles relative to wet
; days only, to eliminate zero values.

eafeb_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip.global_domain.nc.rearrange'
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
eafeb_nyears=149
eafeb_ndays_per_year=90

box=[20,40,-50,180]
;box=[-10,138,-30,154]

; Percentiles to analyze (amount of rain and probability of wet days)
percentiles=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,91,92,93,94,95,96,97,98,99]
n_percentiles=N_ELEMENTS(percentiles)

eafeb_latitude=OPEN_AND_EXTRACT(eafeb_infile,'latitude')
eafeb_longitude=OPEN_AND_EXTRACT(eafeb_infile,'longitude')
DEFINE_BOUNDARIES,box,eafeb_latitude,eafeb_longitude,eafeb_box_tx,/LIMIT
eafeb_nlat=N_ELEMENTS(eafeb_latitude)
eafeb_nlon=N_ELEMENTS(eafeb_longitude)

mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlat=N_ELEMENTS(mask_latitude)
mask_nlon=N_ELEMENTS(mask_longitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_dmeans.h9-w8.precip_percentiles.aus_domain.nc'
dimids=intarr(5)
varids=intarr(8)

id=NCDF_CREATE(outfile,/CLOBBER)
dimids(0)=NCDF_DIMDEF(id,'longitude',eafeb_nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',eafeb_nlat)
dimids(2)=NCDF_DIMDEF(id,'year',eafeb_nyears)
dimids(3)=NCDF_DIMDEF(id,'time',eafeb_ndays_per_year)
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

NCDF_VARPUT,id,varids(0),eafeb_longitude
NCDF_VARPUT,id,varids(1),eafeb_latitude
NCDF_VARPUT,id,varids(2),indgen(eafeb_nyears)+1958
NCDF_VARPUT,id,varids(3),indgen(eafeb_ndays_per_year)
NCDF_VARPUT,id,varids(4),percentiles

total_days=FLOAT(eafeb_nyears)*FLOAT(eafeb_ndays_per_year)
FOR i=0,eafeb_nlon-1 DO BEGIN
   FOR j=0,eafeb_nlat-1 DO BEGIN
      print,i,j,mask(i,j)
      thispt_amount_at_percentile=fltarr(n_percentiles)
;      thispt_precip=REFORM(OPEN_AND_EXTRACT(eafeb_infile,'rain',$
;                                            offset=[eafeb_box_tx(1)+i,eafeb_box_tx(0)+j,0,0],$
;                                            count=[1,1,eafeb_ndays_per_year,eafeb_nyears]),[total_days])
      print,'Reading ...'
      thispt_precip=REFORM(OPEN_AND_EXTRACT(eafeb_infile,'precip',$
                                            offset=[0,0,eafeb_box_tx(0)+j,eafeb_box_tx(1)+i],$
                                            count=[eafeb_nyears,eafeb_ndays_per_year,1,1]))*86400.
      print,mask(i,j),thispt_precip(0,0)
      IF ABS(thispt_precip(0,0)) lt 1E6 THEN BEGIN
         print,'Reforming ...'
         thispt_precip_ts=fltarr(LONG(eafeb_nyears)*eafeb_ndays_per_year)
         FOR k=0,eafeb_nyears-1 DO $
            thispt_precip_ts(k*LONG(eafeb_ndays_per_year):(k+1)*LONG(eafeb_ndays_per_year)-1)=$
            thispt_precip(k,*)         
         print,'Computing percentiles ...'     
         wet_days=[where(thispt_precip_ts gt 1)]
         dry_days=[where(thispt_precip_ts le 1)]
         n_wet_days=N_ELEMENTS(wet_days)
         thispt_precip_percentiles=fltarr(total_days)
         thispt_precip_percentiles[dry_days]=2E20
         thispt_precip_sorted_indices=SORT(thispt_precip_ts[wet_days])
         FOR k=LONG(0),LONG(n_wet_days)-1 DO $
            thispt_precip_percentiles[wet_days(thispt_precip_sorted_indices(k))]=k/FLOAT(n_wet_days)*100.
         done=n_wet_days+1
         FOR k=LONG(0),LONG(n_wet_days)-1 DO BEGIN
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
         FOR k=0,eafeb_nyears-1 DO BEGIN
            lower_bound=LONG(k*FLOAT(eafeb_ndays_per_year))
            upper_bound=LONG((k+1)*FLOAT(eafeb_ndays_per_year)-1)            
;            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles(lower_bound:upper_bound),$
;                        offset=[0,k,j,i],count=[eafeb_ndays_per_year,1,1,1]
            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles(lower_bound:upper_bound),$
                        offset=[i,j,k,0],count=[1,1,1,eafeb_ndays_per_year]
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
         FOR k=0,eafeb_nyears-1 DO BEGIN
            thispt_precip_percentiles=fltarr(eafeb_ndays_per_year)
            thispt_precip_percentiles(*)=2E20
;            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles,offset=[0,k,j,i],$
;                        count=[eafeb_ndays_per_year,1,1,1]
            NCDF_VARPUT,id,varids(5),thispt_precip_percentiles,offset=[i,j,k,0],$
                        count=[1,1,1,eafeb_ndays_per_year]
         ENDFOR
         print,'Writing rainfall amounts ...'
;         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[0,j,i],count=[n_percentiles,1,1]
         NCDF_VARPUT,id,varids(6),thispt_amount_at_percentile,offset=[i,j,0],count=[1,1,n_percentiles]
      ENDELSE
   ENDFOR   
ENDFOR

NCDF_CLOSE,id

END
