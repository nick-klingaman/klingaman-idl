PRO aus_wet_days_percentiles_compute_freq_exceed_twentyc_ensmem
  
; Compute the frequency of exceeding a given climatological percentile
; within a given time period

; Input file containing the percentiles of daily rainfall
input_dir='/home/ss901165/datasets_mango/20THC_REANALYSIS/every_member/precip'
;percentiles_input_file=input_dir+'/ncep-ncar_reanalysis.mar-feb_dmeans.1958-2001.precip_percentiles.t62_gauss.nc'

n_output_periods=5
n_years=44
n_members=56

percentiles_to_consider=[90,91,92,93,94,95,96,97,98,99]
n_percentiles=N_ELEMENTS(percentiles_to_consider)

FOR i=0,n_output_periods-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         start_date=0
         stop_date=91
         period_name='mar-may'
         period_type='scount'
      END
      1 : BEGIN
         start_date=92
         stop_date=183
         period_name='jun-aug'
         period_type='scount'
      END
      2 : BEGIN
         start_date=184
         stop_date=274
         period_name='sep-nov'
         period_type='scount'
      END
      3 : BEGIN
         start_date=275
         stop_date=364
         period_name='dec-feb'
         period_type='scount'
      END
      4 : BEGIN
         start_date=0
         stop_date=364
         period_name='mar-feb'
         period_type='acount'
      END
   ENDCASE
   
   n_days=stop_date-start_date+1

   FOR p=0,n_members-1 DO BEGIN
      print,'Computing frequency of exceedence for member '+STRTRIM(STRING(p+1))
      percentiles_input_file=input_dir+'/20thc_reanalysis_member'+STRTRIM(STRING(p+1),1)+$
                             '.mar-feb_dmeans.1958-2001.precip_percentiles.aus_domain.nc'
      longitude=OPEN_AND_EXTRACT(percentiles_input_file,'longitude')
      latitude=OPEN_AND_EXTRACT(percentiles_input_file,'latitude')
                                ;DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      
      percentiles=OPEN_AND_EXTRACT(percentiles_input_file,'percentile_of_daily_rain',$
                                   offset=[0,0,0,start_date],count=[n_lon,n_lat,n_years,n_days])
      
      freq_exceed=fltarr(n_lon,n_lat,n_years,n_percentiles)
      FOR j=0,n_lon-1 DO BEGIN
         FOR k=0,n_lat-1 DO BEGIN
            IF TOTAL(where(percentiles(j,k,*,*) lt 2e19)) ne -1 THEN BEGIN
               FOR m=0,n_years-1 DO BEGIN
                  FOR n=0,n_percentiles-1 DO BEGIN
                     IF TOTAL(where(REFORM(percentiles(j,k,m,*)) gt percentiles_to_consider(n) and $
                                    REFORM(percentiles(j,k,m,*)) lt 2e19) ne -1) THEN BEGIN
                        freq_exceed(j,k,m,n)=N_ELEMENTS(where(REFORM(percentiles(j,k,m,*)) gt percentiles_to_consider(n) and $
                                                              REFORM(percentiles(j,k,m,*)) lt 2e19))
                     ENDIF ELSE $
                        freq_exceed(j,k,m,n)=0
                  ENDFOR
               ENDFOR
            ENDIF ELSE $
               freq_exceed(j,k,*,*)=2e20
         ENDFOR
      ENDFOR
      
      outfile=input_dir+'/20thc_reanalysis_member'+STRTRIM(STRING(p+1),1)+'.'+period_name+'_'+period_type+'s.1958-2001.precip_freq_exceed.aus_domain.nc'
      id=NCDF_CREATE(outfile,/CLOBBER)
      dimids=intarr(4)
      dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
      dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
      dimids(2)=NCDF_DIMDEF(id,'year',n_years)
      dimids(3)=NCDF_DIMDEF(id,'percentile',n_percentiles)
      varids=fltarr(5)
      varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
      varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
      varids(2)=NCDF_VARDEF(id,'year',[dimids(2)])
      varids(3)=NCDF_VARDEF(id,'percentile',[dimids(3)])
      varids(4)=NCDF_VARDEF(id,'freq_exceed',[dimids(0),dimids(1),dimids(2),dimids(3)])
      
      NCDF_ATTPUT,id,varids(4),'missing_value',2e20

      NCDF_CONTROL,id,/ENDEF
      
      NCDF_VARPUT,id,varids(0),longitude
      NCDF_VARPUT,id,varids(1),latitude
      NCDF_VARPUT,id,varids(2),indgen(n_years)+1958
      NCDF_VARPUT,id,varids(3),percentiles_to_consider
      NCDF_VARPUT,id,varids(4),freq_exceed
      
      NCDF_CLOSE,id
      
   ENDFOR
ENDFOR

STOP
END
