PRO qld_eots_correlate_mjo_silo025
  
; Correlate EOT timeseries against timeseries of the 
; Madden-Julian Oscillation (MJO).

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_eots_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(silo_eots_infiles)
; Number of EOTs to consider
n_eots=3

mjo_dir='/home/ss901165/datasets/MJO_INDICES'
mjo_infiles=[mjo_dir+'/MJO_rmm1_rmm2.dec-feb_dmeans.1975-2009.index_values.nc',$
             mjo_dir+'/MJO_rmm1_rmm2.mar-may_dmeans.1975-2009.index_values.nc',$
             mjo_dir+'/MJO_rmm1_rmm2.jun-aug_dmeans.1975-2009.index_values.nc',$
             mjo_dir+'/MJO_rmm1_rmm2.sep-nov_dmeans.1975-2009.index_values.nc']
mjo_offset=75 ; As offset to SILO EOT timeseries
mjo_nyears=33
mjo_maxdays=100

nino_dir='/home/ss901165/datasets/NINO'
nino_infiles=[nino_dir+'/nino4_hadisst.dec-feb_smeans.1871-2007.nc',$
              nino_dir+'/nino4_hadisst.mar-may_smeans.1871-2007.nc',$
              nino_dir+'/nino4_hadisst.jun-aug_smeans.1871-2007.nc',$
              nino_dir+'/nino4_hadisst.sep-nov_smeans.1871-2007.nc']
nino_offset=104
nino_nyears=mjo_nyears

silo_smeans_eots=fltarr(n_seasons,mjo_nyears,n_eots)
mjo_dmeans_amplitude=fltarr(n_seasons,mjo_maxdays,mjo_nyears)
mjo_dmeans_phase=fltarr(n_seasons,mjo_maxdays,mjo_nyears)
mjo_mean_amplitude=fltarr(n_seasons,mjo_nyears)
mjo_mean_amplitude_phase=fltarr(n_seasons,8,mjo_nyears)
mjo_freq_strong=fltarr(n_seasons,mjo_nyears)
mjo_freq_strong_phase=fltarr(n_seasons,8,mjo_nyears)
print,''
print,'Correlations with MJO activity:'
FOR i=0,n_seasons-1 DO BEGIN
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_eots_infiles(i),'loading',$
                                                   offset=[mjo_offset,0],$
                                                   count=[mjo_nyears,n_eots]))
   season_ndays=N_ELEMENTS(OPEN_AND_EXTRACT(mjo_infiles(i),'day_in_year'))
   mjo_dmeans_amplitude(i,0:season_ndays-1,*)=REFORM(OPEN_AND_EXTRACT(mjo_infiles(i),'amplitude',$
                                                                      offset=[0,0],$
                                                                      count=[season_ndays,mjo_nyears]))
   mjo_dmeans_phase(i,0:season_ndays-1,*)=REFORM(OPEN_AND_EXTRACT(mjo_infiles(i),'phase',$
                                                                  offset=[0,0],$
                                                                  count=[season_ndays,mjo_nyears]))
   nino4_ts=REFORM(OPEN_AND_EXTRACT(nino_infiles(i),'NINO4',$
                                    offset=[nino_offset],count=[nino_nyears]))

   FOR j=0,mjo_nyears-1 DO BEGIN
      thisyear_amplitude=REFORM(mjo_dmeans_amplitude(i,*,j))
      thisyear_phase=REFORM(mjo_dmeans_phase(i,*,j))
      mjo_mean_amplitude(i,j)=MEAN(thisyear_amplitude)
      FOR k=0,7 DO BEGIN
         IF N_ELEMENTS(where(thisyear_phase eq k+1)) gt 1 THEN BEGIN
            mjo_mean_amplitude_phase(i,k,j)=MEAN(thisyear_amplitude[where(thisyear_phase eq k+1)])
            mjo_freq_strong_phase(i,k,j)=N_ELEMENTS(where(thisyear_amplitude ge 1.0 and thisyear_phase eq k+1))
         ENDIF
      ENDFOR
      mjo_freq_strong(i,j)=N_ELEMENTS(where(thisyear_amplitude ge 1.0))
   ENDFOR

   season_correlations_mean_amplitude=fltarr(n_eots)
   season_correlations_mean_amplitude_phase=fltarr(8,n_eots)
   season_partial_correlations_mean_amplitude_phase=fltarr(8,n_eots)
   season_correlations_freq_strong=fltarr(n_eots)
   season_correlations_freq_strong_phase=fltarr(8,n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations_mean_amplitude(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                                      REFORM(mjo_mean_amplitude(i,*)))
      season_correlations_freq_strong(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                                   REFORM(mjo_freq_strong(i,*)))
      FOR k=0,7 DO BEGIN
         season_correlations_mean_amplitude_phase(k,j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                                               REFORM(mjo_mean_amplitude_phase(i,k,*)))
         season_partial_correlations_mean_amplitude_phase(k,j)=P_CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                                                           REFORM(mjo_mean_amplitude_phase(i,k,*)),$
                                                                           nino4_ts)
         season_correlations_freq_strong_phase(k,j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                                            REFORM(mjo_freq_strong_phase(i,k,*)))
      ENDFOR      
   ENDFOR
   print,season_names(i)
   FOR j=0,n_eots-1 DO BEGIN
      print,'EOT '+STRTRIM(STRING(j+1),1)+' with overall activity (amp/freq): '+$
            STRMID(STRTRIM(STRING(season_correlations_mean_amplitude(j)),1),0,6)+'/'+$
            STRMID(STRTRIM(STRING(season_correlations_freq_strong(j)),1),0,6)
      FOR k=0,7 DO $
         print,'EOT '+STRTRIM(STRING(j+1),1)+' with phase '+STRTRIM(STRING(k+1),1)+' (amp/freq): '+$
               STRMID(STRTRIM(STRING(season_correlations_mean_amplitude_phase(k,j)),1),0,6)+'/'+$
               STRMID(STRTRIM(STRING(season_correlations_freq_strong_phase(k,j)),1),0,6)+'  '+$
               STRMID(STRTRIM(STRING(season_partial_correlations_mean_amplitude_phase(k,j)),1),0,6)
   ENDFOR   
   STOP
ENDFOR

STOP

END

