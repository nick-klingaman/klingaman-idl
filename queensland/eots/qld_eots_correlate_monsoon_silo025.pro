PRO qld_eots_correlate_monsoon_silo025
  
; Correlate EOT timeseries against timeseries of the 
; Southern Annular Mode (SAM).

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_eots_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2010.eots.nc',$
                   silo_dir+'/SILO.mar-may_smeans.1900-2010.eots.nc']
                   ;silo_dir+'/SILO.jun-aug_smeans.1900-2010.eots.nc',$
                   ;silo_dir+'/SILO.sep-nov_smeans.1900-2010.eots.nc']
season_names=['dec-feb','mar-may']
n_seasons=N_ELEMENTS(silo_eots_infiles)
; Number of EOTs to consider
n_eots=3

monsoon_dir='/home/ss901165/datasets/20THC_REANALYSIS/zonal_wind'
monsoon_infiles=[monsoon_dir+'/20thc_reanalysis.dec-feb_smeans.1871-2009.kajikawa_monsoon_u850.nc',$
                 monsoon_dir+'/20thc_reanalysis.mar-may_smeans.1871-2009.kajikawa_monsoon_u850.nc']            
monsoon_offset=0 ; As offset to SILO EOT timeseries
monsoon_read_offset=29
monsoon_nyears=110

silo_smeans_eots=fltarr(n_seasons,monsoon_nyears,n_eots)
monsoon_smeans=fltarr(n_seasons,monsoon_nyears)
print,''
print,'Correlations with kajikawa monsoon index:'
FOR i=0,n_seasons-1 DO BEGIN
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_eots_infiles(i),'loading',$
                                                   offset=[monsoon_offset,0],$
                                                   count=[monsoon_nyears,n_eots]))
   monsoon_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(monsoon_infiles(i),'U',$
                                           offset=[monsoon_read_offset],count=[monsoon_nyears]))

   season_correlations=fltarr(n_eots)
   season_pcorrelations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                       REFORM(monsoon_smeans(i,*)))
   ENDFOR
   print,season_names(i)
   FOR j=0,n_eots-1 DO $
      print,'EOT '+STRTRIM(STRING(j+1),1)+': '+STRMID(STRTRIM(STRING(season_correlations(j)),1),0,6)            
ENDFOR

STOP

END

