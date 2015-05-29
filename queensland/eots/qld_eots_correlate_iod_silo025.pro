PRO qld_eots_correlate_iod_silo025
  
; Correlate EOT timeseries against timeseries of the 
; Indian Ocean Dipole (IOD).

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_eots_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(silo_eots_infiles)
; Number of EOTs to consider
n_eots=3

iod_dir='/home/ss901165/datasets/IOD'
iod_infiles=[iod_dir+'/iod_index.dec-feb_smeans.1958-2007.hadisst.nc',$
             iod_dir+'/iod_index.mar-may_smeans.1958-2007.hadisst.nc',$
             iod_dir+'/iod_index.jun-aug_smeans.1958-2007.hadisst.nc',$
             iod_dir+'/iod_index.sep-nov_smeans.1958-2007.hadisst.nc']
iod_offset=58 ; As offset to SILO EOT timeseries
iod_nyears=50

nino4_dir='/home/ss901165/datasets/NINO'
nino4_infiles=[nino4_dir+'/nino4_hadisst.dec-feb_smeans.1871-2007.nc',$
               nino4_dir+'/nino4_hadisst.mar-may_smeans.1871-2007.nc',$
               nino4_dir+'/nino4_hadisst.jun-aug_smeans.1871-2007.nc',$
               nino4_dir+'/nino4_hadisst.sep-nov_smeans.1871-2007.nc']
nino4_offset=87
nino4_nyears=iod_nyears

silo_smeans_eots=fltarr(n_seasons,iod_nyears,n_eots)
iod_smeans=fltarr(n_seasons,iod_nyears)
nino4_smeans=fltarr(n_seasons,nino4_nyears)
print,''
print,'Correlations with Indian Ocean Dipole:'
FOR i=0,n_seasons-1 DO BEGIN
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_eots_infiles(i),'loading',$
                                                   offset=[iod_offset,0],$
                                                   count=[iod_nyears,n_eots]))
   iod_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(iod_infiles(i),'IOD',$
                                           offset=[0],count=[iod_nyears]))
   nino4_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(nino4_infiles(i),'NINO4',$
                                             offset=[nino4_offset],count=[nino4_nyears]))
   ;iod_smeans_trend=REGRESS(indgen(iod_nyears),REFORM(iod_smeans(i,*)))
   ;iod_smeans(i,*)=iod_smeans(i,*)-iod_smeans_trend(0)*indgen(iod_nyears)

   season_correlations=fltarr(n_eots)
   season_pcorrelations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                       REFORM(iod_smeans(i,*)))
      season_pcorrelations(j)=P_CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                          REFORM(iod_smeans(i,*)),$
                                          REFORM(nino4_smeans(i,*)))
   ENDFOR
   print,season_names(i)
   FOR j=0,n_eots-1 DO $
      print,'EOT '+STRTRIM(STRING(j+1),1)+': '+STRMID(STRTRIM(STRING(season_correlations(j)),1),0,6),$
            +' '+STRMID(STRTRIM(STRING(season_pcorrelations(j)),1),0,6)
ENDFOR

STOP

END

