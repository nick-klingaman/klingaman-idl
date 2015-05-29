PRO qld_eots_correlate_sam_silo025
  
; Correlate EOT timeseries against timeseries of the 
; Southern Annular Mode (SAM).

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_eots_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                   silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(silo_eots_infiles)
; Number of EOTs to consider
n_eots=3

sam_dir='/home/ss901165/datasets/SAM'
sam_infiles=[sam_dir+'/SAM_index_BAS.dec-feb_smeans.1957-2009.nc',$
             sam_dir+'/SAM_index_BAS.mar-may_smeans.1957-2009.nc',$
             sam_dir+'/SAM_index_BAS.jun-aug_smeans.1957-2009.nc',$
             sam_dir+'/SAM_index_BAS.sep-nov_smeans.1957-2009.nc']
sam_offset=57 ; As offset to SILO EOT timeseries
sam_nyears=51

nino4_dir='/home/ss901165/datasets/NINO'
nino4_infiles=[nino4_dir+'/nino3_hadisst.dec-feb_smeans.1871-2007.nc',$
               nino4_dir+'/nino3_hadisst.mar-may_smeans.1871-2007.nc',$
               nino4_dir+'/nino3_hadisst.jun-aug_smeans.1871-2007.nc',$
               nino4_dir+'/nino3_hadisst.sep-nov_smeans.1871-2007.nc']
nino4_offset=86
nino4_nyears=sam_nyears

silo_smeans_eots=fltarr(n_seasons,sam_nyears,n_eots)
sam_smeans=fltarr(n_seasons,sam_nyears)
nino4_smeans=fltarr(n_seasons,nino4_nyears)
print,''
print,'Correlations with Southern Annular Mode:'
FOR i=0,n_seasons-1 DO BEGIN
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_eots_infiles(i),'loading',$
                                                   offset=[sam_offset,0],$
                                                   count=[sam_nyears,n_eots]))
   sam_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(sam_infiles(i),'SAM',$
                                           offset=[0],count=[sam_nyears]))
   nino4_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(nino4_infiles(i),'NINO3',$
                                             offset=[nino4_offset],count=[nino4_nyears]))
   ;sam_smeans_trend=REGRESS(indgen(sam_nyears),REFORM(sam_smeans(i,*)))
   ;sam_smeans(i,*)=sam_smeans(i,*)-sam_smeans_trend(0)*indgen(sam_nyears)

   season_correlations=fltarr(n_eots)
   season_pcorrelations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations(j)=CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                       REFORM(sam_smeans(i,*)))
      season_pcorrelations(j)=P_CORRELATE(REFORM(silo_smeans_eots(i,*,j)),$
                                          REFORM(sam_smeans(i,*)),$
                                          REFORM(nino4_smeans(i,*)))
   ENDFOR
   print,season_names(i)
   FOR j=0,n_eots-1 DO $
      print,'EOT '+STRTRIM(STRING(j+1),1)+': '+STRMID(STRTRIM(STRING(season_correlations(j)),1),0,6),$
            +' '+STRMID(STRTRIM(STRING(season_pcorrelations(j)),1),0,6)
ENDFOR

STOP

END

