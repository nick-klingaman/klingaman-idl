PRO qld_higem_eots_correlate_iod_eafeb
  
; Correlate EOT timeseries against timeseries of the 
; Southern Annular Mode (IOD).

higem_dir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_eots_infiles=[higem_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.mar-may_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(higem_eots_infiles)
; Number of EOTs to consider
n_eots=4

iod_dir='/home/ss901165/higem_qccce/es_control_eafeb'
iod_infiles=[iod_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.iod_index.nc',$
             iod_dir+'/higem_eafeb.mar-may_smeans.h9-w8.iod_index.nc',$
             iod_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.iod_index.nc',$
             iod_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.iod_index.nc']
iod_offset=0 ; As offset to HIGEM EOT timeseries
iod_nyears=149

nino4_dir='/home/ss901165/higem_qccce/es_control_eafeb'
nino4_infiles=[nino4_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.mar-may_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.nino_indices.nc']
nino4_offset=0
nino4_nyears=iod_nyears

higem_smeans_eots=fltarr(n_seasons,iod_nyears,n_eots)
iod_smeans=fltarr(n_seasons,iod_nyears)
nino4_smeans=fltarr(n_seasons,nino4_nyears)
print,''
print,'Correlations with Southern Annular Mode:'
FOR i=0,n_seasons-1 DO BEGIN
   higem_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(higem_eots_infiles(i),'loading',$
                                                   offset=[iod_offset,0],$
                                                   count=[iod_nyears,n_eots]))
   iod_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(iod_infiles(i),'iod_index',$
                                           offset=[0],count=[iod_nyears]))
   nino4_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(nino4_infiles(i),'nino4',$
                                             offset=[nino4_offset],count=[nino4_nyears]))
   ;iod_smeans_trend=REGRESS(indgen(iod_nyears),REFORM(iod_smeans(i,*)))
   ;iod_smeans(i,*)=iod_smeans(i,*)-iod_smeans_trend(0)*indgen(iod_nyears)

   season_correlations=fltarr(n_eots)
   season_pcorrelations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations(j)=CORRELATE(REFORM(higem_smeans_eots(i,*,j)),$
                                       REFORM(iod_smeans(i,*)))
      season_pcorrelations(j)=P_CORRELATE(REFORM(higem_smeans_eots(i,*,j)),$
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

