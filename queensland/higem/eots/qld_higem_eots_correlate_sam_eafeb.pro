PRO qld_higem_eots_correlate_sam_eafeb
  
; Correlate EOT timeseries against timeseries of the 
; Southern Annular Mode (SAM).

higem_dir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_eots_infiles=[higem_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.mar-may_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.eots.nc',$
                    higem_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.eots.nc']
season_names=['dec-feb','mar-may','jun-aug','sep-nov']
n_seasons=N_ELEMENTS(higem_eots_infiles)
; Number of EOTs to consider
n_eots=4

sam_dir='/home/ss901165/higem_qccce/es_control_eafeb'
sam_infiles=[sam_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.sam_index.nc',$
             sam_dir+'/higem_eafeb.mar-may_smeans.h9-w8.sam_index.nc',$
             sam_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.sam_index.nc',$
             sam_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.sam_index.nc']
sam_offset=0 ; As offset to HIGEM EOT timeseries
sam_nyears=149

nino4_dir='/home/ss901165/higem_qccce/es_control_eafeb'
nino4_infiles=[nino4_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.mar-may_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.nino_indices.nc',$
               nino4_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.nino_indices.nc']
nino4_offset=0
nino4_nyears=sam_nyears

higem_smeans_eots=fltarr(n_seasons,sam_nyears,n_eots)
sam_smeans=fltarr(n_seasons,sam_nyears)
nino4_smeans=fltarr(n_seasons,nino4_nyears)
print,''
print,'Correlations with Southern Annular Mode:'
FOR i=0,n_seasons-1 DO BEGIN
   higem_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(higem_eots_infiles(i),'loading',$
                                                   offset=[sam_offset,0],$
                                                   count=[sam_nyears,n_eots]))
   sam_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(sam_infiles(i),'sam_index',$
                                           offset=[0],count=[sam_nyears]))
   nino4_smeans(i,*)=REFORM(OPEN_AND_EXTRACT(nino4_infiles(i),'nino4',$
                                             offset=[nino4_offset],count=[nino4_nyears]))
   ;sam_smeans_trend=REGRESS(indgen(sam_nyears),REFORM(sam_smeans(i,*)))
   ;sam_smeans(i,*)=sam_smeans(i,*)-sam_smeans_trend(0)*indgen(sam_nyears)

   season_correlations=fltarr(n_eots)
   season_pcorrelations=fltarr(n_eots)
   FOR j=0,n_eots-1 DO BEGIN
      season_correlations(j)=CORRELATE(REFORM(higem_smeans_eots(i,*,j)),$
                                       REFORM(sam_smeans(i,*)))
      season_pcorrelations(j)=P_CORRELATE(REFORM(higem_smeans_eots(i,*,j)),$
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

