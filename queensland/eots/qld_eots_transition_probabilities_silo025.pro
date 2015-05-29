PRO qld_eots_transition_probabilities_silo025

; Timeseries of EOTs for SILO seasonal means
silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_smeans_infiles=[silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc']
n_seasons=N_ELEMENTS(silo_smeans_infiles)
season_periods=['mar-may','jun-aug','sep-nov','dec-feb']

; Number of EOTs to consider
n_eots=3
; Number of years for each EOT
n_years=108

silo_smeans_eots=fltarr(n_seasons,n_years,n_eots)
FOR i=0,n_seasons-1 DO $
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_smeans_infiles(i),'loading',$
                                                   offset=[0,0],count=[n_years,n_eots]))

eot_transition_matrix_pospos=fltarr(n_seasons,n_eots,n_seasons,n_eots)
eot_transition_matrix_pospos(*,*,*,*)=0.
eot_transition_matrix_posneg=fltarr(n_seasons,n_eots,n_seasons,n_eots)
eot_transition_matrix_posneg(*,*,*,*)=0.
eot_transition_matrix_negpos=fltarr(n_seasons,n_eots,n_seasons,n_eots)
eot_transition_matrix_negpos(*,*,*,*)=0.
eot_transition_matrix_negneg=fltarr(n_seasons,n_eots,n_seasons,n_eots)
eot_transition_matrix_negneg(*,*,*,*)=0.
FOR i=0,n_seasons-1 DO BEGIN
   print,' -> '+STRTRIM(STRING(i+1),1)
   FOR j=0,n_eots-1 DO BEGIN
      this_base_eot=REFORM(silo_smeans_eots(i,*,j))
      FOR k=0,n_seasons-1 DO BEGIN
         FOR m=0,n_eots-1 DO BEGIN
            IF i ge k THEN BEGIN
               shift=1
               temp_base_eot=this_base_eot(0:n_years-shift-1)
            ENDIF ELSE BEGIN
               shift=0
               temp_base_eot=this_base_eot
            ENDELSE
            this_trans_eot=REFORM(silo_smeans_eots(k,shift:n_years-1,m))
            base_rank=fltarr(n_years-shift)
            trans_rank=fltarr(n_years-shift)
            FOR n=0,n_years-shift-1 DO BEGIN
               base_rank(n)=where(SORT(this_base_eot) eq n)*(100./FLOAT(n_years-shift))
               trans_rank(n)=where(SORT(this_trans_eot) eq n)*(100./FLOAT(n_years-shift))
            ENDFOR
;            temp_base_eot=SORT(temp_base_eot)*(100./FLOAT(n_years-shift))
;            this_trans_eot=SORT(this_trans_eot)*(100./FLOAT(n_years-shift))
            FOR n=0,n_years-shift-1 DO BEGIN
               IF base_rank(n) gt 50 and trans_rank(n) gt 50 THEN $
                  eot_transition_matrix_pospos(i,j,k,m)=eot_transition_matrix_pospos(i,j,k,m)+1
               IF base_rank(n) gt 50 and trans_rank(n) lt 50 THEN $
                  eot_transition_matrix_posneg(i,j,k,m)=eot_transition_matrix_posneg(i,j,k,m)+1
               IF base_rank(n) lt 50 and trans_rank(n) gt 50 THEN $
                  eot_transition_matrix_negpos(i,j,k,m)=eot_transition_matrix_negpos(i,j,k,m)+1
               IF base_rank(n) lt 50 and trans_rank(n) lt 50 THEN $
                  eot_transition_matrix_negneg(i,j,k,m)=eot_transition_matrix_negneg(i,j,k,m)+1
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
ENDFOR

col_heading_seasons='    '
col_heading_eots=''
FOR i=0,n_seasons-1 DO BEGIN
   col_heading_seasons=col_heading_seasons+'  '+season_periods(i)+'   '
   FOR j=0,n_eots-1 DO BEGIN
      col_heading_seasons=col_heading_seasons+'     '
      col_heading_eots=col_heading_eots+'   '+STRTRIM(STRING(j+1),1)+'   '
   ENDFOR
   col_heading_eots=col_heading_eots+'      '
ENDFOR

openw,lun,'/home/ss901165/idl/queensland/eots/qld_eots_transition_probabilities_silo025.smeans_yearone.txt',/get_lun
printf,lun,'                EOT next season    '+col_heading_seasons
printf,lun,'                                  '+col_heading_eots
printf,lun,'EOT this season'
printf,lun,'--------------------'
FOR i=0,n_seasons-1 DO BEGIN
   printf,lun,season_periods(i)
   FOR j=0,n_eots-1 DO BEGIN
;      output_string='       '+STRTRIM(STRING(j+1),1)+'   +ve  ('+STRMID(STRTRIM(STRING(N_ELEMENTS(where(silo_smeans_eots(i,*,j) gt 0))/FLOAT(n_years)),1),0,4)+')    +ve       '
      output_string='       '+STRTRIM(STRING(j+1),1)+'   +ve  ('+STRMID(STRTRIM(STRING(0.50),1),0,4)+')    +ve       '
      FOR k=0,n_seasons-1 DO BEGIN
         FOR m=0,n_eots-1 DO BEGIN
            add=0.00
            IF eot_transition_matrix_pospos(i,j,k,m) gt eot_transition_matrix_posneg(i,j,k,m) THEN $
               add=0.01
            output_string=output_string+STRMID(STRTRIM(STRING(eot_transition_matrix_pospos(i,j,k,m)/$
                                                              FLOAT(eot_transition_matrix_pospos(i,j,k,m)+eot_transition_matrix_posneg(i,j,k,m)))+add,1),0,4)+'   '
         ENDFOR
         output_string=output_string+'      '         
      ENDFOR
      printf,lun,output_string
      output_string='                          -ve       '
      FOR k=0,n_seasons-1 DO BEGIN
         FOR m=0,n_eots-1 DO BEGIN
            add=0.00
            IF eot_transition_matrix_pospos(i,j,k,m) lt eot_transition_matrix_posneg(i,j,k,m) THEN $
               add=0.01
            output_string=output_string+STRMID(STRTRIM(STRING(eot_transition_matrix_posneg(i,j,k,m)/$
                                                              FLOAT(eot_transition_matrix_pospos(i,j,k,m)+eot_transition_matrix_posneg(i,j,k,m)))+add,1),0,4)+'   '
         ENDFOR
         output_string=output_string+'      '
      ENDFOR
      printf,lun,output_string
;      output_string='       '+STRTRIM(STRING(j+1),1)+'   -ve  ('+STRMID(STRTRIM(STRING(N_ELEMENTS(where(silo_smeans_eots(i,*,j) lt 0))/FLOAT(n_years)),1),0,4)+')    +ve       '
      output_string='       '+STRTRIM(STRING(j+1),1)+'   -ve  ('+STRMID(STRTRIM(STRING(0.50),1),0,4)+')    +ve       '
      FOR k=0,n_seasons-1 DO BEGIN
         FOR m=0,n_eots-1 DO BEGIN
            add=0.00
            IF eot_transition_matrix_negpos(i,j,k,m) gt eot_transition_matrix_negneg(i,j,k,m) THEN $
               add=0.01
            output_string=output_string+STRMID(STRTRIM(STRING(eot_transition_matrix_negpos(i,j,k,m)/$
                                                              FLOAT(eot_transition_matrix_negpos(i,j,k,m)+eot_transition_matrix_negneg(i,j,k,m)))+add,1),0,4)+'   '
         ENDFOR
         output_string=output_string+'      '
      ENDFOR
      printf,lun,output_string
      output_string='                          -ve       '
      FOR k=0,n_seasons-1 DO BEGIN
         FOR m=0,n_eots-1 DO BEGIN
            add=0.00
            IF eot_transition_matrix_negpos(i,j,k,m) lt eot_transition_matrix_negneg(i,j,k,m) THEN $
               add=0.01
            output_string=output_string+STRMID(STRTRIM(STRING(eot_transition_matrix_negneg(i,j,k,m)/$
                                                              FLOAT(eot_transition_matrix_negpos(i,j,k,m)+eot_transition_matrix_negneg(i,j,k,m)))+add,1),0,4)+'   '
         ENDFOR
         output_string=output_string+'      '
      ENDFOR
      printf,lun,output_string
      printf,lun
   ENDFOR
   printf,lun,'----------------'
ENDFOR

free_lun,lun

STOP
END

