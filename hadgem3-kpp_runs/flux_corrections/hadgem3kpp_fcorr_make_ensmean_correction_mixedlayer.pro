PRO hadgem3kpp_fcorr_make_ensmean_correction_mixedlayer

; Make an ensemble-mean heat correction file that contains both
; the normal fcorr_withz-type correction and a separate, single-level
; correction for the average correction within the mixed layer.

kpp_runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6',$
          'xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
n_runs=N_ELEMENTS(kpp_runs)

corrected_indir='/export/mango/data-10/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'

kpp_times=['0045'];,'0075','0105','0135','0165','0195','0225','0255','0285','0315','0345','0375']
n_times=N_ELEMENTS(kpp_times)

fcorr_indir='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections'

FOR i=0,n_runs-1 DO BEGIN
   print,'--> '+kpp_runs(i)
   fcorr_infile=fcorr_indir+'/'+kpp_runs(i)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
   
   longitude=OPEN_AND_EXTRACT(fcorr_infile,'longitude')
   latitude=OPEN_AND_EXTRACT(fcorr_infile,'latitude')
   z=OPEN_AND_EXTRACT(fcorr_infile,'z')
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   n_z=N_ELEMENTS(z)
   IF i eq 0 THEN BEGIN
      ensmean_mlcorr=fltarr(n_lon,n_lat,n_times)
      mlcorr=fltarr(n_runs,n_lon,n_lat,n_times)
   ENDIF

   fcorr=OPEN_AND_EXTRACT(fcorr_infile,'fcorr')                         

   FOR j=0,n_times-1 DO BEGIN
      corrected_infile=corrected_indir+'/'+kpp_runs(i)+'/KPPocean_'+kpp_times(j)+'_means.nc'
      
      corrected_hmix=OPEN_AND_EXTRACT(corrected_infile,'hmix')

      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_lat-1 DO BEGIN
            IF corrected_hmix(k,m,0) le 1e10 THEN BEGIN
               dz_total=0
               FOR n=0,29 DO BEGIN
                  FOR p=0,n_z-1 DO BEGIN
                     IF ABS(z(p)) le ABS(corrected_hmix(k,m,n)) THEN BEGIN
                        IF p eq 0 THEN BEGIN
                           dz=2*ABS(z(0))
                        ENDIF ELSE $
                           dz=ABS(z(p))-ABS(z(p-1))
                        mlcorr(i,k,m,j)=mlcorr(i,k,m,j)+fcorr(k,m,p,j)*dz
                        dz_total=dz+dz_total
                     ENDIF
                  ENDFOR
               ENDFOR
               mlcorr(i,k,m,j)=mlcorr(i,k,m,j)/FLOAT(dz_total)
            ENDIF
         ENDFOR
      ENDFOR
   ENDFOR
   ensmean_mlcorr(*,*,*)=ensmean_mlcorr(*,*,*)+mlcorr(i,*,*,*)/FLOAT(n_runs)
ENDFOR

STOP
END

               
                  
