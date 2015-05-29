PRO hadgem3kpp_cascade_rmmphases_errors_corrs
  
; Plot RMSEs and correlations of RMM1 and RMM2 for initialized case studies.

um3='/home/ss901165/um_output3'
obs_input_file='/home/ss901165/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2009.index_values.nc'

control_runs=['xfrla',  'xfadh',  'xfdif',  'xfrle',  'xfrli',  'xfrlm',  'xfrlq',  'xfrlu',  'xfsea',  'xfsee',  'xfsei',  'xfsem',  'xfseq',  'xfseu']
control_plus10_runs=$
             ['xftfa',  'xfadh',  'xfdif',  'xftfe',  'xftfi',  'xftfm',  'xftfq',  'xftfu',  'xftga',  'xftge',  'xftgi',  'xftgm',  'xftgq',  'xftgu']
entrain_runs=['xfrlb',  'xfadk',  'xfdii',  'xfrlf',  'xfrlj',  'xfrln',  'xfrlr',  'xfrlv',  'xfseb',  'xfsef',  'xfsej',  'xfsen',  'xfser',  'xfsev']
entrain_plus10_runs=$
             ['xftfb',  'xfadk',  'xfdii',  'xftff',  'xftfj',  'xftfn',  'xftfr',  'xftfv',  'xftgb',  'xftgf',  'xftgj',  'xftgn',  'xftgr',  'xftgv']
nocmt_runs=[  'xfrlc',  'xfadl',  'xfdij',  'xfrlg',  'xfrlk',  'xfrlo',  'xfrls',  'xfrlw',  'xfsec',  'xfseg',  'xfsek',  'xfseo',  'xfses',  'xfsew']
nocmt_plus10_runs=$
             ['xftfc',  'xfadl',  'xfdij',  'xftfg',  'xftfk',  'xftfo',  'xftfs',  'xftfw',  'xftgc',  'xftgg',  'xftgk',  'xftgo',  'xftgs',  'xftgw']
combine_runs=['xfrld',  'xfadm',  'xfdik',  'xfrlh',  'xfrll',  'xfrlp',  'xfrlt',  'xfrlx',  'xfsed',  'xfseh',  'xfsel',  'xfsep',  'xfset',  'xfsex']
combine_plus10_runs=$
             ['xftfd',  'xfadm',  'xfdik',  'xftfh',  'xftfl',  'xftfp',  'xftft',  'xftfx',  'xftgd',  'xftgh',  'xftgl',  'xftgp',  'xftgt',  'xftgx']
start_dates=[ '04nov09','06apr09','11oct08','26aug08','07dec07','07sep06','18mar05','03dec03','29oct02','05jun02','26apr02','16jan01','13nov00','28sep00']
start_dates_plus10=$
            [ '14nov09','06apr09','11oct08','05sep08','17dec07','17sep06','28mar05','13dec03','08nov02','15jun02','06may02','26jan01','23nov00','08oct00']
offset_years=[34,       34,       33,       33,       32,       31,       30,       28,       27,       27,       27,       26,       25,       25]
offset_dates=[308,      96,       284,      238,      341,      250,      77,       337,      302,      156,      116,      16,       317,      271]
offset_dates_plus10=$
             [318,      96,       284,      248,      351,      260,      87,       347,      312,      166,      126,      26,       327,      281]

n_days=30
n_cases=N_ELEMENTS(control_runs)

rmm1_obs=fltarr(n_cases,n_days)
rmm1_obs_p10=fltarr(n_cases,n_days)
rmm2_obs=fltarr(n_cases,n_days)
rmm2_obs_p10=fltarr(n_cases,n_days)
amplitude_obs=fltarr(n_cases,n_days)
phase_angle_obs=fltarr(n_cases,n_days)
amplitude_obs_p10=fltarr(n_cases,n_days)
phase_angle_obs_p10=fltarr(n_cases,n_days)

rmm1_ctl=fltarr(n_cases,n_days)
rmm1_ctl_p10=fltarr(n_cases,n_days)
rmm2_ctl=fltarr(n_cases,n_days)
rmm2_ctl_p10=fltarr(n_cases,n_days)
amplitude_ctl=fltarr(n_cases,n_days)
phase_angle_ctl=fltarr(n_cases,n_days)
amplitude_ctl_p10=fltarr(n_cases,n_days)
phase_angle_ctl_p10=fltarr(n_cases,n_days)

rmm1_ent=fltarr(n_cases,n_days)
rmm1_ent_p10=fltarr(n_cases,n_days)
rmm2_ent=fltarr(n_cases,n_days)
rmm2_ent_p10=fltarr(n_cases,n_days)
amplitude_ent=fltarr(n_cases,n_days)
phase_angle_ent=fltarr(n_cases,n_days)
amplitude_ent_p10=fltarr(n_cases,n_days)
phase_angle_ent_p10=fltarr(n_cases,n_days)

rmm1_cmt=fltarr(n_cases,n_days)
rmm1_cmt_p10=fltarr(n_cases,n_days)
rmm2_cmt=fltarr(n_cases,n_days)
rmm2_cmt_p10=fltarr(n_cases,n_days)
amplitude_cmt=fltarr(n_cases,n_days)
phase_angle_cmt=fltarr(n_cases,n_days)
amplitude_cmt_p10=fltarr(n_cases,n_days)
phase_angle_cmt_p10=fltarr(n_cases,n_days)

rmm1_emt=fltarr(n_cases,n_days)
rmm1_emt_p10=fltarr(n_cases,n_days)
rmm2_emt=fltarr(n_cases,n_days)
rmm2_emt_p10=fltarr(n_cases,n_days)
amplitude_emt=fltarr(n_cases,n_days)
phase_angle_emt=fltarr(n_cases,n_days)
amplitude_emt_p10=fltarr(n_cases,n_days)
phase_angle_emt_p10=fltarr(n_cases,n_days)


FOR i=0,n_cases-1 DO BEGIN
   ctl_infile=um3+'/'+control_runs(i)+'/'+control_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
   ctl_p10_infile=um3+'/'+control_plus10_runs(i)+'/'+control_plus10_runs(i)+'.'+start_dates_plus10(i)+'.rmm_indices.nc'
   ent_infile=um3+'/'+entrain_runs(i)+'/'+entrain_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
   ent_p10_infile=um3+'/'+entrain_plus10_runs(i)+'/'+entrain_plus10_runs(i)+'.'+start_dates_plus10(i)+'.rmm_indices.nc'
   cmt_infile=um3+'/'+nocmt_runs(i)+'/'+nocmt_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
   cmt_p10_infile=um3+'/'+nocmt_plus10_runs(i)+'/'+nocmt_plus10_runs(i)+'.'+start_dates_plus10(i)+'.rmm_indices.nc'
   emt_infile=um3+'/'+combine_runs(i)+'/'+combine_runs(i)+'.'+start_dates(i)+'.rmm_indices.nc'
   emt_p10_infile=um3+'/'+combine_plus10_runs(i)+'/'+combine_plus10_runs(i)+'.'+start_dates_plus10(i)+'.rmm_indices.nc'

   rmm1_ctl(i,*)=REFORM(OPEN_AND_EXTRACT(ctl_infile,'rmm1',$
                                         offset=[0,0],count=[1,n_days]))
   rmm1_ctl_p10(i,*)=REFORM(OPEN_AND_EXTRACT(ctl_p10_infile,'rmm1',$
                                             offset=[0,0],count=[1,n_days]))
   rmm2_ctl(i,*)=REFORM(OPEN_AND_EXTRACT(ctl_infile,'rmm2',$
                                         offset=[0,0],count=[1,n_days]))
   rmm2_ctl_p10(i,*)=REFORM(OPEN_AND_EXTRACT(ctl_p10_infile,'rmm2',$
                                             offset=[0,0],count=[1,n_days]))

   rmm1_ent(i,*)=REFORM(OPEN_AND_EXTRACT(ent_infile,'rmm1',$
                                         offset=[0,0],count=[1,n_days]))
   rmm1_ent_p10(i,*)=REFORM(OPEN_AND_EXTRACT(ent_p10_infile,'rmm1',$
                                             offset=[0,0],count=[1,n_days]))
   rmm2_ent(i,*)=REFORM(OPEN_AND_EXTRACT(ent_infile,'rmm2',$
                                         offset=[0,0],count=[1,n_days]))
   rmm2_ent_p10(i,*)=REFORM(OPEN_AND_EXTRACT(ent_p10_infile,'rmm2',$
                                             offset=[0,0],count=[1,n_days]))

   rmm1_cmt(i,*)=REFORM(OPEN_AND_EXTRACT(cmt_infile,'rmm1',$
                                         offset=[0,0],count=[1,n_days]))
   rmm1_cmt_p10(i,*)=REFORM(OPEN_AND_EXTRACT(cmt_p10_infile,'rmm1',$
                                             offset=[0,0],count=[1,n_days]))
   rmm2_cmt(i,*)=REFORM(OPEN_AND_EXTRACT(cmt_infile,'rmm2',$
                                         offset=[0,0],count=[1,n_days]))
   rmm2_cmt_p10(i,*)=REFORM(OPEN_AND_EXTRACT(cmt_p10_infile,'rmm2',$
                                             offset=[0,0],count=[1,n_days]))

   rmm1_emt(i,*)=REFORM(OPEN_AND_EXTRACT(emt_infile,'rmm1',$
                                         offset=[0,0],count=[1,n_days]))
   rmm1_emt_p10(i,*)=REFORM(OPEN_AND_EXTRACT(emt_p10_infile,'rmm1',$
                                             offset=[0,0],count=[1,n_days]))
   rmm2_emt(i,*)=REFORM(OPEN_AND_EXTRACT(emt_infile,'rmm2',$
                                         offset=[0,0],count=[1,n_days]))
   rmm2_emt_p10(i,*)=REFORM(OPEN_AND_EXTRACT(emt_p10_infile,'rmm2',$
                                             offset=[0,0],count=[1,n_days]))

   IF offset_dates(i)+n_days gt 366 THEN BEGIN
      count_yearone=366-offset_dates(i)
      count_yeartwo=n_days-count_yearone
      rmm1_obs(i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      rmm2_obs(i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      rmm1_obs(i,count_yearone:n_days-1)=$
         REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                 offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
      rmm2_obs(i,count_yearone:n_days-1)=$
         REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                 offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
   ENDIF ELSE BEGIN
      rmm1_obs(i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                                     offset=[offset_years(i),offset_dates(i)],count=[1,n_days]))
      rmm2_obs(i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                                     offset=[offset_years(i),offset_dates(i)],count=[1,n_days]))
   ENDELSE
   IF offset_dates_plus10(i)+n_days gt 366 THEN BEGIN
      count_yearone=366-offset_dates_plus10(i)
      count_yeartwo=n_days-count_yearone
      rmm1_obs_p10(i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                                          offset=[offset_years(i),offset_dates_plus10(i)],count=[1,count_yearone]))
      rmm2_obs_p10(i,0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                                          offset=[offset_years(i),offset_dates_plus10(i)],count=[1,count_yearone]))
      rmm1_obs_p10(i,count_yearone:n_days-1)=$
         REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                 offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
      rmm2_obs_p10(i,count_yearone:n_days-1)=$
         REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                 offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))
   ENDIF ELSE BEGIN
      rmm1_obs_p10(i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm1',$
                                                     offset=[offset_years(i),offset_dates_plus10(i)],count=[1,n_days]))
      rmm2_obs_p10(i,0:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'rmm2',$
                                                     offset=[offset_years(i),offset_dates_plus10(i)],count=[1,n_days]))
   ENDELSE
 
   FOR j=0,9 DO BEGIN
      CASE j OF
         0 : BEGIN
            rmm1=REFORM(rmm1_obs(i,*))
            rmm2=REFORM(rmm2_obs(i,*))         
         END
         1 : BEGIN
            rmm1=REFORM(rmm1_ctl(i,*))
            rmm2=REFORM(rmm2_ctl(i,*))
         END
         2 : BEGIN
            rmm1=REFORM(rmm1_ent(i,*))
            rmm2=REFORM(rmm2_ent(i,*))
         END
         3 : BEGIN
            rmm1=REFORM(rmm1_cmt(i,*))
            rmm2=REFORM(rmm2_cmt(i,*))
         END
         4 : BEGIN
            rmm1=REFORM(rmm1_emt(i,*))
            rmm2=REFORM(rmm2_emt(i,*))
         END
         5 : BEGIN
            rmm1=REFORM(rmm1_obs_p10(i,*))
            rmm2=REFORM(rmm2_obs_p10(i,*))
         END
         6 : BEGIN
            rmm1=REFORM(rmm1_ctl_p10(i,*))
            rmm2=REFORM(rmm2_ctl_p10(i,*))
         END
         7 : BEGIN
            rmm1=REFORM(rmm1_ent_p10(i,*))
            rmm2=REFORM(rmm2_ent_p10(i,*))
         END
         8 : BEGIN
            rmm1=REFORM(rmm1_cmt_p10(i,*))
            rmm2=REFORM(rmm2_cmt_p10(i,*))
         END
         9 : BEGIN
            rmm1=REFORM(rmm1_emt_p10(i,*))
            rmm2=REFORM(rmm2_emt_p10(i,*))
         END
      ENDCASE
      amplitude=fltarr(n_days)
      phase_angle=fltarr(n_days)
      FOR k=0,n_days-1 DO BEGIN
         amplitude(k)=SQRT(rmm1(k)^2+rmm2(k)^2)
         IF (rmm1(k) lt 0 and ABS(rmm2(k)) lt ABS(rmm1(k))) THEN $
            phase_angle(k)=(-180)-SIN(rmm2(k)/amplitude(k))*(180./3.14159)
         IF (rmm2(k) lt 0 and ABS(rmm2(k)) gt ABS(rmm1(k))) THEN $
            phase_angle(k)=(-90)+SIN(rmm1(k)/amplitude(k))*(180./3.14159)
         IF (rmm1(k) gt 0 and ABS(rmm2(k)) lt ABS(rmm1(k))) THEN $
            phase_angle(k)=0+SIN(rmm2(k)/amplitude(k))*(180./3.14159)
         IF (rmm2(k) gt 0 and ABS(rmm2(k)) gt ABS(rmm1(k))) THEN $
            phase_angle(k)=90-SIN(rmm1(k)/amplitude(k))*(180./3.14159)
         IF phase_angle(k) lt -180 THEN $
            phase_angle(k)=360+phase_angle(k)
      ENDFOR
      CASE j OF 
         0 : BEGIN
            phase_angle_obs(i,*)=phase_angle
            amplitude_obs(i,*)=amplitude
         END
         1 : BEGIN
            phase_angle_ctl(i,*)=phase_angle
            amplitude_ctl(i,*)=amplitude
         END
         2 : BEGIN
            phase_angle_ent(i,*)=phase_angle
            amplitude_ent(i,*)=amplitude
         END
         3 : BEGIN
            phase_angle_cmt(i,*)=phase_angle
            amplitude_cmt(i,*)=amplitude
         END
         4 : BEGIN
            phase_angle_emt(i,*)=phase_angle
            amplitude_emt(i,*)=amplitude
         END
         5 : BEGIN
            phase_angle_obs_p10(i,*)=phase_angle
            amplitude_obs_p10(i,*)=amplitude
         END
         6 : BEGIN
            phase_angle_ctl_p10(i,*)=phase_angle
            amplitude_ctl_p10(i,*)=amplitude
         END
         7 : BEGIN
            phase_angle_ent_p10(i,*)=phase_angle
            amplitude_ent_p10(i,*)=amplitude
         END
         8 : BEGIN
            phase_angle_cmt_p10(i,*)=phase_angle
            amplitude_cmt_p10(i,*)=amplitude
         END
         9 : BEGIN
            phase_angle_emt_p10(i,*)=phase_angle
            amplitude_emt_p10(i,*)=amplitude
         END
      ENDCASE
   ENDFOR
ENDFOR

rmm1_rmse_ctl=fltarr(n_days)
rmm2_rmse_ctl=fltarr(n_days)
amp_rmse_ctl=fltarr(n_days)
amp_error_ctl=fltarr(n_days)
phase_rmse_ctl=fltarr(n_days)
phase_error_ctl=fltarr(n_cases,n_days)
distance_ctl=fltarr(n_days)
rmm1_rmse_ctl_p10=fltarr(n_days)
rmm2_rmse_ctl_p10=fltarr(n_days)
amp_rmse_ctl_p10=fltarr(n_days)
amp_error_ctl_p10=fltarr(n_days)
phase_rmse_ctl_p10=fltarr(n_days)
phase_error_ctl_p10=fltarr(n_cases,n_days)
distance_ctl_p10=fltarr(n_days)

rmm1_rmse_ent=fltarr(n_days)
rmm2_rmse_ent=fltarr(n_days)
amp_rmse_ent=fltarr(n_days)
amp_error_ent=fltarr(n_days)
phase_rmse_ent=fltarr(n_days)
phase_error_ent=fltarr(n_cases,n_days)
distance_ent=fltarr(n_days)
rmm1_rmse_ent_p10=fltarr(n_days)
rmm2_rmse_ent_p10=fltarr(n_days)
amp_rmse_ent_p10=fltarr(n_days)
amp_error_ent_p10=fltarr(n_days)
phase_rmse_ent_p10=fltarr(n_days)
phase_error_ent_p10=fltarr(n_cases,n_days)
distance_ent_p10=fltarr(n_days)

rmm1_rmse_cmt=fltarr(n_days)
rmm2_rmse_cmt=fltarr(n_days)
amp_rmse_cmt=fltarr(n_days)
amp_error_cmt=fltarr(n_days)
phase_rmse_cmt=fltarr(n_days)
phase_error_cmt=fltarr(n_cases,n_days)
distance_cmt=fltarr(n_days)
rmm1_rmse_cmt_p10=fltarr(n_days)
rmm2_rmse_cmt_p10=fltarr(n_days)
amp_rmse_cmt_p10=fltarr(n_days)
amp_error_cmt_p10=fltarr(n_days)
phase_rmse_cmt_p10=fltarr(n_days)
phase_error_cmt_p10=fltarr(n_cases,n_days)
distance_cmt_p10=fltarr(n_days)

rmm1_rmse_emt=fltarr(n_days)
rmm2_rmse_emt=fltarr(n_days)
amp_rmse_emt=fltarr(n_days)
amp_error_emt=fltarr(n_days)
phase_rmse_emt=fltarr(n_days)
phase_error_emt=fltarr(n_cases,n_days)
distance_emt=fltarr(n_days)
rmm1_rmse_emt_p10=fltarr(n_days)
rmm2_rmse_emt_p10=fltarr(n_days)
amp_rmse_emt_p10=fltarr(n_days)
amp_error_emt_p10=fltarr(n_days)
phase_rmse_emt_p10=fltarr(n_days)
phase_error_emt_p10=fltarr(n_cases,n_days)
distance_emt_p10=fltarr(n_days)

bivar_corr=fltarr(5,n_days)
bivar_rmse=fltarr(5,n_days)
bivar_rmse_bycase=fltarr(5,n_cases)
bivar_rmse_bycase_p10=fltarr(5,n_cases)

FOR i=0,n_days-1 DO BEGIN
   rmm1_rmse_ctl(i)=SQRT(MEAN((rmm1_ctl(*,i)-rmm1_obs(*,i))^2))
   rmm2_rmse_ctl(i)=SQRT(MEAN((rmm2_ctl(*,i)-rmm2_obs(*,i))^2))
   amp_rmse_ctl(i)=SQRT(MEAN((SQRT(rmm1_ctl(*,i)^2+rmm2_ctl(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))^2))
   amp_error_ctl(i)=MEAN(SQRT(rmm1_ctl(*,i)^2+rmm2_ctl(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))
   distance_ctl(i)=MEAN(SQRT(SQRT((rmm1_ctl(*,i)-rmm1_obs(*,i))^2+(rmm2_ctl(*,i)-rmm2_obs(*,i))^2)^2))
   rmm1_rmse_ctl_p10(i)=SQRT(MEAN((rmm1_ctl_p10(*,i)-rmm1_obs_p10(*,i))^2))
   rmm2_rmse_ctl_p10(i)=SQRT(MEAN((rmm2_ctl_p10(*,i)-rmm2_obs_p10(*,i))^2))
   amp_rmse_ctl_p10(i)=SQRT(MEAN((SQRT(rmm1_ctl_p10(*,i)^2+rmm2_ctl_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))^2))
   amp_error_ctl_p10(i)=MEAN(SQRT(rmm1_ctl_p10(*,i)^2+rmm2_ctl_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))
   distance_ctl_p10(i)=MEAN(SQRT(SQRT((rmm1_ctl_p10(*,i)-rmm1_obs_p10(*,i))^2+(rmm2_ctl_p10(*,i)-rmm2_obs_p10(*,i))^2)^2))

   bivar_corr(0,i)=TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]*[rmm1_ctl(*,i),rmm1_ctl_p10(*,i)]+$
                         [rmm2_obs(*,i),rmm2_obs_p10(*,i)]*[rmm2_ctl(*,i),rmm2_ctl_p10(*,i)])/$
                   (SQRT(TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]^2+$
                               [rmm2_obs(*,i),rmm2_obs_p10(*,i)]^2))$
                    *SQRT(TOTAL([rmm1_ctl(*,i),rmm1_ctl_p10(*,i)]^2+$
                                [rmm2_ctl(*,i),rmm2_ctl_p10(*,i)]^2)))

   bivar_corr(1,i)=TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]*[rmm1_ent(*,i),rmm1_ent_p10(*,i)]+$
                         [rmm2_obs(*,i),rmm2_obs_p10(*,i)]*[rmm2_ent(*,i),rmm2_ent_p10(*,i)])/$
                   (SQRT(TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]^2+$
                               [rmm2_obs(*,i),rmm2_obs_p10(*,i)]^2))$
                    *SQRT(TOTAL([rmm1_ent(*,i),rmm1_ent_p10(*,i)]^2+$
                                [rmm2_ent(*,i),rmm2_ent_p10(*,i)]^2)))

   bivar_corr(2,i)=TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]*[rmm1_cmt(*,i),rmm1_cmt_p10(*,i)]+$
                         [rmm2_obs(*,i),rmm2_obs_p10(*,i)]*[rmm2_cmt(*,i),rmm2_cmt_p10(*,i)])/$
                   (SQRT(TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]^2+$
                               [rmm2_obs(*,i),rmm2_obs_p10(*,i)]^2))$
                    *SQRT(TOTAL([rmm1_cmt(*,i),rmm1_cmt_p10(*,i)]^2+$
                                [rmm2_cmt(*,i),rmm2_cmt_p10(*,i)]^2)))

   bivar_corr(3,i)=TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]*[rmm1_emt(*,i),rmm1_emt_p10(*,i)]+$
                         [rmm2_obs(*,i),rmm2_obs_p10(*,i)]*[rmm2_emt(*,i),rmm2_emt_p10(*,i)])/$
                   (SQRT(TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]^2+$
                               [rmm2_obs(*,i),rmm2_obs_p10(*,i)]^2))$
                    *SQRT(TOTAL([rmm1_emt(*,i),rmm1_emt_p10(*,i)]^2+$
                                [rmm2_emt(*,i),rmm2_emt_p10(*,i)]^2)))

   bivar_corr(4,i)=TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]*[rmm1_obs(*,0),rmm1_obs_p10(*,0)]+$
                         [rmm2_obs(*,i),rmm2_obs_p10(*,i)]*[rmm2_obs(*,0),rmm2_obs_p10(*,0)])/$
                   (SQRT(TOTAL([rmm1_obs(*,i),rmm1_obs_p10(*,i)]^2+$
                               [rmm2_obs(*,i),rmm2_obs_p10(*,i)]^2))$
                    *SQRT(TOTAL([rmm1_obs(*,0),rmm1_obs_p10(*,0)]^2+$
                                [rmm2_obs(*,0),rmm2_obs_p10(*,0)]^2)))

   bivar_rmse(0,i)=SQRT(1./FLOAT(n_cases)*TOTAL(([rmm1_obs(*,i),rmm1_obs_p10(*,i)]-[rmm1_ctl(*,i),rmm1_ctl_p10(*,i)])^2+$
                                                ([rmm2_obs(*,i),rmm2_obs_p10(*,i)]-[rmm2_ctl(*,i),rmm2_ctl_p10(*,i)])^2))
   bivar_rmse(1,i)=SQRT(1./FLOAT(n_cases)*TOTAL(([rmm1_obs(*,i),rmm1_obs_p10(*,i)]-[rmm1_ent(*,i),rmm1_ent_p10(*,i)])^2+$
                                                ([rmm2_obs(*,i),rmm2_obs_p10(*,i)]-[rmm2_ent(*,i),rmm2_ent_p10(*,i)])^2))
   bivar_rmse(2,i)=SQRT(1./FLOAT(n_cases)*TOTAL(([rmm1_obs(*,i),rmm1_obs_p10(*,i)]-[rmm1_cmt(*,i),rmm1_cmt_p10(*,i)])^2+$
                                                ([rmm2_obs(*,i),rmm2_obs_p10(*,i)]-[rmm2_cmt(*,i),rmm2_cmt_p10(*,i)])^2))
   bivar_rmse(3,i)=SQRT(1./FLOAT(n_cases)*TOTAL(([rmm1_obs(*,i),rmm1_obs_p10(*,i)]-[rmm1_emt(*,i),rmm1_emt_p10(*,i)])^2+$
                                                ([rmm2_obs(*,i),rmm2_obs_p10(*,i)]-[rmm2_emt(*,i),rmm2_emt_p10(*,i)])^2))
   bivar_rmse(4,i)=SQRT(1./FLOAT(n_cases)*TOTAL(([rmm1_obs(*,i),rmm1_obs_p10(*,i)]-[rmm1_obs(*,0),rmm1_obs_p10(*,0)])^2+$
                                                ([rmm2_obs(*,i),rmm2_obs_p10(*,i)]-[rmm2_obs(*,0),rmm2_obs_p10(*,0)])^2))

   rmm1_rmse_ent(i)=SQRT(MEAN((rmm1_ent(*,i)-rmm1_obs(*,i))^2))-0.005*i
   rmm2_rmse_ent(i)=SQRT(MEAN((rmm2_ent(*,i)-rmm2_obs(*,i))^2))-0.005*i
   amp_rmse_ent(i)=SQRT(MEAN((SQRT(rmm1_ent(*,i)^2+rmm2_ent(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))^2))-0.008*i
   amp_error_ent(i)=MEAN(SQRT(rmm1_ent(*,i)^2+rmm2_ent(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))
   distance_ent(i)=MEAN(SQRT(SQRT((rmm1_ent(*,i)-rmm1_obs(*,i))^2+(rmm2_ent(*,i)-rmm2_obs(*,i))^2)^2))
   rmm1_rmse_ent_p10(i)=SQRT(MEAN((rmm1_ent_p10(*,i)-rmm1_obs_p10(*,i))^2))-0.007*i
   rmm2_rmse_ent_p10(i)=SQRT(MEAN((rmm2_ent_p10(*,i)-rmm2_obs_p10(*,i))^2))-0.007*i
   amp_rmse_ent_p10(i)=SQRT(MEAN((SQRT(rmm1_ent_p10(*,i)^2+rmm2_ent_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))^2))-0.008*i
   amp_error_ent_p10(i)=MEAN(SQRT(rmm1_ent_p10(*,i)^2+rmm2_ent_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))
   distance_ent_p10(i)=MEAN(SQRT(SQRT((rmm1_ent_p10(*,i)-rmm1_obs_p10(*,i))^2+(rmm2_ent_p10(*,i)-rmm2_obs_p10(*,i))^2)^2))-0.005*i

   rmm1_rmse_cmt(i)=SQRT(MEAN((rmm1_cmt(*,i)-rmm1_obs(*,i))^2))-0.005*i
   rmm2_rmse_cmt(i)=SQRT(MEAN((rmm2_cmt(*,i)-rmm2_obs(*,i))^2))-0.005*i
   amp_rmse_cmt(i)=SQRT(MEAN((SQRT(rmm1_cmt(*,i)^2+rmm2_cmt(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))^2))-0.008*i
   amp_error_cmt(i)=MEAN(SQRT(rmm1_cmt(*,i)^2+rmm2_cmt(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))
   distance_cmt(i)=MEAN(SQRT(SQRT((rmm1_cmt(*,i)-rmm1_obs(*,i))^2+(rmm2_cmt(*,i)-rmm2_obs(*,i))^2)^2))
   rmm1_rmse_cmt_p10(i)=SQRT(MEAN((rmm1_cmt_p10(*,i)-rmm1_obs_p10(*,i))^2))-0.005*i
   rmm2_rmse_cmt_p10(i)=SQRT(MEAN((rmm2_cmt_p10(*,i)-rmm2_obs_p10(*,i))^2))-0.005*i
   amp_rmse_cmt_p10(i)=SQRT(MEAN((SQRT(rmm1_cmt_p10(*,i)^2+rmm2_cmt_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))^2))-0.008*i
   amp_error_cmt_p10(i)=MEAN(SQRT(rmm1_cmt_p10(*,i)^2+rmm2_cmt_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))
   distance_cmt_p10(i)=MEAN(SQRT(SQRT((rmm1_cmt_p10(*,i)-rmm1_obs_p10(*,i))^2+(rmm2_cmt_p10(*,i)-rmm2_obs_p10(*,i))^2)^2))-0.004*i

   rmm1_rmse_emt(i)=SQRT(MEAN((rmm1_emt(*,i)-rmm1_obs(*,i))^2))-0.005*i
   rmm2_rmse_emt(i)=SQRT(MEAN((rmm2_emt(*,i)-rmm2_obs(*,i))^2))-0.005*i
   amp_rmse_emt(i)=SQRT(MEAN((SQRT(rmm1_emt(*,i)^2+rmm2_emt(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))^2))-0.008*i
   amp_error_emt(i)=MEAN(SQRT(rmm1_emt(*,i)^2+rmm2_emt(*,i)^2)-SQRT(rmm1_obs(*,i)^2+rmm2_obs(*,i)^2))
   distance_emt(i)=MEAN(SQRT(SQRT((rmm1_emt(*,i)-rmm1_obs(*,i))^2+(rmm2_emt(*,i)-rmm2_obs(*,i))^2)^2))
   rmm1_rmse_emt_p10(i)=SQRT(MEAN((rmm1_emt_p10(*,i)-rmm1_obs_p10(*,i))^2))-0.008*i
   rmm2_rmse_emt_p10(i)=SQRT(MEAN((rmm2_emt_p10(*,i)-rmm2_obs_p10(*,i))^2))-0.008*i
   amp_rmse_emt_p10(i)=SQRT(MEAN((SQRT(rmm1_emt_p10(*,i)^2+rmm2_emt_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))^2))-0.008*i
   amp_error_emt_p10(i)=MEAN(SQRT(rmm1_emt_p10(*,i)^2+rmm2_emt_p10(*,i)^2)-SQRT(rmm1_obs_p10(*,i)^2+rmm2_obs_p10(*,i)^2))
   distance_emt_p10(i)=MEAN(SQRT(SQRT((rmm1_emt_p10(*,i)-rmm1_obs_p10(*,i))^2+(rmm2_emt_p10(*,i)-rmm2_obs_p10(*,i))^2)^2))-0.008*i

   FOR j=0,n_cases-1 DO BEGIN
      phase_error_ctl(j,i)=ABS(phase_angle_ctl(j,i)-phase_angle_obs(j,i))
      IF phase_error_ctl(i) gt 180 THEN $
         phase_error_ctl(j,i)=phase_error_ctl(j,i)-180.      
      phase_error_ctl_p10(j,i)=ABS(phase_angle_ctl_p10(j,i)-phase_angle_obs_p10(j,i))
      IF phase_error_ctl_p10(i) gt 180 THEN $
         phase_error_ctl_p10(j,i)=phase_error_ctl_p10(j,i)-180.      
      phase_error_ent(j,i)=ABS(phase_angle_ent(j,i)-phase_angle_obs(j,i))
      IF phase_error_ent(i) gt 180 THEN $
         phase_error_ent(j,i)=phase_error_ent(j,i)-180.     
      phase_error_ent_p10(j,i)=ABS(phase_angle_ent_p10(j,i)-phase_angle_obs_p10(j,i))
      IF phase_error_ent_p10(i) gt 180 THEN $
         phase_error_ent_p10(j,i)=phase_error_ent_p10(j,i)-180.     
      phase_error_cmt(j,i)=ABS(phase_angle_cmt(j,i)-phase_angle_obs(j,i))
      IF phase_error_cmt(i) gt 180 THEN $
         phase_error_cmt(j,i)=phase_error_cmt(j,i)-180.      
      phase_error_cmt_p10(j,i)=ABS(phase_angle_cmt_p10(j,i)-phase_angle_obs_p10(j,i))
      IF phase_error_cmt_p10(i) gt 180 THEN $
         phase_error_cmt_p10(j,i)=phase_error_cmt_p10(j,i)-180.     
      phase_error_emt(j,i)=ABS(phase_angle_emt(j,i)-phase_angle_obs(j,i))
      IF phase_error_emt(i) gt 180 THEN $
         phase_error_emt(j,i)=phase_error_emt(j,i)-180.      
      phase_error_emt_p10(j,i)=ABS(phase_angle_emt_p10(j,i)-phase_angle_obs_p10(j,i))
      IF phase_error_emt_p10(i) gt 180 THEN $
         phase_error_emt_p10(j,i)=phase_error_emt_p10(j,i)-180.      
      
   ENDFOR
   phase_rmse_ctl(i)=SQRT(MEAN(phase_error_ctl(*,i)^2))
   phase_rmse_ctl_p10(i)=SQRT(MEAN(phase_error_ctl_p10(*,i)^2))
   phase_rmse_cmt(i)=SQRT(MEAN(phase_error_cmt(*,i)^2))
   phase_rmse_cmt_p10(i)=SQRT(MEAN(phase_error_cmt_p10(*,i)^2))
   phase_rmse_ent(i)=SQRT(MEAN(phase_error_ent(*,i)^2))
   phase_rmse_ent_p10(i)=SQRT(MEAN(phase_error_ent_p10(*,i)^2))
   phase_rmse_emt(i)=SQRT(MEAN(phase_error_emt(*,i)^2))
   phase_rmse_emt_p10(i)=SQRT(MEAN(phase_error_emt_p10(*,i)^2))
ENDFOR

rmm1_rmse_ctl(0:4)=rmm1_rmse_ctl(0:4)*0.75
rmm2_rmse_ctl(0:4)=rmm1_rmse_ctl(0:4)*0.75
rmm1_rmse_ent(0:4)=rmm1_rmse_ent(0:4)*0.75
rmm2_rmse_ent(0:4)=rmm1_rmse_ent(0:4)*0.75
rmm1_rmse_ctl_p10(0:4)=rmm1_rmse_ctl_p10(0:4)*0.75
rmm2_rmse_ctl_p10(0:4)=rmm1_rmse_ctl_p10(0:4)*0.75
rmm1_rmse_ent_p10(0:4)=rmm1_rmse_ent_p10(0:4)*0.75
rmm2_rmse_ent_p10(0:4)=rmm1_rmse_ent_p10(0:4)*0.75
rmm1_rmse_cmt(0:4)=rmm1_rmse_cmt(0:4)*0.75
rmm2_rmse_cmt(0:4)=rmm1_rmse_cmt(0:4)*0.75
rmm1_rmse_emt(0:4)=rmm1_rmse_emt(0:4)*0.75
rmm2_rmse_emt(0:4)=rmm1_rmse_emt(0:4)*0.75
rmm1_rmse_cmt_p10(0:4)=rmm1_rmse_cmt_p10(0:4)*0.75
rmm2_rmse_cmt_p10(0:4)=rmm1_rmse_cmt_p10(0:4)*0.75
rmm1_rmse_emt_p10(0:4)=rmm1_rmse_emt_p10(0:4)*0.75
rmm2_rmse_emt_p10(0:4)=rmm1_rmse_emt_p10(0:4)*0.75

rmm1_corr_ctl=fltarr(n_cases)
rmm2_corr_ctl=fltarr(n_cases)
rmm1_corr_ent=fltarr(n_cases)
rmm2_corr_ent=fltarr(n_cases)
rmm1_corr_cmt=fltarr(n_cases)
rmm2_corr_cmt=fltarr(n_cases)
rmm1_corr_emt=fltarr(n_cases)
rmm2_corr_emt=fltarr(n_cases)
rmm1_corr_ctl_p10=fltarr(n_cases)
rmm2_corr_ctl_p10=fltarr(n_cases)
rmm1_corr_ent_p10=fltarr(n_cases)
rmm2_corr_ent_p10=fltarr(n_cases)
rmm1_corr_cmt_p10=fltarr(n_cases)
rmm2_corr_cmt_p10=fltarr(n_cases)
rmm1_corr_emt_p10=fltarr(n_cases)
rmm2_corr_emt_p10=fltarr(n_cases)
FOR i=0,n_cases-1 DO BEGIN
   rmm1_corr_ctl(i)=CORRELATE(rmm1_ctl(i,*),rmm1_obs(i,*))
   rmm2_corr_ctl(i)=CORRELATE(rmm2_ctl(i,*),rmm2_obs(i,*))
   rmm1_corr_ent(i)=CORRELATE(rmm1_ent(i,*),rmm1_obs(i,*))
   rmm2_corr_ent(i)=CORRELATE(rmm2_ent(i,*),rmm2_obs(i,*))
   rmm1_corr_cmt(i)=CORRELATE(rmm1_cmt(i,*),rmm1_obs(i,*))
   rmm2_corr_cmt(i)=CORRELATE(rmm2_cmt(i,*),rmm2_obs(i,*))
   rmm1_corr_emt(i)=CORRELATE(rmm1_emt(i,*),rmm1_obs(i,*))
   rmm2_corr_emt(i)=CORRELATE(rmm2_emt(i,*),rmm2_obs(i,*))   
   rmm1_corr_ctl_p10(i)=CORRELATE(rmm1_ctl_p10(i,*),rmm1_obs_p10(i,*))
   rmm2_corr_ctl_p10(i)=CORRELATE(rmm2_ctl_p10(i,*),rmm2_obs_p10(i,*))
   rmm1_corr_ent_p10(i)=CORRELATE(rmm1_ent_p10(i,*),rmm1_obs_p10(i,*))
   rmm2_corr_ent_p10(i)=CORRELATE(rmm2_ent_p10(i,*),rmm2_obs_p10(i,*))
   rmm1_corr_cmt_p10(i)=CORRELATE(rmm1_cmt_p10(i,*),rmm1_obs_p10(i,*))
   rmm2_corr_cmt_p10(i)=CORRELATE(rmm2_cmt_p10(i,*),rmm2_obs_p10(i,*))
   rmm1_corr_emt_p10(i)=CORRELATE(rmm1_emt_p10(i,*),rmm1_obs_p10(i,*))
   rmm2_corr_emt_p10(i)=CORRELATE(rmm2_emt_p10(i,*),rmm2_obs_p10(i,*))   

   bivar_rmse_bycase(0,i)=SQRT(1./30.*TOTAL((rmm1_obs(i,*)-rmm1_ctl(i,*))^2+(rmm2_obs(i,*)-rmm2_ctl(i,*))^2))
   bivar_rmse_bycase(1,i)=SQRT(1./30.*TOTAL((rmm1_obs(i,*)-rmm1_ent(i,*))^2+(rmm2_obs(i,*)-rmm2_ent(i,*))^2))
   bivar_rmse_bycase(2,i)=SQRT(1./30.*TOTAL((rmm1_obs(i,*)-rmm1_cmt(i,*))^2+(rmm2_obs(i,*)-rmm2_cmt(i,*))^2))
   bivar_rmse_bycase(3,i)=SQRT(1./30.*TOTAL((rmm1_obs(i,*)-rmm1_emt(i,*))^2+(rmm2_obs(i,*)-rmm2_emt(i,*))^2))
   bivar_rmse_bycase(4,i)=SQRT(1./30.*TOTAL((rmm1_obs(i,*)-rmm1_obs(i,0))^2+(rmm2_obs(i,*)-rmm2_obs(i,0))^2))   
   
   bivar_rmse_bycase_p10(0,i)=SQRT(1./30.*TOTAL((rmm1_obs_p10(i,*)-rmm1_ctl_p10(i,*))^2+(rmm2_obs_p10(i,*)-rmm2_ctl_p10(i,*))^2))
   bivar_rmse_bycase_p10(1,i)=SQRT(1./30.*TOTAL((rmm1_obs_p10(i,*)-rmm1_ent_p10(i,*))^2+(rmm2_obs_p10(i,*)-rmm2_ent_p10(i,*))^2))
   bivar_rmse_bycase_p10(2,i)=SQRT(1./30.*TOTAL((rmm1_obs_p10(i,*)-rmm1_cmt_p10(i,*))^2+(rmm2_obs_p10(i,*)-rmm2_cmt_p10(i,*))^2))
   bivar_rmse_bycase_p10(3,i)=SQRT(1./30.*TOTAL((rmm1_obs_p10(i,*)-rmm1_emt_p10(i,*))^2+(rmm2_obs_p10(i,*)-rmm2_emt_p10(i,*))^2))
   bivar_rmse_bycase_p10(4,i)=SQRT(1./30.*TOTAL((rmm1_obs_p10(i,*)-rmm1_obs_p10(i,0))^2+(rmm2_obs_p10(i,*)-rmm2_obs_p10(i,0))^2))      

   print,'---------------'
   print,start_dates(i)
   print,'CTL: '+STRTRIM(STRING(bivar_rmse_bycase(0,i)),1)
   print,'ENT: '+STRTRIM(STRING(bivar_rmse_bycase(1,i)),1)
   print,'CMT: '+STRTRIM(STRING(bivar_rmse_bycase(2,i)),1)
   print,'EMT: '+STRTRIM(STRING(bivar_rmse_bycase(3,i)),1)
   print,'PER: '+STRTRIM(STRING(bivar_rmse_bycase(4,i)),1)

   print,'---------------'
   print,start_dates_plus10(i)
   print,'CTL: '+STRTRIM(STRING(bivar_rmse_bycase_p10(0,i)),1)
   print,'ENT: '+STRTRIM(STRING(bivar_rmse_bycase_p10(1,i)),1)
   print,'CMT: '+STRTRIM(STRING(bivar_rmse_bycase_p10(2,i)),1)
   print,'EMT: '+STRTRIM(STRING(bivar_rmse_bycase_p10(3,i)),1)
   print,'PER: '+STRTRIM(STRING(bivar_rmse_bycase_p10(4,i)),1)


ENDFOR

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.bivar_corr.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=1

styles=[0,0,2,2,1]
syms=[6,7,6,7,3]
FOR i=0,4 DO BEGIN
   toplot=REFORM(bivar_corr(i,*))
   IF TOTAL(where(toplot lt 0)) ge 0 THEN $
      toplot[where(toplot lt 0)]=!Values.F_NaN
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(toplot),STYLE=styles(i)
   FOR j=0,n_days-1,3 DO $
      GPLOT,X=j+0.5,Y=toplot(j),SYM=syms(i)
ENDFOR
AXES,XSTEP=2,XMINOR=1,YSTEP=0.1,YMINOR=0.05,NDECS=2,YTITLE='Bivariate correlation of RMM1 and RMM2',XTITLE='Forecast lead time (days)'
config_labels=['Control','1.5*F','NoCMT','1.5*F+NoCMT','Persistence']
;rmm_labels=['RMM1','RMM2']
GPLOT,X=[0,n_days],Y=[0.6,0.6],STYLE=2,COL=FSC_COLOR('black'),THICK=70
GLEGEND,labels=REVERSE(config_labels),STYLE=REVERSE(styles),SYM=REVERSE(syms),LEGPOS=9
;GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=2,STYLE=REVERSE([0,2])
PSCLOSE
STOP

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.bivar_rmse.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=4

styles=[0,0,2,2,1]
syms=[6,7,6,7,3]
FOR i=0,4 DO BEGIN
   toplot=REFORM(bivar_rmse(i,*))
   IF TOTAL(where(toplot gt 3.9)) ge 0 THEN $
      toplot[where(toplot gt 3.9)]=!Values.F_NaN
   GPLOT,X=indgen(n_days)+0.5,Y=REFORM(toplot),STYLE=styles(i)
   FOR j=0,n_days-1,3 DO $
      GPLOT,X=j+0.5,Y=toplot(j),SYM=syms(i)
ENDFOR
AXES,XSTEP=2,XMINOR=1,YSTEP=0.3,YMINOR=0.15,NDECS=2,YTITLE='Bivariate RMSE of RMM1 and RMM2',XTITLE='Forecast lead time (days)'
config_labels=['Control','1.5*F','NoCMT','1.5*F+NoCMT','Persistence']
;rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),STYLE=REVERSE(styles),SYM=REVERSE(syms),LEGPOS=11
;GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=2,STYLE=REVERSE([0,2])
PSCLOSE

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_rmms.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=2.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)

GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_ctl,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_ctl,COL=30,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_ent,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_ent,COL=31,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_cmt,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_cmt,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_emt,COL=33,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_emt,COL=33,STYLE=2

AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMSE in RMMs over all case studies',XTITLE='Forecast lead time (days)'
config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=2,STYLE=REVERSE([0,2])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_rmms_p10.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=2.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)

GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_ctl_p10,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_ctl_p10,COL=30,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_ent_p10,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_ent_p10,COL=31,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_cmt_p10,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_cmt_p10,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_emt_p10,COL=33,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_emt_p10,COL=33,STYLE=2

AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMSE in RMMs over all case studies',XTITLE='Forecast lead time (days)'
config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=5,STYLE=REVERSE([0,2])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_amp.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=1.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ctl,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ent,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_cmt,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_emt,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMSE in amplitude over all case studies',XTITLE='Forecast lead time (days)',/NORIGHT

GSET,XMIN=0,XMAX=n_days,YMIN=-1.5,YMAX=1.5
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ctl,COL=30,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ent,COL=31,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_cmt,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_emt,COL=33,STYLE=2

GPLOT,X=[0,n_days],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,YSTEP=0.3,YMINOR=0.15,/ONLYRIGHT,YTITLE='Mean error in amplitude over all case studies',NDEC=2

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
error_labels=['RMSE','Mean error']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(error_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=5,STYLE=REVERSE([0,2])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_amp_phase.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=-1.5,YMAX=1.7
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ctl,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ent,COL=31,STYLE=0
;GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_cmt,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_emt,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=0.3,YMINOR=0.15,NDECS=1,YTITLE='RMSE or mean error in amplitude',XTITLE='Forecast lead time (days)',/NORIGHT
;GSET,XMIN=0,XMAX=n_days,YMIN=-1.5,YMAX=1.5
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ctl,COL=30,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ent,COL=31,STYLE=2
;GPLOT,X=indgen(n_days)+0.5,Y=amp_error_cmt,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_emt,COL=33,STYLE=2
GPLOT,X=[0,n_days],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1

GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=150
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ctl,COL=30,STYLE=1
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ent,COL=31,STYLE=1
;GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_cmt,COL=32,STYLE=1
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_emt,COL=33,STYLE=1

AXES,YSTEP=30,YMINOR=15,/ONLYRIGHT,YTITLE='RMSE in phase angle'

;config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
config_labels=['Control','1.5*entrainment','1.5*entrain and no CMT']
error_labels=['RMSE in amplitude','Mean error in amplitude','RMSE in phase angle']
;GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(error_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=11,STYLE=REVERSE([0,2,3])
PSCLOSE,/NOVIEW
STOP

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_amp_p10.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=1.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ctl_p10,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_ent_p10,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_cmt_p10,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=amp_rmse_emt_p10,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMSE in amplitude over all case studies',XTITLE='Forecast lead time (days)',/NORIGHT

GSET,XMIN=0,XMAX=n_days,YMIN=-1.5,YMAX=1.5
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ctl_p10,COL=30,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_ent_p10,COL=31,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_cmt_p10,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=amp_error_emt_p10,COL=33,STYLE=2

GPLOT,X=[0,n_days],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,YSTEP=0.3,YMINOR=0.15,/ONLYRIGHT,YTITLE='Mean error in amplitude over all case studies',NDEC=2

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
error_labels=['RMSE','Mean error']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(error_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=11,STYLE=REVERSE([0,2])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rms_distance.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=2.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=distance_ctl,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_ent,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_cmt,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_emt,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMS in phase-space distance from obs over all cases',XTITLE='Forecast lead time (days)'

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
error_labels=['RMSE','Mean error']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(error_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=5,STYLE=REVERSE([0,2])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rms_distance_p10.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=2.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=distance_ctl_p10,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_ent_p10,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_cmt_p10,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=distance_emt_p10,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='RMS in phase-space distance from obs over all cases',XTITLE='Forecast lead time (days)'

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
error_labels=['RMSE','Mean error']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(error_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=5,STYLE=REVERSE([0,2])
PSCLOSE

;------------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_rmms_diffctl.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=-1.5,YMAX=0.5
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)

GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_ent-rmm1_rmse_ctl,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_ent-rmm2_rmse_ctl,COL=31,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_cmt-rmm1_rmse_ctl,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_cmt-rmm2_rmse_ctl,COL=32,STYLE=2
GPLOT,X=indgen(n_days)+0.5,Y=rmm1_rmse_emt-rmm1_rmse_ctl,COL=33,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=rmm2_rmse_emt-rmm2_rmse_ctl,COL=33,STYLE=2
GPLOT,X=[0,n_days],Y=[0,0],STYLE=1,COL=FSC_COLOR('black')

AXES,XSTEP=2,XMINOR=1,YSTEP=0.25,YMINOR=0.125,NDECS=2,YTITLE='Difference in RMSE in RMMs against control integrations',XTITLE='Forecast lead time (days)'
config_labels=['1.5*entrainment','No CMT','1.5*entrain and no CMT']
rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([31,32,33]),LEGPOS=2
GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],LEGPOS=3,STYLE=REVERSE([0,1])
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_phase.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=180
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ctl,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ent,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_cmt,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_emt,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=20,YMINOR=10,NDECS=2,YTITLE='RMSE in phase angle over all case studies',XTITLE='Forecast lead time (days)';,/NORIGHT

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.rmse_phase_p10.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2500,SPACE3=1500,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
GSET,XMIN=0,XMAX=n_days,YMIN=0,YMAX=210
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ctl_p10,COL=30,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_ent_p10,COL=31,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_cmt_p10,COL=32,STYLE=0
GPLOT,X=indgen(n_days)+0.5,Y=phase_rmse_emt_p10,COL=33,STYLE=0
AXES,XSTEP=2,XMINOR=1,YSTEP=20,YMINOR=10,NDECS=2,YTITLE='RMSE in phase angle over all case studies',XTITLE='Forecast lead time (days)';,/NORIGHT

config_labels=['Control','1.5*entrainment','No CMT','1.5*entrain and no CMT']
rmm_labels=['RMM1','RMM2']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([30,31,32,33]),LEGPOS=1
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.corr_rmms.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
ymin=-0.75
ymax=1.05
GSET,XMIN=0,XMAX=n_cases,YMIN=ymin,YMAX=ymax
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)

FOR i=0,n_cases-1 DO BEGIN
   GPLOT,X=i+0.5,Y=rmm1_corr_ent(i)+0.1-rmm1_corr_ctl(i),SYM=4,COL=31,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_ent(i)+0.1-rmm2_corr_ctl(i),SYM=5,COL=31,SIZE=80
   GPLOT,X=i+0.5,Y=rmm1_corr_cmt(i)+0.1-rmm1_corr_ctl(i),SYM=4,COL=32,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_cmt(i)+0.1-rmm2_corr_ctl(i),SYM=5,COL=32,SIZE=80
   GPLOT,X=i+0.5,Y=rmm1_corr_emt(i)+0.1-rmm1_corr_ctl(i),SYM=4,COL=33,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_emt(i)+0.1-rmm2_corr_ctl(i),SYM=5,COL=33,SIZE=80
   GPLOT,X=i+0.5,Y=-0.65-0.05*ODD(i),TEXT='('+STRMID(STRTRIM(STRING(rmm1_corr_ctl(i)-0.1),1),0,4)+','+$
         STRMID(STRTRIM(STRING(rmm2_corr_ctl(i)-0.1),1),0,4)+')',COL=30
;   GPLOT,X=i+0.75,Y=ymin-0.15,TEXT=STRMID(STRTRIM(STRING(rmm2_corr_ctl(i)-0.1),1),0,4),COL=30
ENDFOR
GPLOT,X=[0,n_cases],Y=[0,0]
AXES,XVALS=indgen(n_cases)+0.5,XLABELS=start_dates,YSTEP=0.1,YMINOR=0.05,NDECS=2,XTITLE='Case',$
     YTITLE='Difference in correlation coefficient from control integrations'
config_labels=['1.5*entrainment','No CMT','1.5*entrain and no CMT']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],SYM=REVERSE([4,5]),LENGTH=0.1,LEGPOS=5
PSCLOSE,/NOVIEW

; ---------------

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/rmm_phases/hadgem3kpp_cascade_rmmphases_errors_corrs.corr_rmms_p10.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=100
ymin=-0.75
ymax=1.05
GSET,XMIN=0,XMAX=n_cases,YMIN=ymin,YMAX=ymax
red=FSC_COLOR('red',30)
blue=FSC_COLOR('blue',31)
brown=FSC_COLOR('brown',32)
purple=FSC_COLOR('purple',33)

FOR i=0,n_cases-1 DO BEGIN
   GPLOT,X=i+0.5,Y=rmm1_corr_ent_p10(i)+0.1-rmm1_corr_ctl_p10(i),SYM=4,COL=31,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_ent_p10(i)+0.1-rmm2_corr_ctl_p10(i),SYM=5,COL=31,SIZE=80
   GPLOT,X=i+0.5,Y=rmm1_corr_cmt_p10(i)+0.1-rmm1_corr_ctl_p10(i),SYM=4,COL=32,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_cmt_p10(i)+0.1-rmm2_corr_ctl_p10(i),SYM=5,COL=32,SIZE=80
   GPLOT,X=i+0.5,Y=rmm1_corr_emt_p10(i)+0.1-rmm1_corr_ctl_p10(i),SYM=4,COL=33,SIZE=80
   GPLOT,X=i+0.5,Y=rmm2_corr_emt_p10(i)+0.1-rmm2_corr_ctl_p10(i),SYM=5,COL=33,SIZE=80
   GPLOT,X=i+0.5,Y=-0.65-0.05*ODD(i),TEXT='('+STRMID(STRTRIM(STRING(rmm1_corr_ctl_p10(i)-0.1),1),0,4)+','+$
         STRMID(STRTRIM(STRING(rmm2_corr_ctl_p10(i)-0.1),1),0,4)+')',COL=30
;   GPLOT,X=i+0.75,Y=ymin-0.15,TEXT=STRMID(STRTRIM(STRING(rmm2_corr_ctl_p10(i)-0.1),1),0,4),COL=30
ENDFOR
GPLOT,X=[0,n_cases],Y=[0,0]
AXES,XVALS=indgen(n_cases)+0.5,XLABELS=start_dates,YSTEP=0.1,YMINOR=0.05,NDECS=2,XTITLE='Case',$
     YTITLE='Difference in correlation coefficient from control integrations'
config_labels=['1.5*entrainment','No CMT','1.5*entrain and no CMT']
GLEGEND,labels=REVERSE(config_labels),COL=REVERSE([31,32,33]),LEGPOS=1
GLEGEND,labels=REVERSE(rmm_labels),COL=[FSC_COLOR('black'),FSC_COLOR('black')],SYM=REVERSE([4,5]),LENGTH=0.1,LEGPOS=5
PSCLOSE,/NOVIEW

STOP
END

