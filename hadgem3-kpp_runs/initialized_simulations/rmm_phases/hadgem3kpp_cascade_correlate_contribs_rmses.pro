PRO hadgem3kpp_cascade_correlate_contribs_rmses
  
  ctl_rmse=[2.304,3.412,1.834,2.216,3.322,1.922,2.435,3.179,1.547,2.950,2.232,3.350,1.308,2.446,$
            1.464,1.326,2.669,2.760,2.709,3.685,2.378,1.485,2.030,3.074,2.349,1.871]
  ent_rmse=[1.299,2.143,1.755,1.761,2.422,1.398,0.844,1.291,1.866,1.820,1.496,2.300,1.204,1.387,$
            1.378,2.477,2.128,1.202,2.087,2.441,1.710,1.382,2.333,2.575,2.193,1.483]
  cmt_rmse=[1.504,3.324,2.124,2.788,3.819,1.964,1.765,2.432,1.986,1.871,1.786,3.490,1.976,2.786,$
            2.064,1.440,3.658,2.739,2.740,3.101,2.544,1.698,1.101,4.049,2.203,1.688]
  emt_rmse=[1.249,3.110,1.657,2.048,2.547,2.538,1.126,2.509,1.951,1.744,2.353,2.177,1.400,1.978,$
            0.848,1.907,2.762,1.673,2.112,2.936,1.923,2.137,3.158,2.952,2.613,1.633]
  ctl_runs=['xfrla','xfadh','xfdif','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu',$
            'xftfa','xftfe','xftfi','xftfm','xftfq','xftfu','xftga','xftge','xftgi','xftgm','xftgq','xftgu']
offset_years=[34,       34,       33,       33,       32,       31,       30,       28,       27,       27,       27,       26,       25,       25,$
              34,       33,       32,       31,       30,       28,       27,       27,       27,       26,       25,       25]-14
offset_dates=[308,      96,       284,      238,      341,      250,      77,       337,      302,      156,      116,      16,       317,      271,$
              318,     248,      351,      260,      87,       347,      312,      166,      126,      26,       327,      281]-1
obs_input_file='/home/ss901165/datasets/MJO_INDICES/noaa_eraint_obs.jan-dec_dmeans.1989-2009.rmm_indices.nc'

n_runs=N_ELEMENTS(ctl_runs)
n_days=5

ctl_rmm1_olr=fltarr(n_runs)
ctl_rmm1_u850=fltarr(n_runs)
ctl_rmm1_u200=fltarr(n_runs)
ctl_rmm2_olr=fltarr(n_runs)
ctl_rmm2_u850=fltarr(n_runs)
ctl_rmm2_u200=fltarr(n_runs)
obs_rmm1_olr=fltarr(n_runs)
obs_rmm1_u850=fltarr(n_runs)
obs_rmm1_u200=fltarr(n_runs)
obs_rmm2_olr=fltarr(n_runs)
obs_rmm2_u850=fltarr(n_runs)
obs_rmm2_u200=fltarr(n_runs)
FOR i=0,n_runs-1 DO BEGIN
   ctl_infile='/home/ss901165/um_output3/'+ctl_runs(i)+'/rmm_indices.nc'
   ctl_rmm1_olr(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm1_olr'))
   ctl_rmm1_u850(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm1_u850'))
   ctl_rmm1_u200(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm1_u200'))
   ctl_rmm2_olr(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm2_olr'))
   ctl_rmm2_u850(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm2_u850'))
   ctl_rmm2_u200(i)=MEAN(OPEN_AND_EXTRACT(ctl_infile,'contrib_rmm2_u200'))
   IF offset_dates(i)+n_days gt 365 THEN BEGIN
      count_yearone=365-offset_dates(i)
      count_yeartwo=n_days-count_yearone
      temp=fltarr(n_days)
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_olr',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_olr',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm1_olr(i)=ABS(MEAN(temp))
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u850',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u850',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm1_u850(i)=ABS(MEAN(temp))
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u200',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u200',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm1_u200(i)=ABS(MEAN(temp))
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_olr',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_olr',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm2_olr(i)=ABS(MEAN(temp))
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u850',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u850',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm2_u850(i)=ABS(MEAN(temp))
      temp(0:count_yearone-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u200',$
                                                          offset=[offset_years(i),offset_dates(i)],count=[1,count_yearone]))
      temp(count_yearone:n_days-1)=REFORM(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u200',$
                                                             offset=[offset_years(i)+1,0],count=[1,count_yeartwo]))      
      obs_rmm2_u200(i)=ABS(MEAN(temp))
   ENDIF ELSE BEGIN
      obs_rmm1_olr(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_olr',$
                                            offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
      obs_rmm1_u850(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u850',$
                                             offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
      obs_rmm1_u200(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm1_u200',$
                                             offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
      obs_rmm2_olr(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_olr',$
                                            offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
      obs_rmm2_u850(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u850',$
                                             offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
      obs_rmm2_u200(i)=ABS(MEAN(OPEN_AND_EXTRACT(obs_input_file,'contrib_rmm2_u200',$
                                             offset=[offset_years(i),offset_dates(i)],count=[1,n_days])))
   ENDELSE
ENDFOR

FOR i=0,3 DO BEGIN
   CASE i OF 
      0 : BEGIN
         rmse=ctl_rmse
      END
      1 : BEGIN
         rmse=ent_rmse
      END
      2 : BEGIN
         rmse=cmt_rmse
      END
      3 : BEGIN
         rmse=emt_rmse
      END
   ENDCASE
   corrs_sigs=fltarr(6,2)
   corrs_sigs(0,*)=R_CORRELATE(rmse,obs_rmm1_olr)
   corrs_sigs(1,*)=R_CORRELATE(rmse,obs_rmm1_u850)
   corrs_sigs(2,*)=R_CORRELATE(rmse,obs_rmm1_u200)
   corrs_sigs(3,*)=R_CORRELATE(rmse,obs_rmm2_olr)
   corrs_sigs(4,*)=R_CORRELATE(rmse,obs_rmm2_u850)
   corrs_sigs(5,*)=R_CORRELATE(rmse,obs_rmm2_u200)

   IF i gt 0 THEN BEGIN
      rmse_ratio=rmse/ctl_rmse
      corrs_sigs_ratio=fltarr(6,2)
      corrs_sigs_ratio(0,*)=R_CORRELATE(rmse_ratio,obs_rmm1_olr)
      corrs_sigs_ratio(1,*)=R_CORRELATE(rmse_ratio,obs_rmm1_u850)
      corrs_sigs_ratio(2,*)=R_CORRELATE(rmse_ratio,obs_rmm1_u200)
      corrs_sigs_ratio(3,*)=R_CORRELATE(rmse_ratio,obs_rmm2_olr)
      corrs_sigs_ratio(4,*)=R_CORRELATE(rmse_ratio,obs_rmm2_u850)
      corrs_sigs_ratio(5,*)=R_CORRELATE(rmse_ratio,obs_rmm2_u200)
   ENDIF
   print,'************************************************'
   print,corrs_sigs(*,0)
   print,corrs_sigs(*,1)
   print,'************************************************'
   IF i gt 0 THEN BEGIN
      print,corrs_sigs_ratio(*,0)
      print,corrs_sigs_ratio(*,1)
      print,'************************************************'
   ENDIF
ENDFOR

STOP
END
