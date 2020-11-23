PRO spcam_mean_state_comparison,expt_file,expt_case,ctrl_file,ctrl_case,varnames

n_vars=N_ELEMENTS(varnames)
datasets='/group_workspaces/jasmin2/klingaman/datasets'

seasons=[[11,0,1],$
         [2,3,4],$
         [5,6,7],$
         [8,9,10]]
season_names=['DJF','MAM','JJA','SON','ANN']
n_seasons=N_ELEMENTS(season_names)

FOR i=0,n_vars-1 DO BEGIN
   CASE varnames(i) OF
      'precip' : BEGIN
         cam_varname='PRECT'
         obs_file=datasets+'/GPCP/monthly/GPCP_vn2p2.jan-dec_mmean_clim.1981-2010.t42.nc'
         obs_varname='precip'
         obs_name='GPCP vn2.2 (1981-2010)'
         raw_levs=['1','2','3','4','6','8','10','12','15','18','22','26']
         raw_scale=24
         raw_rev=0
         raw_white=2
         obs_diff_levs=['-11','-9','-7','-5','-3','-1','1','3','5','7','9','11']         
         mod_diff_levs=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']
         diff_scale=1
         diff_rev=1
         diff_white=8
         cam_multiplier=86400.*1000.
         cam_offset=0
         obs_multiplier=1.
         obs_offset=0
         obs_revlat=1
         mask=0
         obs_miss=0
      END
      'sst' : BEGIN
         cam_varname='TS'
         obs_file=datasets+'/METO_OCEAN_ANALYSIS/t42/spcam-kpp_sst.jan-dec_mmean_clim.1980-2009.64x128.nc'
         obs_varname='temperature'
         obs_name='Met Office SST analysis (1980-2009)'
         raw_levs=['2','4','6','8','10','12','14','16','18','20','22','24','26','28','30']
         raw_scale=1
         raw_rev=0
         raw_white=2
         obs_diff_levs=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
         mod_diff_levs=['-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1']
         diff_scale=1
         diff_rev=0
         diff_white=8
         cam_multiplier=1
         cam_offset=-273.15
         obs_multiplier=1
         obs_offset=0
         obs_revlat=0
         mask=1
         mask_file=datasets+'/SPCAM-KPP_ANCIL/spcam-kpp_lsm_ocndepth.64x128.nc'
         mask_var='LANDFRAC'
         mask_cond='total_land'
         obs_miss=1
         obs_miss_value=2
         obs_miss_cond=0 ; 0: less than , 1: equal to, 2: greater than
      END
   ENDCASE

   lon=OPEN_AND_EXTRACT(expt_file,'lon')
   lat=OPEN_AND_EXTRACT(expt_file,'lat')
   n_lon=N_ELEMENTS(lon)
   n_lat=N_ELEMENTS(lat)
   
   obs_var=OPEN_AND_EXTRACT(obs_file,obs_varname)*obs_multiplier+obs_offset
   ctrl_var=OPEN_AND_EXTRACT(ctrl_file,cam_varname)*cam_multiplier+cam_offset
   expt_var=OPEN_AND_EXTRACT(expt_file,cam_varname)*cam_multiplier+cam_offset

   IF mask eq 1 THEN $
      lsm=REFORM(OPEN_AND_EXTRACT(mask_file,mask_var))

   FOR j=0,n_seasons-1 DO BEGIN      
     obs_season=fltarr(n_lon,n_lat)
     expt_season=fltarr(n_lon,n_lat)
     ctrl_season=fltarr(n_lon,n_lat)
     
     IF j lt 4 THEN BEGIN
        n_months=N_ELEMENTS(seasons(*,j))
        FOR m=0,n_months-1 DO BEGIN
           obs_season=obs_season+REFORM(obs_var(*,*,seasons(m,j)))/FLOAT(n_months)
           ctrl_season=ctrl_season+REFORM(ctrl_var(*,*,seasons(m,j)))/FLOAT(n_months)
           expt_season=expt_season+REFORM(expt_var(*,*,seasons(m,j)))/FLOAT(n_months)
        ENDFOR
     ENDIF ELSE BEGIN
        FOR k=0,n_lon-1 DO BEGIN
           FOR n=0,n_lat-1 DO BEGIN
              obs_season(k,n)=MEAN(obs_var(k,n,*))
              ctrl_season(k,n)=MEAN(ctrl_var(k,n,*))
              expt_season(k,n)=MEAN(expt_var(k,n,*))
           ENDFOR
        ENDFOR  
     ENDELSE

     IF obs_revlat eq 1 THEN BEGIN
        obs_temp=obs_season
        FOR n=0,n_lat-1 DO $
           obs_season(*,n)=obs_temp(*,n_lat-n-1)
     ENDIF

     IF mask eq 1 THEN BEGIN
        CASE mask_cond OF
           'total_land' : BEGIN ; Mask points that are totally land (=1.0)
              obs_season[where(lsm ge 0.999)]=!Values.F_NaN
              expt_season[where(lsm ge 0.999)]=!Values.F_NaN
              ctrl_season[where(lsm ge 0.999)]=!Values.F_NaN
           END
           'total_ocean' : BEGIN ; Mask points that are totally ocean (=0.0)
              obs_season[where(lsm le 0.001)]=!Values.F_NaN
              expt_season[where(lsm le 0.001)]=!Values.F_NaN
              ctrl_season[where(lsm le 0.001)]=!Values.F_NaN
           END
           'any_land' : BEGIN ; Mask points that have any land (> 0.0)
              obs_season[where(lsm ge 0.001)]=!Values.F_NaN
              expt_season[where(lsm ge 0.001)]=!Values.F_NaN
              ctrl_season[where(lsm ge 0.001)]=!Values.F_NaN
           END
           'any_ocean' : BEGIN ; Mask points that have any ocean (< 1.0)
              obs_season[where(lsm le 0.999)]=!Values.F_NaN
              expt_season[where(lsm le 0.999)]=!Values.F_NaN
              ctrl_season[where(lsm le 0.999)]=!Values.F_NaN
           END
        ENDCASE
     ENDIF

     IF obs_miss eq 1 THEN BEGIN
        CASE obs_miss_cond OF
           0 : BEGIN
              expt_season[where(obs_season le obs_miss_value)]=!Values.F_NaN              
              ctrl_season[where(obs_season le obs_miss_value)]=!Values.F_NaN
              obs_season[where(obs_season le obs_miss_value)]=!Values.F_NaN
           END
           1 : BEGIN
              expt_season[where(obs_season eq obs_miss_value)]=!Values.F_NaN              
              ctrl_season[where(obs_season eq obs_miss_value)]=!Values.F_NaN
              obs_season[where(obs_season eq obs_miss_value)]=!Values.F_NaN
           END
           2 : BEGIN
              expt_season[where(obs_season ge obs_miss_value)]=!Values.F_NaN              
              ctrl_season[where(obs_season ge obs_miss_value)]=!Values.F_NaN
              obs_season[where(obs_season ge obs_miss_value)]=!Values.F_NaN
           END
        ENDCASE
     ENDIF

     psfile='/home/users/npklingaman/plots/spcam/mean_state/spcam_mean_state_comparison.'+varnames(i)+'.'+$
            expt_case+'-minus-'+ctrl_case+'.'+season_names(j)+'.ps'
     PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=2,YPLOTS=2,XPLOTS=2,MARGIN=1500,XOFFSET=500,SPACE3=200,$
            YSPACING=1800
     IF raw_white ne 0 THEN BEGIN
        IF raw_rev eq 0 THEN BEGIN
           CS,SCALE=raw_scale,NCOL=N_ELEMENTS(raw_levs)+1,white=[raw_white]
        ENDIF ELSE $
           CS,SCALE=raw_scale,NCOL=N_ELEMENTS(raw_levs)+1,white=[raw_white],/REV
     ENDIF ELSE BEGIN
        IF raw_rev eq 0 THEN BEGIN
           CS,SCALE=raw_scale,NCOL=N_ELEMENTS(raw_levs)+1
        ENDIF ELSE $
           CS,SCALe=raw_scale,NCOL=N_ELEMENTS(raw_levs)+1,/REV
     ENDELSE
     LEVS,MANUAL=raw_levs

     ; Experiment raw data
     POS,XPOS=1,YPOS=2
     MAP
     CON,X=lon,Y=lat,FIELD=expt_season,/NOLINES,/BLOCK,TITLE=season_names(j)+' '+varnames(i)+' for '+expt_case,/NOAXES,CB_WIDTH=110
     AXES,XSTEP=60,YSTEP=30    

     ; Experiment minus control
     IF diff_white ne 0 THEN BEGIN
        IF diff_rev eq 0 THEN BEGIN
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(mod_diff_levs)+1,white=[diff_white]
        ENDIF ELSE $
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(mod_diff_levs)+1,white=[diff_white],/REV
     ENDIF ELSE BEGIN
        IF diff_rev eq 0 THEN BEGIN
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(mod_diff_levs)+1
        ENDIF ELSE $
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(mod_diff_levs)+1,/REV
     ENDELSE
     LEVS,MANUAL=mod_diff_levs
     POS,XPOS=2,YPOS=2
     MAP
     CON,X=lon,Y=lat,FIELD=expt_season-ctrl_season,/NOLINES,/BLOCK,TITLE=expt_case+' minus '+ctrl_case,/NOAXES,CB_WIDTH=110
     AXES,XSTEP=60,YSTEP=30

     ; Experiment minus observations
     IF diff_white ne 0 THEN BEGIN
        IF diff_rev eq 0 THEN BEGIN
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(obs_diff_levs)+1,white=[diff_white]
        ENDIF ELSE $
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(obs_diff_levs)+1,white=[diff_white],/REV
     ENDIF ELSE BEGIN
        IF diff_rev eq 0 THEN BEGIN
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(obs_diff_levs)+1
        ENDIF ELSE $
           CS,SCALE=diff_scale,NCOL=N_ELEMENTS(obs_diff_levs)+1,/REV
     ENDELSE
     LEVS,MANUAL=obs_diff_levs
     POS,XPOS=1,YPOS=1
     MAP
     CON,X=lon,Y=lat,FIELD=expt_season-obs_season,/NOLINES,/BLOCK,TITLE=expt_case+' minus '+obs_name,/NOAXES,CB_WIDTH=110
     AXES,XSTEP=60,YSTEP=30

     ; Control minus observations
     POS,XPOS=2,YPOS=1
     MAP
     CON,X=lon,Y=lat,FIELD=ctrl_season-obs_season,/NOLINES,/BLOCK,TITLE=ctrl_case+' minus '+obs_name,/NOAXES,CB_WIDTH=110
     AXES,XSTEP=60,YSTEP=30

     PSCLOSE,/NOVIEW
  ENDFOR

ENDFOR

STOP
END


