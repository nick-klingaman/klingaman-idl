PRO hadgem3kpp_cascade_phase2_composites_10days

; Make day-by-day composites of precip, U850, U200, OLR from
; the phase 2 integrations for the control and 1.5x entrainment.

um3='/home/ss901165/um_output3'

n_sets=2
control_runs=       ['xfrla',  'xfadh',  'xfdif',  'xfrle',  'xfrli',  'xfrlm',  'xfrlq']; 'xfrlu',  'xfsea',  'xfsee',  'xfsei',  'xfsem',  'xfseq',  'xfseu']
control_dirs=       ['xfrla','xfadh.old_stash','xfdif','xfrle','xfrli','xfrlm',  'xfrlq'];  'xfrlu',  'xfsea',  'xfsee',  'xfsei',  'xfsem',  'xfseq',  'xfseu']
entrain_runs=       ['xfrlb',  'xfadk',  'xfdii',  'xfrlf',  'xfrlj',  'xfrln',  'xfrlr'];  'xfrlv',  'xfseb',  'xfsef',  'xfsej',  'xfsen',  'xfser',  'xfsev']
entrain_dirs=       ['xfrlb','xfadk.old_stash','xfdii','xfrlf','xfrlj','xfrln',  'xfrlr'];,  'xfrlv',  'xfseb',  'xfsef',  'xfsej',  'xfsen',  'xfser',  'xfsev']
start_dates=        [ '04nov09','06apr09','11oct08','26aug08','07dec07','07sep06','18mar05','03dec03','29oct02','05jun02','26apr02','16jan01','13nov00','28sep00']
eraint_offset_years=[20,       20,       19,       19,       18,       17,       16,       14,       13,       13,       13,       12,       11,       11]
offset_dates=       [308,      96,       284,      238,      341,      250,      77,       337,      302,      156,      116,      16,       317,      271]-1

n_days=10
n_cases=N_ELEMENTS(control_runs)
n_plots=2

box=[-16,40,15,181]

FOR i=0,n_plots-1 DO BEGIN
   CASE i OF      
      0 : BEGIN
         first_varname='olr.2.5x2.5'
         first_ncvarname='olr'
         first_obs_infile='/home/ss901165/datasets/NOAA_CIRES_OLR/daily/NOAA_CIRES_OLR.jan-dec_dmeans.1989-2009.nc'
         first_obs_ncvarname='olr'
         first_obs_offset_years=eraint_offset_years
         first_obs_offset_dates=offset_dates
         first_var_levels=indgen(70)*1.95+170         
         first_cb_title='Outgoing longwave radiation (W m!U-2!N)'
         first_var_multiplier=1
         first_color_rev=0
         first_obs_multiplier=1./100.
         first_obs_add=327.65

         second_varname='precip'
         second_ncvarname='precip'
         second_obs_infile='/home/ss901165/datasets_mango/ERA-INTERIM/U200/U200.jan-dec_dmeans.1989-2009.mjo_domain.n96.nc'
         second_obs_ncvarname='precip'        
         second_obs_offset_years=eraint_offset_years
         second_obs_offset_dates=offset_dates
         second_var_multiplier=86400.
         second_var_levels=findgen(12)*1.5+5
      END
   ENDCASE

   FOR j=0,n_sets-1 DO BEGIN
      CASE j OF
         1 : BEGIN
            set_name='control'
            runids=control_runs
            dirs=control_dirs
         END
         0 : BEGIN
            set_name='15xentrain'
            runids=entrain_runs
            dirs=entrain_dirs
         END
      ENDCASE
      FOR k=0,n_cases-1 DO BEGIN
         first_model_infile=um3+'/'+dirs(k)+'/'+runids(k)+'a.'+start_dates(k)+'_'+first_varname+'.nc'
         
         first_longitude=OPEN_AND_EXTRACT(first_model_infile,'longitude')
         first_latitude=OPEN_AND_EXTRACT(first_model_infile,'latitude')
         DEFINE_BOUNDARIES,box,first_latitude,first_longitude,box_tx,/LIMIT
         first_nlon=N_ELEMENTS(first_longitude)
         first_nlat=N_ELEMENTS(first_latitude)
         
         first_var=REFORM(OPEN_AND_EXTRACT(first_model_infile,first_ncvarname,$
                                           offset=[box_tx(1),box_tx(0),0],count=[first_nlon,first_nlat,n_days]))*first_var_multiplier

         second_model_infile=um3+'/'+dirs(k)+'/'+runids(k)+'a.'+start_dates(k)+'_'+second_varname+'.nc'
         
         second_longitude=OPEN_AND_EXTRACT(second_model_infile,'longitude')
         second_latitude=OPEN_AND_EXTRACT(second_model_infile,'latitude')
         DEFINE_BOUNDARIES,box,second_latitude,second_longitude,box_tx,/LIMIT
         second_nlon=N_ELEMENTS(second_longitude)
         second_nlat=N_ELEMENTS(second_latitude)
         
         second_var=REFORM(OPEN_AND_EXTRACT(second_model_infile,second_ncvarname,$
                                           offset=[box_tx(1),box_tx(0),0],count=[second_nlon,second_nlat,n_days]))*second_var_multiplier
         
         IF k eq 0 THEN BEGIN
            composite_firstvar=fltarr(n_days,first_nlon,first_nlat)
            composite_secondvar=fltarr(n_days,second_nlon,second_nlat)
         ENDIF

         FOR m=0,n_days-1 DO BEGIN
            composite_firstvar(m,*,*)=first_var(*,*,m)*1./FLOAT(n_cases)+composite_firstvar(m,*,*)
            composite_secondvar(m,*,*)=second_var(*,*,m)*1./FLOAT(n_cases)+composite_secondvar(m,*,*)
         ENDFOR     
      ENDFOR
      
      dummy_v=fltarr(second_nlon,second_nlat)
      dummy_v(*,*)=0.
      FOR k=0,n_days-1 DO BEGIN
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/hadgem3kpp_cascade_phase2_composites_10days.'+$
                set_name+'.'+first_varname+'_'+second_varname+'.day'+STRTRIM(STRING(k+1),1)+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=800,XOFFSET=1000,YOFFSET=1000,TFONT=2,TCHARSIZE=100,$
                SPACE2=1000,YSIZE=12000
         IF first_color_rev eq 0 THEN BEGIN
            CS,SCALE=7,NCOLS=N_ELEMENTS(first_var_levels)+1
         ENDIF ELSE IF first_color_rev eq 1 THEN $
            CS,SCALE=7,NCOLS=N_ELEMENTS(first_var_levels)+1,/REV
         LEVS,MANUAL=first_var_levels
         MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
         CON,X=first_longitude,Y=first_latitude,FIELD=REFORM(composite_firstvar(k,*,*)),$
             /NOLINES,CB_TITLE=first_cb_title,/NOAXES,/NOCOLBAR
         CS,SCALE=3,NCOLS=N_ELEMENTS(second_var_levels)+1
         LEVS,MANUAL=second_var_levels
         FOR m=1,second_nlon-2 DO $
            FOR n=1,second_nlat-2 DO $
               IF composite_secondvar(k,m,n) gt second_var_levels(0) THEN $
                  GPLOT,X=second_longitude(m),Y=second_latitude(n),SYM=1,COL=NEAREST(second_var_levels,composite_secondvar(k,m,n))+2,$
                        SIZE=51   
         AXES,XSTEP=20,YSTEP=5
         PSCLOSE
      ENDFOR   
   ENDFOR
   ; FOR k=0,n_cases-1 DO BEGIN   
   ;    first_longitude=OPEN_AND_EXTRACT(first_obs_infile,'longitude')
   ;    first_latitude=OPEN_AND_EXTRACT(first_obs_infile,'latitude')
   ;    DEFINE_BOUNDARIES,box,first_latitude,first_longitude,box_tx,/LIMIT
   ;    first_nlon=N_ELEMENTS(first_longitude)
   ;    first_nlat=N_ELEMENTS(first_latitude)
      
   ;    first_var=REFORM(OPEN_AND_EXTRACT(first_obs_infile,first_obs_ncvarname,$
   ;                                      offset=[box_tx(1),box_tx(0),first_obs_offset_dates(k),first_obs_offset_years(k)],count=[first_nlon,first_nlat,n_days,1]))*first_obs_multiplier+first_obs_add
            
   ;    second_longitude=OPEN_AND_EXTRACT(second_obs_infile,'longitude')
   ;    second_latitude=OPEN_AND_EXTRACT(second_obs_infile,'latitude')
   ;    DEFINE_BOUNDARIES,box,second_latitude,second_longitude,box_tx,/LIMIT
   ;    second_nlon=N_ELEMENTS(second_longitude)
   ;    second_nlat=N_ELEMENTS(second_latitude)
      
   ;    second_var=REFORM(OPEN_AND_EXTRACT(second_obs_infile,second_obs_ncvarname,$
   ;                                       offset=[box_tx(1),box_tx(0),second_obs_offset_years(k),second_obs_offset_dates(k)],count=[second_nlon,second_nlat,1,n_days]))
      
   ;    IF k eq 0 THEN BEGIN
   ;       composite_firstvar=fltarr(n_days,first_nlon,first_nlat)
   ;       composite_secondvar=fltarr(n_days,second_nlon,second_nlat)
   ;    ENDIF
      
   ;    FOR m=0,n_days-1 DO BEGIN
   ;       composite_firstvar(m,*,*)=first_var(*,*,m)*1./FLOAT(n_cases)+composite_firstvar(m,*,*)
   ;       composite_secondvar(m,*,*)=second_var(*,*,m)*1./FLOAT(n_cases)+composite_secondvar(m,*,*)
   ;    ENDFOR     
   ; ENDFOR
   
   ; dummy_v=fltarr(second_nlon,second_nlat)
   ; dummy_v(*,*)=0.
   ; FOR k=0,n_days-1 DO BEGIN
   ;    psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/composites/hadgem3kpp_cascade_phase2_composites_10days.'+$
   ;           'obs.'+first_varname+'_'+second_varname+'.day'+STRTRIM(STRING(k+1),1)+'.ps'
   ;    PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=800,XOFFSET=1000,YOFFSET=1000,TFONT=2,TCHARSIZE=100,$
   ;           SPACE2=1000,YSIZE=10000
   ;    IF first_color_rev eq 0 THEN BEGIN
   ;       CS,SCALE=1,NCOLS=N_ELEMENTS(first_var_levels)+1
   ;    ENDIF ELSE IF first_color_rev eq 1 THEN $
   ;       CS,SCALE=1,NCOLS=N_ELEMENTS(first_var_levels)+1,/REV
   ;    LEVS,MANUAL=first_var_levels
   ;    MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
   ;    CON,X=first_longitude,Y=first_latitude,FIELD=REFORM(composite_firstvar(k,*,*)),$
   ;        /NOLINES,/BLOCK,CB_TITLE=first_cb_title,/NOAXES
   ;    ;VECT,U=REFORM(composite_secondvar(k,*,*)),V=dummy_v,X=second_longitude,Y=second_latitude,$
   ;    ;     MAG=second_mag,MUNITS='m s!U-1!N',STRIDE=3
   ;    AXES,XSTEP=20,YSTEP=5
   ;    PSCLOSE
   ; ENDFOR
ENDFOR
   
STOP
END
