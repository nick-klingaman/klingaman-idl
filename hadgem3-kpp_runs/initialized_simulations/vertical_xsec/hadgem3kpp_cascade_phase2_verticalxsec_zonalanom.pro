PRO hadgem3kpp_cascade_phase2_verticalxsec_zonalanom

; Plot cross-sections of anomalies from the zonal mean,
; making composites of all phase 2 initialized simulations,
; using data from the first few days

n_sets=4
n_variables=2
um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']
box=[-5,40,5,200]
n_z=85

meaning_period=8
meaning_units='hours'
n_times=24
time_offset=0
n_frames=n_times/meaning_period

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
         set_name='control'
      END
      1 : BEGIN
         runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
         set_name='15xentrain'
      END
      2 : BEGIN
         runids=['xfrlc','xfrlg','xfrlk','xfrlo','xfrls','xfrlw','xfsec','xfseg','xfsek','xfseo','xfses','xfsew']
         set_name='nocmt'
      END
      3 : BEGIN
         runids=['xfrld','xfrlh','xfrll','xfrlp','xfrlt','xfrlx','xfsed','xfseh','xfsel','xfsep','xfset','xfsex']
         set_name='15xentrain_nocmt'
      END
   ENDCASE
   n_cases=3
   ; n_cases=N_ELEMENTS(runids)
   FOR j=0,n_variables-1 DO BEGIN
      CASE j OF
         0 : BEGIN
            first_varname=['T_after_advect']
            first_ncvarname=['field981']
            first_longitude_varname=['longitude_1']
            first_latitude_varname=['latitude_2']
            first_varmult=1.
            first_vardesc='temperature'
            first_vartitle='temperature'
            first_plot_anomaly=1
            first_contour_levels=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                                  '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
            first_cb_title='Temperature anomaly (K) from three-hour data'

            ;second_varname=['Tinc_bdylrlscld','Tinc_conv','Tinc_lsrain']
            ;second_ncvarname=['unspecified_13','unspecified_12','unspecified']
            ;second_longitude_varname=['longitude_1','longitude','longitude']
            ;second_latitude_varname=['latitude_2','latitude','latitude']
            ;second_varmult=[3.,3.,3.]
            ;second_vardesc='diabatic heating from physics'
            ;second_vartitle='diabatic_physics'
            ;second_plot_anomaly=0           
            ;second_contour_levels=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
            ;                       '0.05','0.15','0.25','0.35','0.45','0.55','0.65']
            ;second_contour_spacing='0.10 K hr!U-1!N'

            second_varname=['Tinc_swrad_pc2','Tinc_lwrad_pc2']
            second_ncvarname=['unspecified','unspecified']
            second_longitude_varname=['longitude','longitude']
            second_latitude_varname=['latitude','latitude']
            second_varmult=[3.,3.]
            second_vardesc='diabatic heating from radiation'
            second_vartitle='diabatic_radiation'
            second_plot_anomaly=0
            second_contour_levels=['-0.17','-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
                                   '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17']
            second_contour_spacing='0.01 K hr!U-1!N'           
         END
         1 : BEGIN
            first_varname=['w_after_ts']
            first_ncvarname=['dz_dt']
            first_longitude_varname=['longitude']
            first_latitude_varname=['latitude']
            first_varmult=100.
            first_vardesc='vertical velocity'
            first_vartitle='vertical_velocity'
            first_plot_anomaly=1
            first_contour_levels=['-4.25','-3.75','-3.25','-2.75','-2.25','-1.75','-1.25','-0.75','-0.25',$
                                  '0.25','0.75','1.25','1.75','2.25','2.75','3.25','3.75','4.25']
            first_cb_title='Vertical velocity anomaly (cm s!U-1!N) from three-hour data'

;            second_varname=['Tinc_bdylrlscld','Tinc_conv','Tinc_lsrain']
;            second_ncvarname=['unspecified_13','unspecified_12','unspecified']
;            second_longitude_varname=['longitude_1','longitude','longitude']
;            second_latitude_varname=['latitude_2','latitude','latitude']
;            second_varmult=[3.,3.,3.]
;            second_vardesc='diabatic heating from physics'
;            second_vartitle='diabatic_physics'
;            second_plot_anomaly=0    
;            second_contour_levels=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
;                                   '0.05','0.15','0.25','0.35','0.45','0.55','0.65']           
;            second_contour_spacing='0.10 K hr!U-1!N'
            second_varname=['Tinc_swrad_pc2','Tinc_lwrad_pc2']
            second_ncvarname=['unspecified','unspecified']
            second_longitude_varname=['longitude','longitude']
            second_latitude_varname=['latitude','latitude']
            second_varmult=[3.,3.]
            second_vardesc='diabatic heating from radiation'
            second_vartitle='diabatic_radiation'
            second_plot_anomaly=0
            second_contour_levels=['-0.17','-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
                                   '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17']
            second_contour_spacing='0.01 K hr!U-1!N'
         END
         2 : BEGIN
            first_varname=['t_after_ts']
            first_ncvarname=['temp_1']
            first_longitude_varname=['longitude']
            first_latitude_varname=['latitude']
            first_varmult=1.
            first_vardesc='temperature'
            first_vartitle='temperature'
            first_plot_anomaly=1
            first_contour_levels=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1',$
                                  '0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
            first_cb_title='Temperature anomaly (K) from three-hour data'

            second_varname=['w_after_ts']
            second_ncvarname=['dz_dt']
            second_longitude_varname=['longitude']
            second_latitude_varname=['latitude']
            second_varmult=[100.]
            second_vardesc='vertical velocity'
            second_vartitle='vertical_velocity'
            second_plot_anomaly=0           
            second_contour_levels=['-6.0','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4',$
                                   '0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0']
            second_contour_spacing='0.80 cm s!U-1!N'
         END
      ENDCASE

      thlvl_pres_file=um3+'/'+runids(0)+'.new_stash/'+runids(0)+'.'+date_ranges(0)+'_3hrmeans.p_thlvl_after_ts.nc'
      thlvl_longitude=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude')
      thlvl_latitude=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
      DEFINE_BOUNDARIES,box,thlvl_latitude,thlvl_longitude,thlvl_box_tx,/LIMIT
      thlvl_nlon=N_ELEMENTS(thlvl_longitude)
      thlvl_nlat=N_ELEMENTS(thlvl_latitude)
      thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_1',$
                                          offset=[thlvl_box_tx(1),thlvl_box_tx(0),0,0],$
                                          count=[thlvl_nlon,thlvl_nlat,n_z,n_times]))
      thlvl_pres_mean=fltarr(n_z)
      FOR k=0,n_z-1 DO $
         thlvl_pres_mean(k)=MEAN(thlvl_pres(*,*,k,*))/100.

      FOR k=0,n_cases-1 DO BEGIN
         FOR m=0,N_ELEMENTS(first_varname)-1 DO BEGIN
            first_infile=um3+'/'+runids(k)+'.new_stash/'+runids(k)+'.'+date_ranges(k)+'_3hrmeans.'+first_varname(m)+'.nc'
            print,first_infile
            first_longitude=OPEN_AND_EXTRACT(first_infile,first_longitude_varname(m))            
            first_latitude=OPEN_AND_EXTRACT(first_infile,first_latitude_varname(m))
            DEFINE_BOUNDARIES,box,first_latitude,first_longitude,first_box_tx,/LIMIT
            first_nlon=N_ELEMENTS(first_longitude)
            first_nlat=N_ELEMENTS(first_latitude)
            IF m eq 0 THEN $
               first_var_sum=fltarr(first_nlon,first_nlat,n_z,n_frames)
            IF k eq 0 and m eq 0 THEN BEGIN
               first_var_latavg=fltarr(first_nlon,n_z,n_frames)
               first_var_zonalmean=fltarr(n_z,n_frames)
            ENDIF
            
            first_var=REFORM(OPEN_AND_EXTRACT(first_infile,first_ncvarname(m),offset=[first_box_tx(1),first_box_tx(0),0,time_offset],$
                                              count=[first_nlon,first_nlat,n_z,n_times]))*first_varmult(m)
            print,'Read first var'
            
            FOR n=0,first_nlon-1 DO $
               FOR p=0,first_nlat-1 DO $
                  FOR r=0,n_z-1 DO $
                     FOR s=0,n_frames-1 DO $
                        first_var_sum(n,p,r,s)=first_var_sum(n,p,r,s)+MEAN(first_var(n,p,r,s*meaning_period:(s+1)*meaning_period-1))
         ENDFOR
         FOR m=0,first_nlon-1 DO $
            FOR n=0,n_z-1 DO $
               FOR p=0,n_frames-1 DO $
                  first_var_latavg(m,n,p)=MEAN(first_var_sum(m,*,n,p))*1./FLOAT(n_cases)+first_var_latavg(m,n,p)
         
         FOR m=0,N_ELEMENTS(second_varname)-1 DO BEGIN
            second_infile=um3+'/'+runids(k)+'.new_stash/'+runids(k)+'.'+date_ranges(k)+'_3hrmeans.'+second_varname(m)+'.nc'
            print,second_infile
            second_longitude=OPEN_AND_EXTRACT(second_infile,second_longitude_varname(m))
            second_latitude=OPEN_AND_EXTRACT(second_infile,second_latitude_varname(m))
            DEFINE_BOUNDARIES,box,second_latitude,second_longitude,second_box_tx,/LIMIT
            second_nlon=N_ELEMENTS(second_longitude)
            second_nlat=N_ELEMENTS(second_latitude)
            IF m eq 0 THEN $
               second_var_sum=fltarr(second_nlon,second_nlat,n_z,n_frames)
            IF k eq 0 and m eq 0 THEN BEGIN
               second_var_latavg=fltarr(second_nlon,n_z,n_frames)
               second_var_zonalmean=fltarr(n_z,n_frames)
            ENDIF
            
            second_var=REFORM(OPEN_AND_EXTRACT(second_infile,second_ncvarname(m),offset=[second_box_tx(1),second_box_tx(0),0,time_offset],$
                                              count=[second_nlon,second_nlat,n_z,n_times]))*second_varmult(m)
            print,'Read second var'
            
            FOR n=0,second_nlon-1 DO $
               FOR p=0,second_nlat-1 DO $
                  FOR r=0,n_z-1 DO $
                     FOR s=0,n_frames-1 DO $
                        second_var_sum(n,p,r,s)=second_var_sum(n,p,r,s)+MEAN(second_var(n,p,r,s*meaning_period:(s+1)*meaning_period-1))
         ENDFOR
         FOR m=0,second_nlon-1 DO $
            FOR n=0,n_z-1 DO $
               FOR p=0,n_frames-1 DO $
                  second_var_latavg(m,n,p)=MEAN(second_var_sum(m,*,n,p))*1./FLOAT(n_cases)+second_var_latavg(m,n,p)
         FOR m=0,n_z-1 DO $
            FOR n=0,n_frames-1 DO $
                                ; It is not necessary to scale the zonal mean, because it the latitude-average variable has already been scaled 
                                ; above
               second_var_zonalmean(m,n)=MEAN(second_var_latavg(*,m,n))+second_var_zonalmean(m,n)    
      ENDFOR

      FOR m=0,n_z-1 DO BEGIN
         FOR n=0,n_frames-1 DO BEGIN
            first_var_zonalmean(m,n)=MEAN(first_var_latavg(*,m,n))
            second_var_zonalmean(m,n)=MEAN(second_var_latavg(*,m,n))
         ENDFOR
      ENDFOR

      FOR n=0,n_frames-1 DO BEGIN
         first_var_plot=fltarr(first_nlon,n_z)
         IF first_plot_anomaly eq 1 THEN BEGIN
            FOR p=0,first_nlon-1 DO $
               FOR r=0,n_z-1 DO $
                  first_var_plot(p,r)=first_var_latavg(p,r,n)-first_var_zonalmean(r,n)
         ENDIF ELSE $
            first_var_plot(*,*)=REFORM(first_var_latavg(*,*,n))
         
         second_var_plot=fltarr(second_nlon,n_z)
         IF second_plot_anomaly eq 1 THEN BEGIN
            FOR p=0,second_nlon-1 DO $
               FOR r=0,n_z-1 DO $
                  second_var_plot(p,r)=second_var_latavg(p,r,n)-second_var_zonalmean(r,n)
         ENDIF ELSE $
            second_var_plot(*,*)=REFORM(second_var_latavg(*,*,n))
         
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/vertical_xsec/hadgem3kpp_cascade_phase2_verticalxsec_zonalanom.'+$
                first_vartitle+'_with_'+second_vartitle+'.'+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+$
                STRTRIM(STRING((n+1)*meaning_period*3-1),1)+'.'+set_name+'.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=2200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
                SPACE2=700
         CS,SCALE=1,NCOLS=N_ELEMENTS(first_contour_levels)+1
         LEVS,MANUAL=first_contour_levels
         GSET,XMIN=box(1),XMAX=box(3),YMIN=1020.,YMAX=80.
         plot_title='Composite of latavg 5S-5N '+first_vartitle+' '
         IF first_plot_anomaly eq 1 THEN $
            plot_title=plot_title+'anomaly from zonal mean '
         plot_title=plot_title+'with '+second_vartitle+' '
         IF second_plot_anomaly eq 1 THEN $
            plot_title=plot_title+'anomaly from zonal mean '
         plot_title=plot_title+'for '+set_name+' - '+meaning_units+STRTRIM(STRING(n*meaning_period*3),1)+'-'+$
            STRTRIM(STRING((n+1)*meaning_period*3-1),1)
         CON,X=first_longitude,Y=thlvl_pres_mean,FIELD=REFORM(first_var_plot),/NOLINES,$
             /BLOCK,title=plot_title,cb_title=first_cb_title
         LEVS,MANUAL=second_contour_levels
         CON,X=second_longitude,Y=thlvl_pres_mean,FIELD=REFORM(second_var_plot),/NOFILL,/NOLINELABELS,$
             POSITIVE_STYLE=0,NEGATIVE_STYLE=2,THICK=[REVERSE(indgen(N_ELEMENTS(second_contour_levels)/2+1)*20+50),$
                                                      indgen(N_ELEMENTS(second_contour_levels)/2+1)*20+50]
         AXES,XSTEP=20,XMINOR=5,NDECS=1,XTITLE='Longitude (degrees east)',YTITLE='Pressure (hPa)',$
              YVALS=indgen(19)*50+100
         GPLOT,X=(MAX(second_longitude)-MIN(second_longitude))*0.85+MIN(second_longitude),Y=1100,TEXT=second_vartitle+' contours are spaced '+$
               second_contour_spacing+' apart'
         PSCLOSE,/NOVIEW
      ENDFOR 
   ENDFOR
ENDFOR

STOP
END
