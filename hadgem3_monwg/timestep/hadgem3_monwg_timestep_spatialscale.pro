PRO hadgem3_monwg_timestep_spatialscale

amzgg='/home/ss901165/um_output3/hadgem3_monwg/amzgg'
anbba='/home/ss901165/um_output3/hadgem3_monwg/anbba'
aqua='/home/ss901165/um_output5'
cascade='/home/ss901165/um_output6/cascade'
answa='/home/ss901165/um_output6/answa_N1024param'
answc='/home/ss901165/um_output6/answc_N1024explt'
trmm='/home/ss901165/datasets/TRMM_3B42V6/'

n_sets=26
thresholds=[1,5,20,40,100]
n_thresholds=N_ELEMENTS(thresholds)
domain_size=9 ; Ideally an odd number

FOR i=24,n_sets-1 DO BEGIN
   print,i
   CASE i OF
      8 : BEGIN
         infile=answa+'/wp_3hrmeans.nc'
         varname='cvrain'
         plot_title='answa (GA6 N1024p) 3hr-mean cvrain - WPac'
         start_read=1
         n_time=960
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answa_cvrain_WPac_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      9 : BEGIN
         infile=answa+'/wp_dmeans.nc'
         varname='cvrain'
         plot_title='answa (GA6 N1024p) daily-mean cvrain - WPac'
         start_read=1
         n_time=120
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answa_cvrain_WPac_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      10 : BEGIN
         infile=answa+'/wp_5dmeans.nc'
         varname='cvrain'
         plot_title='answa (GA6 N1024p) 5 day-mean cvrain - WPac'
         start_read=1
         n_time=24
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answa_cvrain_WPac_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      11 : BEGIN
         infile=answc+'/wp_3hrmeans.nc'
         varname='tot_precip'
         plot_title='answc (GA6 N1024e) 3hr-mean lsrain - WPac'
         start_read=1
         n_time=960
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answc_lsrain_WPac_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      12 : BEGIN
         infile=answc+'/wp_dmeans.nc'
         varname='tot_precip'
         plot_title='answc (GA6 N1024e) daily-mean lsrain - WPac'
         start_read=1
         n_time=120
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answc_lsrain_WPac_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      13 : BEGIN
         infile=answc+'/wp_5dmeans.nc'
         varname='tot_precip'
         plot_title='answc (GA6 N1024e) 5 day-mean lsrain - WPac'
         start_read=1
         n_time=24
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answc_lsrain_WPac_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=0
      END
      14 : BEGIN
         infile=amzgg+'/amzgga.jun-sep_3hrmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N96, 3hr means'
         start_read=0
         n_time=960
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='amzgg_totprecip_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      15 : BEGIN
         infile=amzgg+'/amzgga.jun-sep_dmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N96, daily means'
         start_read=0
         n_time=120
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='amzgg_totprecip_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      16 : BEGIN
         infile=amzgg+'/amzgga.jun-sep_5dmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N96, 5-day means'
         start_read=0
         n_time=24
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='amzgg_totprecip_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      17 : BEGIN
         infile=trmm+'n96/TRMM_3B42v6A.jan-dec_3hrmeans_ts.1999-2010.n96.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 N96 3hr means'
         start_read=0
         n_time=29200
         tsteps_per_day=8
         multiplier=24.
         psfile_title='trmm_1999_n96_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=0
      END            
      18 : BEGIN
         infile=trmm+'n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 N96 daily means'
         start_read=0
         n_time=4745
         tsteps_per_day=8
         multiplier=1.
         psfile_title='trmm_1999_n96_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=0
      END      
      20 : BEGIN
         infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_3hrmeans.2003.precip.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 0.25x0.25 3hr means'
         start_read=0
         n_time=2920
         tsteps_per_day=8
         multiplier=24.
         psfile_title='trmm_1999_0.25_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=0
      END
      19 : BEGIN
         infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2010.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 0.25x0.25 daily means'
         start_read=0
         n_time=4383
         tsteps_per_day=8
         multiplier=1.
         psfile_title='trmm_1999_0.25_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=0
      END
      21 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_3hrmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N512, 3hr means'
         start_read=0
         n_time=960
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='anbba_totprecip_3hrmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      22 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_dmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N512, daily means'
         start_read=0
         n_time=120
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='anbba_totprecip_dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      23 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_5dmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N512, 5-day means'
         start_read=0
         n_time=24
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='anbba_totprecip_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
         use_time_mean=0
      END
      24 : BEGIN
         infile=trmm+'n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 N96 five-day means'
         start_read=0
         n_time=4745
         tsteps_per_day=8
         multiplier=1.
         psfile_title='trmm_1999_n96_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=1
         time_mean=5
      END    
      25 : BEGIN
         infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2010.nc'
         varname='precip'
         plot_title='TRMM 1999-2010 0.25x0.25 five-day means'
         start_read=0
         n_time=4383
         tsteps_per_day=8
         multiplier=1.
         psfile_title='trmm_1999_0.25_5dmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,20,175]
         use_map=1
         use_time_mean=1
         time_mean=5
      END
      0 : BEGIN
         infile=answc+'/wp.nc'
         varname='cvrain'
         plot_title='answa (GA6 N1024p) 2*ts (8min) cvrain - WPac'
         start_read=1
         n_time=30000
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answa_cvrain_WPac_8minmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=1
         time_mean=2
      END
      1 : BEGIN
         infile=answc+'/wp.nc'
         varname='tot_precip'
         plot_title='answc (GA6 N1024e) 2*ts (8min) cvrain - WPac'
         start_read=1
         n_time=30000
         tsteps_per_day=15*24.
         multiplier=86400.;/tsteps_per_day
         psfile_title='answc_cvrain_WPac_8minmean'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[0,130,20,160]
         use_map=1
         use_time_mean=1
         time_mean=2
      END
      6 : BEGIN
         infile=amzgg+'/amzgga.jun-sep_tsmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N96'
         start_read=0
         n_time=8640
         tsteps_per_day=72
         multiplier=86400.;/tsteps_per_day
         psfile_title='amzgg_totprecip'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
      END
      7 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.nc'
         varname='precip'
         plot_title='GA5.0, N512'
         start_read=0
         n_time=17000
         tsteps_per_day=144
         multiplier=86400.;/tsteps_per_day
         psfile_title='anbba_totprecip'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-20,45,15,175]
         use_map=1
      END
      2 : BEGIN
         infile=aqua+'/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
         varname='tot_precip'
         plot_title='GA3.0, N96, aqua'
         start_read=0
         n_time=10000
         tsteps_per_day=72.
         multiplier=72.;/tsteps_per_day
         psfile_title='xhccr_totprecip'
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         row_map=[11,12,13,14]
         column_map=[0]
         box=[-15,0,15,360]
         use_map=1
      END
      3 : BEGIN
         infile=cascade+'/xesb_5216_15min_mean.n512.nc'
         varname='precipitation_flux'
         plot_title='Cascade 4km -> N512 - 15min mean precip'
         start_read=24
         n_time=952
         tsteps_per_day=96
         multiplier=86400./tsteps_per_day
         psfile_title='xesb_n512_15minprecip'          
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                      '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
         row_map=[0]
         column_map=[12,13,14,15]
         mean_levs=['3','6','9','12','15','18','21']
         box=[-10,45,20,175]
         use_map=1
      END    
      4 : BEGIN
         infile=cascade+'/xesb_5216_15min_mean.n96.nc'
         varname='precipitation_flux'
         plot_title='Cascade 4km -> N96 - 15min mean precip'
         start_read=24
         n_time=952
         tsteps_per_day=96
         multiplier=86400./tsteps_per_day
         psfile_title='xesb_n96_15minprecip'          
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                      '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
         row_map=[0]
         column_map=[12,13,14,15]
         mean_levs=['3','6','9','12','15','18','21']
         box=[-10,45,20,175]
         use_map=1
      END    
      5 : BEGIN
         infile=cascade+'/xesb_5216_15min_mean.nc'
         varname='precipitation_flux'
         plot_title='Cascade 4km - 15min mean precip'
         start_read=24
         n_time=952
         tsteps_per_day=96
         multiplier=86400./tsteps_per_day
         psfile_title='xesb_15minprecip'          
         mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
         mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                      '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
         row_map=[0]
         column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
         mean_levs=['3','6','9','12','15','18','21']
         box=[-10,50,10,170]
         use_map=1
      END    
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,'longitude')   
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
   precip=OPEN_AND_EXTRACT(infile,varname,$
                           offset=[box_tx(1),box_tx(0),0],$
                           count=[n_lon,n_lat,n_time])*multiplier

   IF TOTAL(where(precip le 0)) ge 0 THEN $
      precip[where(precip le 0)]=0
   IF TOTAL(where(precip ge 100000)) ge 0 THEN $
      precip[where(precip ge 100000)]=0
   
   IF use_time_mean eq 1 THEN BEGIN
      FOR j=0,n_lon-1 DO BEGIN
         FOR k=0,n_lat-1 DO BEGIN
            new=fltarr(n_time/time_mean)
            FOR m=0,n_time/time_mean-1 DO $
               new(m)=MEAN(precip(j,k,m*time_mean:(m+1)*time_mean-1))
	    precip(j,k,0:n_time/time_mean-1)=new
         ENDFOR
      ENDFOR
      precip=precip(*,*,0:n_time/time_mean-1)
      n_time=n_time/time_mean
   ENDIF
   
   n_lags=7 ; Including zero
   corr_coeffs=fltarr(domain_size,domain_size,n_lags)
   frac_wet_values=fltarr(domain_size,domain_size,n_thresholds)
   frac_dry_values=fltarr(domain_size,domain_size,n_thresholds)
   central_count_wet=fltarr(n_thresholds)
   central_count_dry=fltarr(n_thresholds)
   avg_frac_wet=fltarr(n_thresholds)
   FOR p=0,n_thresholds-1 DO $
      avg_frac_wet(p)=N_ELEMENTS(where(precip gt thresholds(p)))/FLOAT(n_lon*n_lat*n_time)

   bad_pts=0
   FOR j=0,n_lon-domain_size,domain_size DO BEGIN
      xpt=j+domain_size/2      
      FOR k=0,n_lat-domain_size,domain_size DO BEGIN
         ypt=k+domain_size/2
         central_ts=REFORM(precip(xpt,ypt,*))
         FOR p=0,n_thresholds-1 DO BEGIN
            central_ts_heaviside=fltarr(n_time)
            IF TOTAL(where(central_ts gt thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts gt thresholds(p))]=1
            IF TOTAL(where(central_ts le thresholds(p))) ge 0 THEN $
               central_ts_heaviside[where(central_ts le thresholds(p))]=0
            central_count_wet(p)=central_count_wet(p)+TOTAL(central_ts_heaviside)
            central_count_dry(p)=central_count_dry(p)+n_time-TOTAL(central_ts_heaviside)
            FOR m=xpt-domain_size/2,xpt+domain_size/2 DO BEGIN
               FOR n=ypt-domain_size/2,ypt+domain_size/2 DO BEGIN               
                  thispt_ts=REFORM(precip(m,n,*))
                  thispt_ts_heaviside=fltarr(n_time)
                  IF TOTAL(thispt_ts) ne 0 THEN BEGIN
                     IF TOTAL(where(thispt_ts gt thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts gt thresholds(p))]=1
                     IF TOTAL(where(thispt_ts le thresholds(p))) ge 0 THEN $
                        thispt_ts_heaviside[where(thispt_ts le thresholds(p))]=0
                     IF p eq 0 THEN BEGIN
                        IF TOTAL(thispt_ts_heaviside) ne 0 and TOTAL(central_ts_heaviside) ne 0 THEN BEGIN
                           FOR r=0,n_lags-1 DO $
                              corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)=$
                              corr_coeffs(m-xpt+domain_size/2,n-ypt+domain_size/2,r)+$
                              CORRELATE(central_ts(0:n_time-1-r),thispt_ts(r:n_time-1))
                        ENDIF ELSE $
                           bad_pts=bad_pts+1
                     ENDIF
                     frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                        frac_wet_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                        (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 1)))
                     frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)=$
                        frac_dry_values(m-xpt+domain_size/2,n-ypt+domain_size/2,p)+$
                        (N_ELEMENTS(where(thispt_ts_heaviside eq 1 and central_ts_heaviside eq 0)))
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR

   FOR p=0,n_thresholds-1 DO BEGIN
      frac_wet_values(*,*,p)=frac_wet_values(*,*,p)/FLOAT(central_count_wet(p))
      frac_dry_values(*,*,p)=frac_dry_values(*,*,p)/FLOAT(central_count_dry(p))
   ENDFOR
   corr_coeffs=corr_coeffs/FLOAT(corr_coeffs(domain_size/2,domain_size/2))
   
   mylevs_corr=['0.08','0.16','0.24','0.32','0.40','0.48','0.56','0.64','0.72','0.80','0.88','0.96']
   mylevs_frac=['0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50']
   mylevs_ratio=['0.20','0.25','0.33','0.50','0.75','0.90','1.11','1.50','2.00','3.00','4.00','5.00']

   FOR p=0,n_lags-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_lag'+STRTRIM(STRING(p),1)+'.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Correlation of tstep precip at (x,y) and lag '+STRTRIM(STRING(p),1)+' with (0,0) - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_corr)+1
      LEVS,MANUAL=mylevs_corr
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(corr_coeffs(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Correlation coefficient'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(corr_coeffs(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW
   ENDFOR
   
   mylevs_radvlag=['-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
          TCHARSIZE=90,SPACE3=200
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=n_lags-0.5,TITLE='Correlation with central point at t=0 - '+plot_title
   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs_radvlag)+1,white=[4]
   LEVS,MANUAL=mylevs_radvlag
   corr_plot=fltarr(domain_size/2+1,n_lags)
   FOR p=0,n_lags-1 DO BEGIN
      count=fltarr(domain_size/2+1)
      FOR r=0,domain_size-1 DO BEGIN
         FOR s=0,domain_size-1 DO BEGIN
            ;distance=SQRT((r-domain_size/2)^2+(s-domain_size/2)^2)
            distance=MAX([ABS(r-domain_size/2),ABS(s-domain_size/2)])
            corr_plot(distance,p)=corr_coeffs(r,s,p)+corr_plot(distance,p)
            count(distance)=count(distance)+1
         ENDFOR
      ENDFOR
      corr_plot(*,p)=corr_plot(*,p)/FLOAT(count)
   ENDFOR
   CON,X=indgen(domain_size/2+1),Y=indgen(n_lags),FIELD=corr_plot,/BLOCK,/NOLINES,CB_WIDTH=115,$
       CB_TITLE='Mean correlation coefficient'
   FOR j=0,domain_size/2 DO $
      FOR k=0,n_lags-1 DO $
         GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_plot(j,k)),1),0,5),ALIGN=0.5
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (same units as input data)'
   PSCLOSE,/NOVIEW

   FOR p=0,n_thresholds-1 DO BEGIN
      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.frac_wet_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given wet (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_frac)+1
      LEVS,MANUAL=mylevs_frac
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_wet_values(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Fraction of timesteps'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_wet_values(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
              STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.ratio_wet_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given wet (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[8],/REV
      LEVS,MANUAL=mylevs_ratio
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_wet_values(*,*,p)/avg_frac_wet(p)),$
          /NOLINES,/BLOCK,CB_WIDTH=110,CB_TITLE='Ratio to mean frequency'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_wet_values(j,k,p)/avg_frac_wet(p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.frac_dry_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given dry (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_frac)+1
      LEVS,MANUAL=mylevs_frac
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_dry_values(*,*,p)),/NOLINES,/BLOCK,$
          CB_WIDTH=110,CB_TITLE='Fraction of timesteps'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_dry_values(j,k,p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_spatialscale.'+psfile_title+'.'+$
             STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.ratio_dry_'+STRTRIM(STRING(thresholds(p)),1)+'mm.ps'
      PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,XSIZE=17000,YSIZE=17000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=3000,$
             TCHARSIZE=90,SPACE3=200
      GSET,XMIN=domain_size/2*(-1.)-0.5,XMAX=domain_size/2+0.5,YMIN=domain_size/2*(-1.)-0.5,YMAX=domain_size/2+0.5,$
           TITLE='Wet (x,y) given dry (0,0), thresh='+STRTRIM(STRING(thresholds(p)),1)+' mm day!U-1!N - '+plot_title
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[8],/REV
      LEVS,MANUAL=mylevs_ratio
      CON,X=indgen(domain_size)-domain_size/2,Y=indgen(domain_size)-domain_size/2,FIELD=REFORM(frac_dry_values(*,*,p)/avg_frac_wet(p)),$
          /NOLINES,/BLOCK,CB_WIDTH=110,CB_TITLE='Ratio to mean frequency'
      FOR j=0,domain_size-1 DO $
         FOR k=0,domain_size-1 DO $
            GPLOT,X=j-domain_size/2,Y=k-domain_size/2,TEXT=STRMID(STRTRIM(STRING(frac_dry_values(j,k,p)/avg_frac_wet(p)),1),0,4),ALIGN=0.5
      AXES,XSTEP=1,YSTEP=1,XTITLE='West <-- gridpoints --> East',YTITLE='South <-- gridpoints --> North'  
      PSCLOSE,/NOVIEW
   ENDFOR

ENDFOR

STOP
END

