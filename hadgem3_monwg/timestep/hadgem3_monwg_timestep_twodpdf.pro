PRO hadgem3_monwg_timestep_twodpdf

amzgg='/home/ss901165/um_output3/hadgem3_monwg/amzgg'
anbba='/home/ss901165/um_output3/hadgem3_monwg/anbba'
xhccr='/home/ss901165/um_output5/xhccr/daily'
xhccs='/home/ss901165/um_output5/xhccs/daily'
xhcct='/home/ss901165/um_output5/xhcct/daily'
cascade='/home/ss901165/um_output6/cascade'
answa='/home/ss901165/um_output6/answa_N1024param'
answc='/home/ss901165/um_output6/answc_N1024explt'
trmm='/home/ss901165/datasets/TRMM_3B42V6/'

;box=[-5,0,10,360]
box=[-10,45,20,175]
n_sets=32

heaviside=[1,2,4,6,9,12,16,20,25,30,40,60,90,130,180]
n_heavi=N_ELEMENTS(heaviside)
on_levs_ratio=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00']
on_levs=['0.02','0.04','0.06','0.08','0.10','0.12']
off_levs_ratio=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13']
off_levs=['0.50','0.60','0.70','0.80','0.90','1.00']
off_levs_map=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00']

FOR i=31,n_sets-1 DO BEGIN
    print,i
    CASE i OF
       16 : BEGIN
          infile=amzgg+'/amzgga.jun-sep_3hrmeans.1982.precip.nc'
          varname='precip'
          plot_title='amzgg (GA5.0, N96) 3hr-mean precip'
          start_read=0
          n_time=960
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='amzgg_totprecip_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       17 : BEGIN
          infile=amzgg+'/amzgga.jun-sep_dmeans.1982.precip.nc'
          varname='precip'
          plot_title='amzgg (GA5.0, N96) daily-mean precip'
          start_read=0
          n_time=120
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='amzgg_totprecip_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       18 : BEGIN
          infile=amzgg+'/amzgga.jun-sep_5dmeans.1982.precip.nc'
          varname='precip'
          plot_title='amzgg (GA5.0, N96) 5 day-mean precip'
          start_read=0
          n_time=24
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='amzgg_totprecip_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       19 : BEGIN
          infile=anbba+'/anbbaa.jun-sep_3hrmeans.1982.precip.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512) 3hr-mean precip'
          start_read=0
          n_time=960
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='anbba_totprecip_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       20 : BEGIN
          infile=anbba+'/anbbaa.jun-sep_dmeans.1982.precip.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512) daily-mean precip'
          start_read=0
          n_time=120
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='anbba_totprecip_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       21 : BEGIN
          infile=anbba+'/anbbaa.jun-sep_5dmeans.1982.precip.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512) 5 day-mean precip'
          start_read=0
          n_time=24
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='anbba_totprecip_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END       
       22 : BEGIN
          infile=answa+'/wp_3hrmeans.nc'
          varname='cvrain'
          plot_title='answa (GA6 N1024 param) 3hr-mean cvrain - WPac'
          start_read=1
          n_time=960
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='answa_cvrain_WPac_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       23 : BEGIN
          infile=answa+'/wp_dmeans.nc'
          varname='cvrain'
          plot_title='answa (GA6 N1024 param) daily-mean cvrain - WPac'
          start_read=0
          n_time=120
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='answa_cvrain_WPac_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       24 : BEGIN
          infile=answa+'/wp_5dmeans.nc'
          varname='cvrain'
          plot_title='answa (GA6 N1024 param) 5 day-mean cvrain - WPac'
          start_read=0
          n_time=24
          tsteps_per_day=1.
          multiplier=86400./tsteps_per_day
          psfile_title='answa_cvrain_WPac_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       25 : BEGIN
          infile=answc+'/wp_3hrmeans.nc'
          varname='tot_precip'
          plot_title='answc (GA6 N1024 explt) 3hr-mean lsrain - WPac'
          start_read=1
          n_time=960
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='answc_lsrain_WPac_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       26 : BEGIN
          infile=answc+'/wp_dmeans.nc'
          varname='tot_precip'
          plot_title='answc (GA6 N1024 explt) daily-mean lsrain - WPac'
          start_read=1
          n_time=120
          tsteps_per_day=1
          multiplier=86400./tsteps_per_day
          psfile_title='answc_lsrain_WPac_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       27 : BEGIN
          infile=answc+'/wp_5dmeans.nc'
          varname='tot_precip'
          plot_title='answc (GA6 N1024 explt) 5-day mean lsrain - WPac'
          start_read=1
          n_time=24
          tsteps_per_day=15*24.
          multiplier=86400./tsteps_per_day
          psfile_title='answc_lsrain_WPac_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=0
       END
       28 : BEGIN
          infile=trmm+'n96/TRMM_3B42v6A.jan-dec_3hrmeans_ts.1999-2010.n96.nc'
          varname='precip'
          plot_title='TRMM N96 3hr mean precip'
          start_read=0
          n_time=29200
          tsteps_per_day=1
          multiplier=24.
          psfile_title='trmm_n96_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       29 : BEGIN
          infile=trmm+'n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
          varname='precip'
          plot_title='TRMM N96 daily-mean precip'
          start_read=0
          n_time=4745
          tsteps_per_day=1
          multiplier=1.
          psfile_title='trmm_n96_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       30 : BEGIN
          infile=trmm+'n96/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2011.n96.nc'
          varname='precip'
          plot_title='TRMM N96 5-day mean precip'
          start_read=0
          n_time=4745
          tsteps_per_day=1
          multiplier=1.
          psfile_title='trmm_n96_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=1
          time_mean=5
       END
       31 : BEGIN
          infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_3hrmeans.2003.precip.nc'
          varname='precip'
          plot_title='TRMM 0.25x0.25 3hr mean precip'
          start_read=0
          n_time=2920
          tsteps_per_day=1
          multiplier=24.
          psfile_title='trmm_0.25_3hrmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       32 : BEGIN
          infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2010.nc'
          varname='precip'
          plot_title='TRMM 0.25x0.25 dmean precip'
          start_read=0
          n_time=4383
          tsteps_per_day=1
          multiplier=1.
          psfile_title='trmm_0.25_dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=0
       END
       33 : BEGIN
          infile=trmm+'native_resolution/TRMM_3B42v6A.jan-dec_dmeans_ts.1999-2010.nc'
          varname='precip'
          plot_title='TRMM 0.25x0.25 5-day mean precip'
          start_read=0
          n_time=4383
          tsteps_per_day=1
          multiplier=1.
          psfile_title='trmm_0.25_5dmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
          ysize_map=12000
          use_time_mean=1
          time_mean=5
       END
       0 : BEGIN
          infile=answa+'/wp.nc'
          varname='cvrain'
          plot_title='answa (GA6 N1024 param) timestep cvrain - WPac'
          start_read=1
          n_time=30000
          tsteps_per_day=15*24.
          multiplier=86400./tsteps_per_day
          psfile_title='answa_cvrain_WPac_8minmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1       
          ysize_map=12000
          use_time_mean=1
          time_mean=2
       END
       1 : BEGIN
          infile=answc+'/wp.nc'
          varname='lsrain'
          plot_title='answc (GA6 N1024 explt) timestep cvrain - WPac'
          start_read=1
          n_time=30000
          tsteps_per_day=15*24.
          multiplier=86400./tsteps_per_day
          psfile_title='answc_cvrain_WPac_8minmean'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[0,130,20,160]
          use_map=1
          ysize_map=12000
          use_time_mean=1
          time_mean=2
       END
       14 : BEGIN
          infile=cascade+'/xewx_5216_15min_mean.nc'
          varname='precipitation_flux'
          plot_title='xewx (Cascade 40km param) 15min mean precip'
          start_read=24
          n_time=1920
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xewx_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END
       15 : BEGIN
          infile=cascade+'/xewx_5216_15min_mean.n96.nc'
          varname='precipitation_flux'
          plot_title='xewx (Cascade 40km param -> N96) 15min mean precip'
          start_read=24
          n_time=1920
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xewx_n96_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END
       2 : BEGIN
          infile=cascade+'/xfvb_5216_15min_mean.nc'
          varname='precipitation_flux'
          plot_title='xfvb (Cascade 12km param) 15min mean precip'
          start_read=24
          n_time=960
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xfvb_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END
       3 : BEGIN
          infile=cascade+'/xfvb_5216_15min_mean.n96.nc'
          varname='precipitation_flux'
          plot_title='xfvb (Cascade 12km param -> N96) 15min mean precip'
          start_read=24
          n_time=960
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xfvb_n96_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END
       4 : BEGIN
          infile=cascade+'/xesb_5216_15min_mean.nc'
          varname='precipitation_flux'
          plot_title='xesb (Cascade 4km 2Dsmag) 15min mean precip'
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
          box=[-5,70,5,80]
          use_map=1
       END
       5 : BEGIN
          infile=cascade+'/xesb_5216_15min_mean.n96.nc'
          varname='precipitation_flux'
          plot_title='xesb (Cascade 4km 2Dsmag -> N96) 15min mean precip'
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
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END    
       6 : BEGIN
          infile=cascade+'/xfhf_5216_15min_mean.nc'
          varname='precipitation_flux'
          plot_title='xfhf (Cascade 4km 3Dsmag) 15min mean precip'
          start_read=24
          n_time=576
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xfhf_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-5,70,5,80]
          use_map=1
       END
       7 : BEGIN
          infile=cascade+'/xfhf_5216_15min_mean.n96.nc'
          varname='precipitation_flux'
          plot_title='xfhf (Cascade 4km 3Dsmag -> N96) 15min mean precip'
          start_read=24
          n_time=952
          tsteps_per_day=96
          multiplier=86400./tsteps_per_day
          psfile_title='xfhf_n96_15minprecip'          
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[12,13,14,15]
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,45,20,175]
          use_map=1
       END    
       8 : BEGIN
          infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512) total precip tstep'
          start_read=0
          n_time=17280
          tsteps_per_day=144
          multiplier=86400./tsteps_per_day
          psfile_title='anbba_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']                                
          row_map=[0]
          column_map=[12,13,14,15]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,60,10,90]
          use_map=1
          use_time_mean=0
       END
       9 : BEGIN
          infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.n96.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512->N96) total precip tstep'
          start_read=0
          n_time=17280
          tsteps_per_day=144
          multiplier=86400./tsteps_per_day
          psfile_title='anbba_n96_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[0]
          column_map=[11,12,13,14]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,175]
          use_map=1
       END       
       10 : BEGIN
          infile=amzgg+'/amzgga.jun-sep_tsmeans.1982.precip.nc'
          varname='precip'
          plot_title='amzgg (GA5.0, N96) total precip tstep'
          start_read=0
          n_time=8640
          tsteps_per_day=72
          multiplier=86400./tsteps_per_day
          psfile_title='amzgg_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']     
          row_map=[11,12,13,14]
          column_map=[0]
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-20,45,20,200]
          use_map=1
       END       
       11 : BEGIN
          infile=xhccr+'/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
          varname='tot_precip'
          plot_title='xhccr (GA3.0, N96, aquaplanet) total precip tstep'
          start_read=10000
          n_time=20000
          tsteps_per_day=72
          multiplier=1
          psfile_title='xhccr_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
;          mylevs_norm=['0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
;                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4']
          
;          map_levs=['0.001','0.002','0.003','0.005','0.007','0.01','0.02','0.03','0.05','0.07','0.1','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          box=[-10,0,20,360]
          use_map=0
          row_map=[11,12,13,14]
          column_map=[0]
       END
       12 : BEGIN
          infile=xhccs+'/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
          varname='tot_precip'
          plot_title='xhccs (GA3.0, N96, aquaplanet) total precip tstep'
          start_read=10000
          n_time=20000
          tsteps_per_day=72
          multiplier=1
          psfile_title='xhccs_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
;          mylevs_norm=['0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
;                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4']
          box=[-10,0,20,360]
          use_map=0
       END
       13 : BEGIN
          infile=xhcct+'/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
          varname='tot_precip'
          plot_title='xhcct (GA3.0, N96, aquaplanet) total precip tstep'
          start_read=10000
          n_time=20000
          tsteps_per_day=72
          multiplier=1
          psfile_title='xhcct_totprecip'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
;          mylevs_norm=['0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
;                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4']
          box=[-10,0,20,360]
          use_map=0
       END
    ENDCASE
    
    twod_pdf_pwet=fltarr(n_heavi+1,n_heavi+1)
    twod_pdf_prct=fltarr(n_heavi+1,n_heavi+1)
    oned_pdf=fltarr(n_heavi+1)
    oned_pdf_prct=fltarr(n_heavi+1)
    heaviside_tstep=heaviside/FLOAT(tsteps_per_day)

    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
    precip=REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                   offset=[box_tx(1),box_tx(0),start_read],$
                                   count=[n_lon,n_lat,n_time-start_read]))*multiplier

    IF TOTAL(where(precip ge 10000)) ge 0 THEN $
       precip[where(precip ge 10000)]=!Values.F_NaN

    IF use_time_mean eq 1 THEN BEGIN
       FOR j=0,n_lon-1 DO BEGIN
          FOR k=0,n_lat-1 DO BEGIN
             new=fltarr(n_time-start_read)/time_mean
             FOR m=0,(n_time-start_read)/time_mean-1 DO $
                new(m)=MEAN(precip(j,k,m*time_mean:(m+1)*time_mean-1))
             precip(j,k,*)=new(*)
          ENDFOR
       ENDFOR
    ENDIF
    

    precip_mean=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO $
       FOR k=0,n_lat-1 DO $
          precip_mean(j,k)=MEAN(precip(j,k,*))*FLOAT(tsteps_per_day)

    spatial_freq=fltarr(n_lon,n_lat)
    spatial_freq_onoff=fltarr(n_lon,n_lat)
    spatial_freq_offon=fltarr(n_lon,n_lat)
    spatial_freq_on=fltarr(n_lon,n_lat)
    spatial_freq_off=fltarr(n_lon,n_lat)

    FOR j=0,n_lon-1 DO BEGIN
       FOR k=0,n_lat-1 DO BEGIN
          ts=REFORM(precip(j,k,*))
          FOR m=0,n_heavi-1 DO BEGIN
             IF TOTAL(where(ts ge heaviside_tstep(m))) ge 0 THEN BEGIN
                valid=where(ts ge heaviside_tstep(m))
                oned_pdf(m+1)=oned_pdf(m+1)+N_ELEMENTS(valid)
                next=ts[valid+1]                
                FOR n=0,n_heavi-1 DO BEGIN
                   IF TOTAL(where(next ge heaviside_tstep(n))) ge 0 THEN $
                      twod_pdf_pwet(m+1,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n)))+twod_pdf_pwet(m+1,n+1)
                ENDFOR
                IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
                   twod_pdf_pwet(m+1,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_pwet(m+1,0)
             ENDIF
          ENDFOR
	  FOR m=0,n_heavi-2 DO BEGIN
	     IF TOTAL(where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))) ge 0 THEN BEGIN
                valid=where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))
                oned_pdf_prct(m+1)=oned_pdf_prct(m+1)+N_ELEMENTS(valid)
                next=ts[valid+1]
                FOR n=0,n_heavi-2 DO BEGIN
                   IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                      twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(m+1,n+1)
                ENDFOR
                IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN BEGIN
                   twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(m+1,0)
                   IF where(row_map eq m) ge 0 THEN BEGIN
                      spatial_freq_onoff(j,k)=spatial_freq_onoff(j,k)+N_ELEMENTS(where(next lt heaviside_tstep(0)))
                      spatial_freq_on(j,k)=spatial_freq_on(j,k)+N_ELEMENTS(valid)
                   ENDIF
                ENDIF
		IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                   twod_pdf_prct(m+1,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(m+1,n_heavi)
	     ENDIF 
	  ENDFOR
          IF TOTAL(where(ts lt heaviside_tstep(0))) ge 0 THEN BEGIN
             valid=where(ts lt heaviside_tstep(0))
             oned_pdf(0)=oned_pdf(0)+N_ELEMENTS(valid)
             oned_pdf_prct(0)=oned_pdf_prct(0)+N_ELEMENTS(valid)
             next=ts[valid+1]
             spatial_freq_off(j,k)=spatial_freq_off(j,k)+N_ELEMENTS(valid)
             FOR n=0,n_heavi-1 DO BEGIN
                IF TOTAL(where(next ge heaviside_tstep(n))) ge 0 THEN $
                   twod_pdf_pwet(0,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n)))+twod_pdf_pwet(0,n+1)
             ENDFOR
             FOR n=0,n_heavi-2 DO BEGIN
                IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN BEGIN
                   twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(0,n+1)                
                   IF where(row_map eq n) ge 0 THEN $
                      spatial_freq_offon(j,k)=spatial_freq_offon(j,k)+N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))
                ENDIF
             ENDFOR
             IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
                twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(0,0)
             IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                twod_pdf_prct(0,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(0,n_heavi)
             IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
                twod_pdf_pwet(0,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_pwet(0,0)
          ENDIF           
          IF TOTAL(where(ts ge heaviside_tstep(n_heavi-1))) ge 0 THEN BEGIN
             valid=where(ts ge heaviside_tstep(n_heavi-1))
             oned_pdf_prct(n_heavi)=oned_pdf_prct(n_heavi)+N_ELEMENTS(valid)
             next=ts[valid+1]
             FOR n=0,n_heavi-2 DO BEGIN
                IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                   twod_pdf_prct(n_heavi,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(n_heavi,n+1)             
             ENDFOR
             IF TOTAL(where(next lt heaviside_tstep(0))) ge 0 THEN $
                twod_pdf_prct(n_heavi,0)=N_ELEMENTS(where(next lt heaviside_tstep(0)))+twod_pdf_prct(n_heavi,0)
             IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                twod_pdf_prct(n_heavi,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(n_heavi,n_heavi)
          ENDIF
       ENDFOR
    ENDFOR

    spatial_freq=spatial_freq/FLOAT(n_time)
    spatial_freq_on=spatial_freq_on/FLOAT(n_time)
    spatial_freq_off=spatial_freq_off/FLOAT(n_time)
    spatial_freq_onoff=spatial_freq_onoff/FLOAT(n_time)
    spatial_freq_offon=spatial_freq_offon/FLOAT(n_time)

    FOR m=0,n_heavi DO $
       twod_pdf_pwet(m,*)=twod_pdf_pwet(m,*)/FLOAT(oned_pdf(m))
    oned_pdf=oned_pdf/(FLOAT(n_lon)*FLOAT(n_lat)*FLOAT(n_time))
    oned_pdf_prct=oned_pdf_prct/TOTAL(oned_pdf_prct)
    
    twod_pdf_prct_norm_col=fltarr(n_heavi+1,n_heavi+1)
    twod_pdf_prct_norm_row=fltarr(n_heavi+1,n_heavi+1)
    FOR m=0,n_heavi DO BEGIN
       twod_pdf_prct_norm_col(m,*)=twod_pdf_prct(m,*)/TOTAL(twod_pdf_prct(m,*))
       twod_pdf_prct_norm_row(*,m)=twod_pdf_prct(*,m)/TOTAL(twod_pdf_prct(*,m))
    ENDFOR
    twod_pdf_prct=twod_pdf_prct/TOTAL(twod_pdf_prct)
;    FOR m=0,n_heavi DO $
;       twod_pdf_prct(m,*)=twod_pdf_prct(m,*)/TOTAL(twod_pdf_prct(m,*))

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_pwet.ps'
    PSOPEN,file=psfile,TFONT=6,CHARSIZE=135,FONT=6,TCHARSIZE=110,XSIZE=16000,YSIZE=16000,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    LEVS,MANUAL=mylevs
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_pwet,/BLOCK,/NOLINES,CB_WIDTH=125,CB_TITLE='Probability'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+1)+0.5,YVALS=indgen(n_heavi+1)+0.5,YLABELS=['< '+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1)],$
         XLABELS=['< '+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1)],$
         YTITLE='Threshold on timestep t+1 (mm day!U-1!N)',XTITLE='Threshold on timestep t (mm day!U-1!N)',/NORIGHT
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.005,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf,STYLE=2
    AXES,YVALS=['0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation above threshold',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    twod_pdf_pwet_norm=fltarr(n_heavi+1,n_heavi+1)
    FOR m=0,n_heavi DO $
       twod_pdf_pwet_norm(m,*)=twod_pdf_pwet(m,*)/oned_pdf

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_pwet_normalised.ps'
    PSOPEN,file=psfile,TFONT=6,CHARSIZE=135,FONT=6,TCHARSIZE=110,XSIZE=16000,YSIZE=16000,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
    LEVS,MANUAL=mylevs_norm
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_pwet_norm,/BLOCK,/NOLINES,CB_WIDTH=125,CB_TITLE='Ratio of probability'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+1)+0.5,YVALS=indgen(n_heavi+1)+0.5,YLABELS=['< '+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1)],$
         XLABELS=['< '+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1)],$
         YTITLE='Threshold on timestep t+1 (mm day!U-1!N)',XTITLE='Threshold on timestep t (mm day!U-1!N)',/NORIGHT
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.005,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf,STYLE=2
    AXES,YVALS=['0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation above threshold',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=100,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
    LEVS,MANUAL=mylevs_prct
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Probability',TITLE=psfile_title
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT    
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    IF TOTAL(where(oned_pdf_prct lt 0.001)) ge 0 THEN $
       oned_pdf_prct[where(oned_pdf_prct lt 0.001)]=!Values.F_NaN
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2,SYM=3
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_col.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    LEVS,MANUAL=mylevs
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_row.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    LEVS,MANUAL=mylevs
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW
    
    FOR m=0,n_heavi DO BEGIN
       twod_pdf_prct_norm_col(m,*)=twod_pdf_prct_norm_col(m,*)/oned_pdf_prct
       twod_pdf_prct_norm_row(*,m)=twod_pdf_prct_norm_row(*,m)/oned_pdf_prct
    ENDFOR

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_col2.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
    LEVS,MANUAL=mylevs_norm
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column and PDF)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_row2.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
    LEVS,MANUAL=mylevs_norm
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row and PDF)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW
 
    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_onoff_freq.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    IF use_map eq 1 THEN BEGIN
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ENDIF ELSE $
       GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    CS,SCALE=26,NCOLS=N_ELEMENTS(map_levs)+1,white=[2]
    LEVS,MANUAL=map_levs
    IF i ne 5 THEN BEGIN
       CON,X=longitude,Y=latitude,FIELD=spatial_freq,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are on (>= 40 mm/day) then off (< 1 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq)),1)+' )',0,5)+' - '+psfile_title
    ENDIF ELSE $
       CON,X=longitude,Y=latitude,FIELD=spatial_freq,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are on (>= 60 mm/day) then off (< 1 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq)),1)+' )',0,5)+' - '+psfile_title
    LEVS,MANUAL=mean_levs
    CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    AXES
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_on_freq.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    IF use_map eq 1 THEN BEGIN
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ENDIF ELSE $
       GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    CS,SCALE=26,NCOLS=N_ELEMENTS(map_levs)+1,white=[2]
    LEVS,MANUAL=map_levs
    IF i ne 5 THEN BEGIN
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are on (>= 40 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq_on)),1)+' )',0,5)+' - '+psfile_title
    ENDIF ELSE $
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are on (>= 60 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq_on)),1)+' )',0,5)+' - '+psfile_title
    LEVS,MANUAL=mean_levs
    CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    AXES
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_onoff_ratio.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    IF use_map eq 1 THEN BEGIN
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ENDIF ELSE $
       GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    CS,SCALE=26,NCOLS=N_ELEMENTS(on_levs_ratio)+1,white=[2]
    LEVS,MANUAL=on_levs_ratio
    IF i ne 5 THEN BEGIN
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_onoff/spatial_freq_on,CB_TITLE='Probability',/NOLINES,/BLOCK,$
           TITLE='Probability that "on" (> 40 mm/day) is followed by "off" (< 1 mm/day) - '+psfile_title
    ENDIF ELSE $
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_onoff/spatial_freq_on,CB_TITLE='Probability',/NOLINES,/BLOCK,$
           TITLE='Probability that "on" (> 40 mm/day) is followed by "off" (< 1 mm/day) - '+psfile_title
    LEVS,MANUAL=on_levs
    CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,/NOFILL,POSITIVE_STYLE=2
    AXES
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_off_freq.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    IF use_map eq 1 THEN BEGIN
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ENDIF ELSE $
       GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    CS,SCALE=26,NCOLS=N_ELEMENTS(off_levs_map)+1,white=[2]
    LEVS,MANUAL=off_levs_map
    IF i ne 5 THEN BEGIN
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are off (< 1 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq_off)),1)+' )',0,5)+' - '+psfile_title
    ENDIF ELSE $
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
           TITLE='Frac of tsteps that are off (< 1 mm/day) - avg freq = '+$
           STRMID(STRTRIM(STRING(MEAN(spatial_freq_off)),1)+' )',0,5)+' - '+psfile_title
    LEVS,MANUAL=mean_levs
    CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    AXES
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_offon_ratio.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    IF use_map eq 1 THEN BEGIN
       MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ENDIF ELSE $
       GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    CS,SCALE=26,NCOLS=N_ELEMENTS(off_levs_ratio)+1,white=[2]
    LEVS,MANUAL=off_levs_ratio
    IF i ne 5 THEN BEGIN
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_offon/spatial_freq_off,CB_TITLE='Probability',/NOLINES,/BLOCK,$
           TITLE='Probability that "off" (< 1 mm/day) is followed by "on" (> 40 mm/day) - '+psfile_title
    ENDIF ELSE $
       CON,X=longitude,Y=latitude,FIELD=spatial_freq_offon/spatial_freq_off,CB_TITLE='Probability',/NOLINES,/BLOCK,$
           TITLE='Probability that "off" (< 1 mm/day) is followed by "on" (> 40 mm/day) - '+psfile_title
    LEVS,MANUAL=off_levs
    CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,/NOFILL,POSITIVE_STYLE=2
    AXES
    PSCLOSE

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_scatter_onoff_freq.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=160,TCHARSIZE=100,SPACE2=2000,YOFFSET=1000,MARGIN=3000,XOFFSET=1000
    GSET,XMIN=0,XMAX=40,YMIN=0,YMAX=0.2,TITLE=plot_title
    FOR j=0,n_lon-1 DO $
       FOR k=0,n_lat-1 DO $
          GPLOT,X=precip_mean(j,k),Y=spatial_freq(j,k),SYM=5,SIZE=40
    GPLOT,X=1,Y=0.18,TEXT='Correlation = '+STRMID(STRTRIM(STRING(CORRELATE(precip_mean,spatial_freq)),1),0,5),ALIGN=0.0
    AXES,XTITLE='Mean precipitation (mm day!U-1!N)',YTITLE='Fractional frequency of on/off or off/on',$
         XSTEP=4,XMINOR=2,YSTEP=0.02,YMINOR=0.01,NDECS=2
    PSCLOSE,/NOVIEW

 ENDFOR

STOP
END

       
