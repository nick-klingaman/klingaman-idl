PRO qld_higem_dblco2_precip_transstbl_distributions

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_trans_indir='/home/ss901165/higem_qccce/es_2pctco2_eafee'
higem_stbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_stable_nyears=31
higem_trans_nyears=20

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

box=[-50,90,-1,180]
qldbox=[-30,138,-10,154]

n_30yr_runs=200                    ; Number of 30 year runs to sample from control integration
n_10yr_runs=40
control_nchunks=3
runs_levels=['0.01','0.05','0.10','0.15','0.20','0.80','0.85','0.90','0.95','0.99']
runs_10yr_levels=['-0.99','-0.95','-0.90','-0.85','-0.80','0.80','0.85','0.90','0.95','0.99']

n_seasons=5
FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         season_name='dec-feb'
         season_type='smean'
         season_ndays=90.
         bins_10yrmeans=[210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360]
         bins_10yriav=[16,24,32,40,48,56,64,72,80,88,96,104,112,120,128]
         bins_30yrmeans=[240,245,250,255,260,265,270,275,280,285,290,295,300,305,310,315]
         bins_30yriav=[56,60,64,68,72,76,80,84,88,92,96,100,104,108]
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='smean'
         season_ndays=90.
         bins_10yrmeans=[110,120,130,140,150,160,170,180,190,200,210,220,230,240]
         bins_10yriav=[16,24,32,40,48,56,64,72,80,88,96,104,112]
         bins_30yrmeans=[140,145,150,155,160,165,170,175,180,185,190,195,200,205]
         bins_30yriav=[24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84]
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='smean'
         season_ndays=90.
         bins_10yrmeans=[12,16,20,24,28,32,36,40,44,48,52,56,60,64]
         bins_10yriav=[0,4,8,12,16,20,24,28,32,36,40,44,48,52,56]
         bins_30yrmeans=[22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52]
         bins_30yriav=[14,16,18,20,22,24,26,28,30,32,34,36,38,40]
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='smean'
         season_ndays=90.
         bins_10yrmeans=[25,30,35,40,45,50,55,60,65,70,75,80,85,90]
         bins_10yriav=[4,8,12,16,20,24,28,32,36,40,44,48,52,56,60]
         bins_30yrmeans=[40,44,48,52,56,60,64,68,72,76,80,84,88,92]
         bins_30yriav=[16,18,20,22,24,26,28,30,32,34,36,38,40,42,44]
      END
      4 : BEGIN
         season_name='may-apr'
         season_type='amean'
         season_ndays=360.
         bins_10yrmeans=[460,475,490,505,520,535,550,565,580,595,610,625,640,655,670]
         bins_10yriav=[20,35,50,65,80,95,110,125,140,155,170,185,200,215]
         bins_30yrmeans=[470,480,490,500,510,520,530,540,550,560,570,580,590,600,610]
         bins_30yriav=[62,70,78,86,94,102,110,118,126,134,142,150,158,166]
      END
   ENDCASE
   n_bins_10yrmeans=N_ELEMENTS(bins_10yrmeans)
   n_bins_10yriav=N_ELEMENTS(bins_10yriav)
   n_bins_30yrmeans=N_ELEMENTS(bins_30yrmeans)
   n_bins_30yriav=N_ELEMENTS(bins_30yriav)

   higem_control_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.precip.global_domain.nc'
   higem_trans_infile=higem_trans_indir+'/higem_eafee.'+season_name+'_'+season_type+'s.k9-u7.precip.global_domain.nc'
   higem_stable_infile=higem_stbl_indir+'/higem_eadwu.'+season_name+'_'+season_type+'s.o2-r3.precip.global_domain.nc'

   control_longitude=OPEN_AND_EXTRACT(higem_control_infile,'longitude')
   control_latitude=OPEN_AND_EXTRACT(higem_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_latitude,control_longitude,control_box_tx,/LIMIT
   DEFINE_BOUNDARIES,qldbox,control_latitude,control_longitude,control_qldbox_tx
   control_nlon=N_ELEMENTS(control_longitude)
   control_nlat=N_ELEMENTS(control_latitude)

   trans_longitude=OPEN_AND_EXTRACT(higem_trans_infile,'longitude')
   trans_latitude=OPEN_AND_EXTRACT(higem_trans_infile,'latitude')
   DEFINE_BOUNDARIES,box,trans_latitude,trans_longitude,trans_box_tx,/LIMIT
   DEFINE_BOUNDARIES,qldbox,trans_latitude,trans_longitude,trans_qldbox_tx
   trans_nlon=N_ELEMENTS(trans_longitude)
   trans_nlat=N_ELEMENTS(trans_latitude)

   stable_longitude=OPEN_AND_EXTRACT(higem_stable_infile,'longitude')
   stable_latitude=OPEN_AND_EXTRACT(higem_stable_infile,'latitude')
   DEFINE_BOUNDARIES,box,stable_latitude,stable_longitude,stable_box_tx,/LIMIT
   DEFINE_BOUNDARIES,qldbox,stable_latitude,stable_longitude,stable_qldbox_tx
   stable_nlon=N_ELEMENTS(stable_longitude)
   stable_nlat=N_ELEMENTS(stable_latitude)

   control_precip=OPEN_AND_EXTRACT(higem_control_infile,'precip',$
                                   offset=[control_box_tx(1),control_box_tx(0),0],$
                                   count=[control_nlon,control_nlat,higem_control_nyears])*86400.*season_ndays
   trans_precip=OPEN_AND_EXTRACT(higem_trans_infile,'precip',$
                                 offset=[trans_box_tx(1),trans_box_tx(0),17],$
                                 count=[trans_nlon,trans_nlat,higem_trans_nyears])*86400.*season_ndays
   stable_precip=OPEN_AND_EXTRACT(higem_stable_infile,'precip',$
                                  offset=[stable_box_tx(1),stable_box_tx(0),0],$
                                  count=[stable_nlon,stable_nlat,higem_stable_nyears])*86400.*season_ndays
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[stable_box_tx(1),stable_box_tx(0),0,0],$
                                count=[stable_nlon,stable_nlat,1,1]))
   
                                ; For randomly sampled 30 year means from control against the 30 year mean
                                ; from the 2x CO2 fixed integration

   stable_position_30years=fltarr(stable_nlon,stable_nlat)
   FOR j=0,control_nlon-1 DO BEGIN
      FOR k=0,control_nlat-1 DO BEGIN
         ; Generate vector of random numbers
         sample_starts=FIX(119*RANDOMU(seed,n_30yr_runs))
         run_means=fltarr(n_30yr_runs)
         FOR m=0,n_30yr_runs-1 DO $
            run_means(m)=MEAN(control_precip(j,k,sample_starts(m):sample_starts(m)+29))

         stable_mean=MEAN(stable_precip(j,k,*))
         stable_position_30years(j,k)=N_ELEMENTS(where(stable_mean lt run_means))         
      ENDFOR
   ENDFOR
   
                                ; For randomly sampled 10 year means from control against the same from the
                                ; 2x CO2 fixed integration for (appending some years
                                ; from the transient integration to each end of the fixed CO2 integration)
;   stable_position_10years_pos=fltarr(stable_nlon,stable_nlat)
;   stable_position_10years_neg=fltarr(stable_nlon,stable_nlat)
;   stable_position_10years=fltarr(stable_nlon,stable_nlat)
;   FOR j=0,control_nlon-1 DO BEGIN
;      FOR k=0,control_nlat-1 DO BEGIN
;         control_sample_starts=FIX(139*RANDOMU(control_seed_10yr,n_10yr_runs))
;         stable_sample_starts=FIX((higem_stable_nyears+higem_trans_nyears-10)*(RANDOMU(trans_seed_10yr,n_10yr_runs)))
;         
;         control_ts=REFORM(control_precip(j,k,*))
;         stable_ts=[REFORM(trans_precip(j,k,0:12)),REFORM(stable_precip(j,k,*)),$
;                    REFORM(trans_precip(j,k,13:higem_trans_nyears-1))] 
;
;         control_run_means=fltarr(n_10yr_runs)
;         stable_run_means=fltarr(n_10yr_runs)
;         FOR m=0,n_10yr_runs-1 DO BEGIN
;            control_run_means(m)=MEAN(control_ts(control_sample_starts(m):control_sample_starts(m)+9))
;            stable_run_means(m)=MEAN(stable_ts(stable_sample_starts(m):stable_sample_starts(m)+9))            
;            FOR n=0,n_10yr_runs-1 DO BEGIN
;               stable_sigfraction_runs=N_ELEMENTS(where(stable_run_means(n) lt control_run_means))/FLOAT(n_10yr_runs)
;               IF stable_sigfraction_runs gt 0.90 THEN $
;                  stable_position_10years_pos(j,k)=stable_position_10years_pos(j,k)+1
;               IF stable_sigfraction_runs lt 0.10 THEN $
;                  stable_position_10years_neg(j,k)=stable_position_10years_neg(j,k)-1
;            ENDFOR
;         ENDFOR
;      ENDFOR
;   ENDFOR
;   stable_position_10years_pos=stable_position_10years_pos/FLOAT(n_10yr_runs*n_10yr_runs)
;   stable_position_10years_neg=stable_position_10years_neg/FLOAT(n_10yr_runs*n_10yr_runs)
;   FOR j=0,control_nlon-1 DO BEGIN
;      FOR k=0,control_nlat-1 DO BEGIN
;         stable_position_10years(j,k)=MAX([ABS(stable_position_10years_pos(j,k)),ABS(stable_position_10years_neg(j,k))])
;         IF ABS(stable_position_10years_neg(j,k)) gt ABS(stable_position_10years_pos(j,k)) THEN $
;            stable_position_10years(j,k)=stable_position_10years(j,k)*(-1.)
;      ENDFOR
;   ENDFOR
   
                                ; Do the same 10 year random resampling for the all-Queensland rainfall,
                                ; then plot the distributions

   control_qldaavg=fltarr(higem_control_nyears)
   stable_qldaavg=fltarr(higem_stable_nyears+higem_trans_nyears)
   FOR j=0,higem_control_nyears-1 DO BEGIN
      temp=REFORM(control_precip(*,*,j))
      temp[where(mask eq 0)]=!Values.F_NaN
      control_qldaavg(j)=MEAN(temp(control_qldbox_tx(1):control_qldbox_tx(3),$
                                   control_qldbox_tx(0):control_qldbox_tx(2)),/NaN)
   ENDFOR
   FOR j=0,higem_stable_nyears+higem_trans_nyears-1 DO BEGIN
      IF j lt 13 THEN BEGIN
         temp=REFORM(trans_precip(*,*,j))
         temp[where(mask eq 0)]=!Values.F_NaN
         stable_qldaavg(j)=MEAN(temp(trans_qldbox_tx(1):trans_qldbox_tx(3),$
                                     trans_qldbox_tx(0):trans_qldbox_tx(2)),/NaN)
      ENDIF ELSE IF j ge 13 and j lt 13+higem_stable_nyears-1 THEN BEGIN
         temp=REFORM(stable_precip(*,*,j-13))
         temp[where(mask eq 0)]=!Values.F_NaN
         stable_qldaavg(j)=MEAN(temp(stable_qldbox_tx(1):stable_qldbox_tx(3),$
                                     stable_qldbox_tx(0):stable_qldbox_tx(2)),/NaN)
      ENDIF ELSE BEGIN
         temp=REFORM(trans_precip(*,*,j-higem_stable_nyears))
         temp[where(mask eq 0)]=!Values.F_NaN
         stable_qldaavg(j)=MEAN(temp(trans_qldbox_tx(1):trans_qldbox_tx(3),$
                                     trans_qldbox_tx(0):trans_qldbox_tx(2)),/NaN)
      ENDELSE
   ENDFOR

   control_qldaavg_10yrmeans=fltarr(control_nchunks,n_10yr_runs)
   control_qldaavg_10yriav=fltarr(control_nchunks,n_10yr_runs)
   stable_qldaavg_10yrmeans=fltarr(n_10yr_runs)
   stable_qldaavg_10yriav=fltarr(n_10yr_runs)
   control_qldaavg_30yrmeans=fltarr(n_30yr_runs)
   control_qldaavg_30yriav=fltarr(n_30yr_runs)
;   
;   control_10yr_sample_starts=FIX((higem_control_nyears-10)*RANDOMU(control_10yr_qld_seed,n_10yr_runs))
;   stable_10yr_sample_starts=FIX((higem_stable_nyears+higem_trans_nyears-10)*RANDOMU(stable_10yr_qld_seed,n_10yr_runs))
   control_30yr_sample_starts=FIX((higem_control_nyears-30)*RANDOMU(control_30yr_qld_seed,n_30yr_runs))
;   
;   FOR j=0,n_10yr_runs-1 DO BEGIN
;      control_qldaavg_10yrmeans(j)=MEAN(control_qldaavg(control_10yr_sample_starts(j):control_10yr_sample_starts(j)+9))
;      stable_qldaavg_10yrmeans(j)=MEAN(stable_qldaavg(stable_10yr_sample_starts(j):stable_10yr_sample_starts(j)+9))
;   ENDFOR
   FOR j=0,n_10yr_runs-1 DO BEGIN
      stable_qldaavg_10yrmeans(j)=MEAN(stable_qldaavg(j:j+9))
      stable_qldaavg_10yriav(j)=STDDEV(stable_qldaavg(j:j+9))
      FOR k=0,control_nchunks-1 DO BEGIN
         control_qldaavg_10yrmeans(k,j)=MEAN(control_qldaavg(k*49+j:k*49+j+9))
         control_qldaavg_10yriav(k,j)=STDDEV(control_qldaavg(k*49+j:k*49+j+9))
      ENDFOR
   ENDFOR
   FOR j=0,n_30yr_runs-1 DO BEGIN
      control_qldaavg_30yrmeans(j)=MEAN(control_qldaavg(control_30yr_sample_starts(j):control_30yr_sample_starts(j)+29))
      control_qldaavg_30yriav(j)=STDDEV(control_qldaavg(control_30yr_sample_starts(j):control_30yr_sample_starts(j)+29))
   ENDFOR   
   ;print,control_qldaavg_30yriav
 
   control_qldaavg_10yrmeans_binned=fltarr(control_nchunks,n_bins_10yrmeans)
   control_qldaavg_10yriav_binned=fltarr(control_nchunks,n_bins_10yriav)
   stable_qldaavg_10yrmeans_binned=fltarr(n_bins_10yrmeans)
   stable_qldaavg_10yriav_binned=fltarr(n_bins_10yriav)
   FOR k=0,control_nchunks-1 DO BEGIN
      control_qldaavg_10yrmeans_indices=VALUE_LOCATE(bins_10yrmeans,REFORM(control_qldaavg_10yrmeans(k,*)))
      control_qldaavg_10yriav_indices=VALUE_LOCATE(bins_10yriav,REFORM(control_qldaavg_10yriav(k,*)))
      FOR j=0,n_bins_10yrmeans-1 DO $
         IF TOTAL(where(control_qldaavg_10yrmeans_indices eq j)) ge 0 THEN $
            control_qldaavg_10yrmeans_binned(k,j)=N_ELEMENTS(where(control_qldaavg_10yrmeans_indices eq j))/FLOAT(n_10yr_runs)
      FOR j=0,n_bins_10yriav-1 DO $ 
         IF TOTAL(where(control_qldaavg_10yriav_indices eq j)) ge 0 THEN $
            control_qldaavg_10yriav_binned(k,j)=N_ELEMENTS(where(control_qldaavg_10yriav_indices eq j))/FLOAT(n_10yr_runs)
   ENDFOR
   stable_qldaavg_10yrmeans_indices=VALUE_LOCATE(bins_10yrmeans,stable_qldaavg_10yrmeans)
   stable_qldaavg_10yriav_indices=VALUE_LOCATE(bins_10yriav,stable_qldaavg_10yriav)
   FOR j=0,n_bins_10yrmeans-1 DO $
      IF TOTAL(where(stable_qldaavg_10yrmeans_indices eq j)) ge 0 THEN $
         stable_qldaavg_10yrmeans_binned(j)=N_ELEMENTS(where(stable_qldaavg_10yrmeans_indices eq j))/FLOAT(n_10yr_runs)
   FOR j=0,n_bins_10yriav-1 DO $
      IF TOTAL(where(stable_qldaavg_10yriav_indices eq j)) ge 0 THEN $
         stable_qldaavg_10yriav_binned(j)=N_ELEMENTS(where(stable_qldaavg_10yriav_indices eq j))/FLOAT(n_10yr_runs)

   control_qldaavg_30yrmeans_binned=fltarr(n_bins_30yrmeans)
   control_qldaavg_30yriav_binned=fltarr(n_bins_30yriav)
   control_qldaavg_30yrmeans_indices=VALUE_LOCATE(bins_30yrmeans,control_qldaavg_30yrmeans)
   control_qldaavg_30yriav_indices=VALUE_LOCATE(bins_30yriav,control_qldaavg_30yriav)
   FOR j=0,n_bins_30yrmeans-1 DO $
      IF TOTAL(where(control_qldaavg_30yrmeans_indices eq j)) ge 0 THEN $
         control_qldaavg_30yrmeans_binned(j)=N_ELEMENTS(where(control_qldaavg_30yrmeans_indices eq j))/FLOAT(n_30yr_runs)
   FOR j=0,n_bins_30yriav-1 DO $
      IF TOTAL(where(control_qldaavg_30yriav_indices eq j)) ge 0 THEN $
         control_qldaavg_30yriav_binned(j)=N_ELEMENTS(where(control_qldaavg_30yriav_indices eq j))/FLOAT(n_30yr_runs)

      
   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_distributions.'+season_name+'_'+season_type+'s.30year_means.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000,SPACE3=300
   CS,SCALE=26,NCOLS=N_ELEMENTS(runs_levels)+1,white=[7]
   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   LEVS,MANUAL=runs_levels
   CON,FIELD=stable_position_30years/FLOAT(n_30yr_runs),X=control_longitude,Y=control_latitude,$
       TITLE='Fraction of randomly sampled 30 year means greater than 30 year fixed 2xCO2 HiGEM integration - '+$
       season_name,CB_TITLE='Fraction (unitless, equal to p for > control precip and 1-p for < control precip in 2xCO2)',/BLOCK,/NOLINES
   AXES
   PSCLOSE,/NOVIEW

;   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_distributions.'+season_name+'_'+season_type+'s.10year_means.ps'
;   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
;          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
;   CS,SCALE=26,NCOLS=N_ELEMENTS(runs_10yr_levels)+1,white=[7]
;   MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
;   LEVS,MANUAL=runs_10yr_levels
;   CON,FIELD=stable_position_10years*2.,X=control_longitude,Y=control_latitude,$
;       TITLE='Frac random 10 year means from fixed 2xCO2 greater/less than 90% of random '+$
;       ' 10 year means from control - '+season_name,$
;       CB_TITLE='Fraction (unitless, positive for fraction drier, negative for fraction wetter)',$
;       /BLOCK,/NOLINES
;   AXES
;   PSCLOSE

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_distributions.'+season_name+'_'+$
          season_type+'s.10year_means_qldaavg.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=300,XOFFSET=800,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,SPACE3=300
   GSET,XMIN=MIN(bins_10yrmeans),XMAX=MAX(bins_10yrmeans),YMIN=0,YMAX=0.6,TITLE='For Queensland area-average using '+season_name+' means, '+$
        'all 10 year means from control and 2x CO2 and 30 year means from control'
   FOR j=0,control_nchunks-1 DO $
      GPLOT,X=bins_10yrmeans,Y=REFORM(control_qldaavg_10yrmeans_binned(j,*)),COL=FSC_COLOR('purple'),STYLE=REFORM(j+2)
   GPLOT,X=bins_10yrmeans,Y=stable_qldaavg_10yrmeans_binned,COL=FSC_COLOR('red'),STYLE=2
   GPLOT,X=bins_30yrmeans,Y=control_qldaavg_30yrmeans_binned,COL=FSC_COLOR('black'),STYLE=0
   GPLOT,X=[MEAN(stable_qldaavg(13:13+higem_stable_nyears-1)),MEAN(stable_qldaavg(13:13+higem_stable_nyears-1))],$
         Y=[0,0.6],STYLE=0,COL=FSC_COLOR('red')
   AXES,XSTEP=bins_10yrmeans(2)-bins_10yrmeans(1),YSTEP=0.05,YMINOR=0.025,NDECS=3,$
        XTITLE='Area-averaged total precipitation (mm)',YTITLE='Probability'
   labels=['10 year means from control years 1-50','10 year means from control years 51-100','10 year means from control years 101-150',$
           '10 year means from [2x fixed (30 y) + 2% trans (20 y)]',$
           '30 year means from control (x 40)','Mean of 2x fixed (30 years)']
   GLEGEND,labels=REVERSE(labels),COL=REVERSE([FSC_COLOR('purple'),FSC_COLOR('purple'),$
                                               FSC_COLOR('purple'),FSC_COLOR('red'),FSC_COLOR('black'),FSC_COLOR('red')]),$
           STYLE=REVERSE([2,3,4,2,0,0]),LEGPOS=9
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_distributions.'+season_name+'_'+$
          season_type+'s.10year_iav_qldaavg.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1800,SPACE2=300,XOFFSET=800,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,SPACE3=300
   GSET,XMIN=MIN(bins_10yriav),XMAX=MAX(bins_10yriav),YMIN=0,YMAX=0.7,TITLE='For Queensland area-average using '+season_name+' means, '+$
        'all 10 year running standard deviations from control and 2x CO2'
   FOR j=0,control_nchunks-1 DO $
      GPLOT,X=bins_10yriav,Y=REFORM(control_qldaavg_10yriav_binned(j,*)),COL=FSC_COLOR('purple'),STYLE=REFORM(j+2)
   GPLOT,X=bins_10yriav,Y=stable_qldaavg_10yriav_binned,COL=FSC_COLOR('red'),STYLE=2
   GPLOT,X=bins_30yriav,Y=control_qldaavg_30yriav_binned,COL=FSC_COLOR('black'),STYLE=0
   GPLOT,X=[STDDEV(stable_qldaavg(13:13+higem_stable_nyears-1)),STDDEV(stable_qldaavg(13:13+higem_stable_nyears-1))],$
         Y=[0,0.7],STYLE=0,COL=FSC_COLOR('red')
   AXES,XSTEP=bins_10yriav(2)-bins_10yriav(1),YSTEP=0.05,YMINOR=0.025,NDECS=3,$
        XTITLE='Standard deviation in area-averaged precipitation (mm)',YTITLE='Probability'
   labels=['10 year stddev from control years 1-50','10 year stddev from control years 51-100','10 year stddev from control years 101-150',$
           '10 year stddev from [2x fixed (30 y) + 2% trans (20 y)]',$
           '30 year stddev from control (x 40)','Stddev of 2x fixed (30 years)']
   GLEGEND,labels=REVERSE(labels),COL=REVERSE([FSC_COLOR('purple'),FSC_COLOR('purple'),FSC_COLOR('purple'),FSC_COLOR('red'),$
                                              FSC_COLOR('black'),FSC_COLOR('red')]),$
           STYLE=REVERSE([2,3,4,2,0,0]),LEGPOS=9
   PSCLOSE,/NOVIEW
   

ENDFOR
STOP

END
