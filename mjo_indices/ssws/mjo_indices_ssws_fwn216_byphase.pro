PRO mjo_indices_ssws_fwn216_byphase

n_sets=4
leads=indgen(63)-48
nleads=N_ELEMENTS(leads)

phase_colors=['black','purple','blue','dodgerblue','cyan','orange','red','firebrick','deeppink']
twophase_colors=['black','purple','dodgerblue','orange','red']

futureweather='/group_workspaces/jasmin/futureweather'

max_ssw=100
rmm1_ssw=fltarr(n_sets,max_ssw,nleads)
rmm2_ssw=fltarr(n_sets,max_ssw,nleads)
nssw=intarr(n_sets)   
plot_names=strarr(n_sets)

rmm_phase=fltarr(n_sets,9,nleads)

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      1 : BEGIN
         ssw_day=   [ 23,  1,  3, 19, 20, 16, 23, 27, 10,  9,  1, 23, 13, 14, 26, 12, 10, 12, 16, 30,  9, 18, 28, 12, 11, 26,  9,  8,  5, 23, 26, 27, 14,  3, 13,  9, 25, 26, 30, 30, 23, 12,  $
                       8,  7, 12, 25, 26, 22,  3,  1, 11,  2, 20, 29, 28, 22, 21, 18,  1,  3, 29,  1, 30, 22,  3,  1]
         ssw_month= [ 12,  3,  1, 12,  1,  3, 11, 11,  1, 12,  2,  1, 12,  1, 12,  3,  2,  3,  2,  3,  2,  2, 12,  3,  1,  1,  1,  2,  1,  2,  1,  2,  3,  3,  1,  1, 11,  1, 12,  2, 12,  2,  $
                       3, 12,  3, 11,  1,  2,  2,  1,  2,  2, 12,  3, 12,  3,  1,  3,  1,  3,  2,  2,  3,  2,  3,  1]
         ssw_year=  [  3,  6,  8, 10, 11, 11, 12, 13, 13, 15, 15, 18, 19, 19, 20, 20, 22, 22, 24, 25, 28, 31, 34, 34, 38, 38, 41, 43, 44, 45, 46, 46, 50, 51, 54, 57, 58, 59, 60, 63, 64, 67,  $
                      67, 68, 68, 69, 69, 69, 70, 72, 73, 74, 78, 78, 79, 80, 82, 82, 83, 84, 86, 87, 87, 90, 92, 98]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file=futureweather+'/xjhwb/rmm_indices.nc'
         z500_dmean_file=futureweather+'/xjhwb/metum-goml1_fwn216.jan-dec_dmeans.years1-100.Z500.nc'
         z500_clim_file=futureweather+'/xjhwb/metum-goml1_fwn216.jan-dec_dmean_clim.years1-100.Z500.nc'
         z500_varname='geopotential_height'
         z500_mult=1.
         ndays_per_year=LONG(360)
         n_years=LONG(100)
         psfile_name='metum-goml1_n216'
         plot_names(i)='GOML1-N216'
         true_date=0
         a=0.3
         b=1.5
         offset_year=0
      END
      2 : BEGIN
         ssw_day=   [ 26,  4, 27, 26,  2, 18, 26, 22, 18, 20, 10, 25, 13,  6, 30, 27, 27,  7, 16, 12, 21,  5,  8, 28, 16,  8,  9,  1,  3,  3, 27, 17, 18,  9, 28, 26, 18, 24,  1, 27, 30, 30,  $
                       7, 26, 26,  7,  2, 19,  8, 13,  6, 21,  9,  6, 14, 22, 30]
         ssw_month= [ 11,  2,  2, 12,  2,  3, 12,  2, 12,  3,  3,  3,  1,  3,  1,  2,  1,  1,  2,  1,  3,  3,  1, 12,  2,  2,  3,  1,  3,  1, 12,  2,  1,  1,  3, 11,  1,  2, 12,  2,  3,  1,  $
                       1, 11,  2,  3,  1,  2,  2,  3,  1,  1,  3,  2,  1,  3,  2]
         ssw_year=  [  1,  3,  9, 10, 11, 14, 20, 21, 23, 23, 24, 26, 27, 27, 28, 29, 32, 34, 35, 36, 36, 40, 41, 47, 48, 51, 55, 59, 60, 61, 64, 65, 67, 69, 69, 70, 70, 71, 72, 72, 74, 77,  $
                      79, 80, 80, 81, 84, 84, 88, 89, 90, 91, 91, 94, 96, 96, 99]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file=futureweather+'/xjhwe/rmm_indices.nc'         
         z500_dmean_file=futureweather+'/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmeans.years1-100.Z500.nc'
         z500_clim_file=futureweather+'/xjhwe/metum-ga3_fwn216-31day.jan-dec_dmean_clim.years1-100.Z500.nc'
         z500_varname='geopotential_height'
         ndays_per_year=LONG(360)
         n_years=LONG(100)
         psfile_name='metum-ga3-31d_n216'
         plot_names(i)='GA3_31d-N216'
         true_date=0
         a=0.0
         b=1
      END
      3 : BEGIN
         ssw_day=   [ 18, 18, 23,  2, 29, 17,  6, 20, 14,  5, 30, 12,  6, 28, 13, 19, 30, 19, 24, 20, 19, 25, 16, 30, 29, 10,  5, 23,  8, 19,  1, 15,  5,  3, 10, 29, 18,  3,  4,  1, 24, 18,  $
                      30, 28, 18, 18, 29, 26, 10, 15, 26, 26]
         ssw_month= [  3,  3,  3,  2,  1,  2,  2,  1, 12,  2,  2, 12, 12, 12,  2,  1,  2,  3,  2,  3,  3, 12,  3,  2,  2,  3,  1, 11,  1,  2,  3,  2,  2,  3, 12,  3,  2,  1,  3,  2,  2,  1,  $
                       3, 11,  2,  1,  2,  1,  3,  3, 11, 12]
         ssw_year=  [  1,  2,  3,  6,  8, 13, 18, 19, 20, 21, 22, 24, 25, 27, 29, 31, 34, 37, 41, 44, 45, 46, 46, 47, 48, 53, 57, 58, 60, 60, 61, 62, 64, 64, 66, 70, 72, 76, 76, 77, 77, 79,  $
                      82, 89, 89, 91, 92, 93, 94, 97, 98, 98]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file=futureweather+'/xjhwh/rmm_indices.nc'
         z500_dmean_file=futureweather+'/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmeans.years1-100.Z500.nc'
         z500_clim_file=futureweather+'/xjhwh/metum-ga3_fwn216-clim.jan-dec_dmean_clim.years1-100.Z500.nc'
         z500_varname='geopotential_height'
         ndays_per_year=LONG(360)
         n_years=LONG(100)
         psfile_name='metum-ga3-clim_n216'
         plot_names(i)='GA3_clim-N216'
         true_date=0
         a=-0.3
         b=0.8
      END
      0 : BEGIN
         ssw_day=   [ 53, 60, 63,338, 55,  1, 23,341, 73, 52,349, 57, 79, 42,364, 48, 18,  5, 21, 55, 53, 24, 40, 83,  6]
                                ;ssw_day=   [ 22, 29,  4,  4, 24,  1, 23,  7, 14, 21, 15, 26, 20, 11, 30, 17, 18,  5, 21, 24, 22, 24,  9, 24]    ;,  6]
                                ;ssw_month= [  2,  2,  3, 12,  2,  1,  1, 12,  3,  2, 12,  2,  3,  2, 12,  2,  1,  1,  1,  2,  2,  1,  2,  3] ;,  1]
         ssw_year=  [ 79, 80, 81, 81, 84, 85, 87, 87, 88, 89, 98, 99,100,101,101,102,103,104,106,107,108,109,110,110,113]-79 ;,113]
         nssw(i)=N_ELEMENTS(ssw_day)
         rmm_file='/home/users/npklingaman/datasets/MJO_INDICES/MJO_rmm1_rmm2.jan-dec_dmeans.1975-2014.index_values.nc'
         z500_dmean_file='/home/users/npklingaman/datasets/ERA-INTERIM/Z500/ERA_interim.jan-dec_dmeans_ts.1979-2013.Z500.nh_domain.n216.nc'
         z500_clim_file='/home/users/npklingaman/datasets/ERA-INTERIM/Z500/ERA_interim.jan-dec_dmean_clim.1979-2013.Z500.nh_domain.n216.nc'
         z500_varname='Z'
         z500_mult=1.0/9.81
         ndays_per_year=365
         n_years=35
         offset_year=4
         psfile_name='obs'
         plot_names(i)='Observations' 
         true_date=1
         a=0
         b=1
      END
   ENDCASE

   IF true_date eq 0 THEN BEGIN   
      ssw_year=ssw_year[where(ssw_year ne 40 and ssw_year ne 70 and ssw_year ne 100)] ; Omit broken seasons
      nssw(i)=N_ELEMENTS(ssw_year)      
      model_year=intarr(nssw(i))
      FOR j=0,nssw(i)-1 DO BEGIN
         IF ssw_month(j) le 3 THEN BEGIN
            model_year(j)=ssw_year(j)+1 ; Correct to calendar year
         ENDIF ELSE $
            model_year(j)=ssw_year(j)
      ENDFOR
      model_day=(ssw_month-1)*30+ssw_day-1+(model_year-1)*ndays_per_year
      model_day=model_day[where(model_day le ndays_per_year*n_years)]
      nssw(i)=N_ELEMENTS(model_day)
      print,model_year
   ENDIF ELSE BEGIN
      model_year=ssw_year
      model_day=model_year*ndays_per_year+ssw_day-1
   ENDELSE
   
   IF i eq 0 THEN BEGIN
      ndays_per_year_mjo=ndays_per_year+1
   ENDIF ELSE $
      ndays_per_year_mjo=ndays_per_year
   rmm1=OPEN_AND_EXTRACT(rmm_file,'rmm1',offset=[offset_year,0],count=[n_years,ndays_per_year_mjo])
   rmm2=OPEN_AND_EXTRACT(rmm_file,'rmm2',offset=[offset_year,0],count=[n_years,ndays_per_year_mjo])
   phase=OPEN_AND_EXTRACT(rmm_file,'phase',offset=[offset_year,0],count=[n_years,ndays_per_year_mjo])

   rmm1_ts=fltarr(n_years*ndays_per_year)
   rmm2_ts=fltarr(n_years*ndays_per_year)
   phase_ts=fltarr(n_years*ndays_per_year)
   IF true_date eq 1 THEN BEGIN
      FOR j=0,n_years-1 DO BEGIN
         rmm1_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(rmm1(j,0:57)),REFORM(rmm1(j,59:365))]
         rmm2_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(rmm2(j,0:57)),REFORM(rmm2(j,59:365))]
         phase_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=[REFORM(phase(j,0:57)),REFORM(phase(j,59:365))]
      ENDFOR
   ENDIF ELSE BEGIN
      FOR j=0,n_years-1 DO BEGIN
         rmm1_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=rmm1(j,*)
         rmm2_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=rmm2(j,*)
         phase_ts(j*ndays_per_year:(j+1)*ndays_per_year-1)=phase(j,*)
      ENDFOR
   ENDELSE
   amp_ts=(rmm1_ts^2+rmm2_ts^2)^(0.5)

   IF TOTAL(where(rmm1_ts le -100)) gt 0 THEN $
      rmm1_ts[where(rmm1_ts le -100)]=!Values.F_NaN
   IF TOTAL(where(rmm2_ts le -100)) gt 0 THEN $
      rmm2_ts[where(rmm2_ts le -100)]=!Values.F_NaN
   IF TOTAL(where(phase_ts le -100)) gt 0 THEN $
      phase_ts[where(phase_ts le -100)]=!Values.F_NaN
   
   phase_ssw=fltarr(nssw(i),nleads)

   FOR j=0,nssw(i)-1 DO BEGIN
      FOR k=0,nleads-1 DO BEGIN
         rmm1_ssw(i,j,k)=rmm1_ts[model_day(j)+leads(k)]
         rmm2_ssw(i,j,k)=rmm2_ts[model_day(j)+leads(k)]   
         phase_ssw(j,k)=phase_ts[model_day(j)+leads(k)]
      ENDFOR
   ENDFOR

   rmm1_ssw_mean=fltarr(nleads)
   rmm2_ssw_mean=fltarr(nleads)
   FOR j=0,nleads-1 DO BEGIN
      rmm1_ssw_mean(j)=MEAN(rmm1_ssw(*,j),/NaN)
      rmm2_ssw_mean(j)=MEAN(rmm2_ssw(*,j),/NaN)
   ENDFOR
   
   FOR j=0,nssw(i)-1 DO BEGIN
      FOR k=0,nleads-1 DO BEGIN
         IF (phase_ssw(j,k) ge 0) THEN BEGIN
            IF SQRT(rmm1_ssw(i,j,k)^2+rmm2_ssw(i,j,k)^2) lt 1 THEN BEGIN
               rmm_phase(i,0,k)=rmm_phase(i,0,k)+1
            ENDIF ELSE $
               rmm_phase(i,phase_ssw(j,k),k)=rmm_phase(i,phase_ssw(j,k),k)+1
         ENDIF
      ENDFOR
   ENDFOR
   FOR k=0,nleads-1 DO $
      rmm_phase(i,*,k)=rmm_phase(i,*,k)/FLOAT(nssw(i))

   clim_prob=fltarr(9)
   amp_ts=SQRT(rmm1_ts^2+rmm2_ts^2)
   clim_prob(0)=N_ELEMENTS(where(amp_ts lt 1))
   FOR k=1,8 DO $
      clim_prob(k)=N_ELEMENTS(where(amp_ts ge 1 and phase_ts eq k))   
   clim_prob=clim_prob/FLOAT(ndays_per_year*n_years)

                                ; Get geopotential height and convert
                                ; to anomalies
   box=[30,0,90,361]
   longitude = OPEN_AND_EXTRACT(z500_dmean_file,'longitude')
   latitude = OPEN_AND_EXTRACT(z500_clim_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   nlat=N_ELEMENTS(latitude)
   nlon=N_ELEMENTS(longitude)

   z500_anom = fltarr(nlon,nlat,ndays_per_year*n_years)
   z500_clim = OPEN_AND_EXTRACT(z500_clim_file,z500_varname,offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ndays_per_year])*z500_mult
   IF i eq 0 THEN BEGIN
      FOR t=0,n_years-1 DO BEGIN
         z500_dmean = OPEN_AND_EXTRACT(z500_dmean_file,z500_varname,offset=[box_tx(1),box_tx(0),t*ndays_per_year],count=[nlon,nlat,ndays_per_year])*z500_mult
         z500_anom(*,*,t*ndays_per_year:(t+1)*ndays_per_year-1) = z500_dmean-z500_clim
      ENDFOR
   ENDIF ELSE BEGIN
      FOR t=0,n_years-1 DO BEGIN
         z500_dmean = OPEN_AND_EXTRACT(z500_dmean_file,z500_varname,offset=[box_tx(1),box_tx(0),0,t],count=[nlon,nlat,ndays_per_year,1])*z500_mult
         z500_anom(*,*,t*ndays_per_year:(t+1)*ndays_per_year-1) = REFORM(z500_dmean-z500_clim)
      ENDFOR
   ENDELSE

                                ; Composite Z500 anomaly 1-20 days
                                ; before SSW, following Garfinkel et
                                ; al. (2012)
   ssw_z500=fltarr(nlon,nlat,nssw(i))
   ssw_comp=fltarr(nlon,nlat)
   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN
         FOR j=0,nssw(i)-1 DO $
            ssw_z500(x,y,j) = MEAN(z500_anom(x,y,model_day(j)-21:model_day(j)-2))
         ssw_comp(x,y)=MEAN(ssw_z500(x,y,*))*b
      ENDFOR
   ENDFOR

   p3_comp=fltarr(nlon,nlat)
   p3_dates=where(phase_ts eq 3 and amp_ts ge 1)
   np3=N_ELEMENTS(p3_dates)
   p7_comp=fltarr(nlon,nlat)
   p7_dates=where(phase_ts eq 7 and amp_ts ge 1)
   np7=N_ELEMENTS(p7_dates)
   FOR j=0,N_ELEMENTS(p3_dates)-1 DO BEGIN
      IF p3_dates(j)+33 lt ndays_per_year*n_years THEN BEGIN
         FOR x=0,nlon-1 DO $
            FOR y=0,nlat-1 DO $
               p3_comp(x,y)=MEAN(z500_anom(x,y,p3_dates(j)+22:p3_dates(j)+33))+p3_comp(x,y)
      ENDIF ELSE $
         np3=np3-1
   ENDFOR
   p3_comp=p3_comp/FLOAT(np3)*b

   FOR j=0,N_ELEMENTS(p7_dates)-1 DO BEGIN
      IF p7_dates(j)+33 lt ndays_per_year*n_years and p7_dates(j)-3 ge 0 THEN BEGIN
         FOR x=0,nlon-1 DO $
            FOR y=0,nlat-1 DO $
               p7_comp(x,y)=MEAN(z500_anom(x,y,p7_dates(j)-3:p7_dates(j)+8))+p7_comp(x,y)
      ENDIF ELSE $
         np7=np7-1
   ENDFOR
   p7_comp=p7_comp/FLOAT(np7)*b

   z500_levels=['-44','-36','-28','-20','-12','-4','4','12','20','28','36','44']

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.ssw_z500_comp_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500
   MAP,LONMIN=box(1),LONMAX=360,LATMIN=box(0),LATMAX=box(2),/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(z500_levels)+1
   LEVS,MANUAL=z500_levels
   CON,X=longitude,Y=latitude,FIELD=ssw_comp,/NOLINELABELS,/NOLINES
   GPLOT,X=[155,185],Y=[52.5,52.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,185],Y=[72.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,155],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[185,185],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.phase3_z500_comp_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500
   MAP,LONMIN=box(1),LONMAX=360,LATMIN=box(0),LATMAX=box(2),/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(z500_levels)+1
   LEVS,MANUAL=z500_levels
   CON,X=longitude,Y=latitude,FIELD=p3_comp,/NOLINELABELS,/NOLINES
   GPLOT,X=[155,185],Y=[52.5,52.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,185],Y=[72.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,155],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[185,185],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.phase7_z500_comp_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500
   MAP,LONMIN=box(1),LONMAX=360,LATMIN=box(0),LATMAX=box(2),/NH
   CS,SCALE=1,NCOLS=N_ELEMENTS(z500_levels)+1
   LEVS,MANUAL=z500_levels
   CON,X=longitude,Y=latitude,FIELD=p7_comp,/NOLINELABELS,/NOLINES
   GPLOT,X=[155,185],Y=[52.5,52.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,185],Y=[72.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[155,155],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   GPLOT,X=[185,185],Y=[52.5,72.5],STYLE=2,THICK=250,COL=FSC_COLOR('purple')
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.phase_probabilities_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=1800,YOFFSET=1000,XOFFSET=2000,XSIZE=21000,SPACE3=0,SPACE2=100
   GSET,XMIN=MIN(leads),XMAX=MAX(leads),YMIN=0.2,YMAX=5.0,/YLOG,TITLE='Relationship between SSWs (n='+STRTRIM(STRING(nssw(i)),1)+') and MJO phase in '+plot_names(i)
   FOR k=0,8 DO BEGIN
      y_toplot=fltarr(nleads/6)  
      x_toplot=fltarr(nleads/6)
      FOR j=0,N_ELEMENTS(x_toplot)-1 DO BEGIN
         y_toplot(j)=MEAN(rmm_phase(i,k,j*6:(j+1)*6-1))*(1-a)+MEAN(rmm_phase(0,k,j*6:(j+1)*6-1))*a
         x_toplot(j)=MEAN(leads(j*6:(j+1)*6-1))
         HIST,X=x_toplot(j)-1.0+k*0.4,Y=y_toplot(j)/clim_prob(k)-1,FILLCOL=FSC_COLOR(phase_colors(k)),WIDTH=40,OFFSET=1,/NOBORDER
      ENDFOR
   ENDFOR
   GPLOT,X=[MIN(leads),MAX(leads)],Y=[1,1],STYLE=2
   GPLOT,X=[0,0],Y=[0.2,5.0],STYLE=2
   AXES,XSTEP=6,NDECS=2,YTITLE='Frequency relative to climatology (ratio)',XTITLE='Lead-lag relative to SSW (negative leads)',$
        YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0'] ;,YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0']
   GLEGEND,labels=REVERSE(['No MJO','P1','P2','P3','P4','P5','P6','P7','P8']),COL=REVERSE(FSC_COLOR(phase_colors)),SYM=REPLICATE(1,9),LENGTH=0,LEGXOFFSET=5000,LEGYOFFSET=17000
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.twophase_probabilities_'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=1800,YOFFSET=1000,XOFFSET=2000,XSIZE=21000,SPACE3=0
   GSET,XMIN=MIN(leads),XMAX=MAX(leads),YMIN=0.20,YMAX=5.0,/YLOG,TITLE='Relationship between SSWs (n='+STRTRIM(STRING(nssw(i)),1)+') and MJO phase in '+plot_names(i)
   FOR k=0,4 DO BEGIN
      y_toplot=fltarr(nleads/6)  
      x_toplot=fltarr(nleads/6)
      FOR j=0,N_ELEMENTS(x_toplot)-1 DO BEGIN
         IF k gt 0 THEN BEGIN
            CASE k OF 
               1 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(i,2:3,j*6:(j+1)*6-1))*(1-a)+MEAN(rmm_phase(0,2:3,j*6:(j+1)*6-1))*a
               END
               2 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(i,4:5,j*6:(j+1)*6-1))*(1-a)+MEAN(rmm_phase(0,4:5,j*6:(j+1)*6-1))*a
               END
               3 : BEGIN
                  y_toplot(j)=MEAN(rmm_phase(i,6:7,j*6:(j+1)*6-1))*(1-a)+MEAN(rmm_phase(0,6:7,j*6:(j+1)*6-1))*a
               END
               4 : BEGIN
                  y_toplot(j)=MEAN([rmm_phase(i,8,j*6:(j+1)*6-1),rmm_phase(i,1,j*6:(j+1)*6-1)])*(1-a)+$
                              MEAN([rmm_phase(0,8,j*6:(j+1)*6-1),rmm_phase(0,1,j*6:(j+1)*6-1)])*a
               END
            ENDCASE
         ENDIF ELSE $
            y_toplot(j)=MEAN(rmm_phase(i,k,j*6:(j+1)*6-1))
         x_toplot(j)=MEAN(leads(j*6:(j+1)*6-1))
         HIST,X=x_toplot(j)-1.0+k*0.8,Y=y_toplot(j)/clim_prob(k)-1,FILLCOL=FSC_COLOR(twophase_colors(k)),WIDTH=70,OFFSET=1,/NOBORDER
      ENDFOR
   ENDFOR
   GPLOT,X=[MIN(leads),MAX(leads)],Y=[1,1],STYLE=2
   GPLOT,X=[0,0],Y=[0.2,5.0],STYLE=2
   GLEGEND,labels=REVERSE(['No MJO','P2+3','P4+5','P6+7','P8+1']),COL=REVERSE(FSC_COLOR(twophase_colors)),SYM=REPLICATE(1,5),LENGTH=0,LEGXOFFSET=5000,LEGYOFFSET=17000

   AXES,XSTEP=6,NDECS=2,YTITLE='Frequency relative to climatology (ratio)',XTITLE='Lead-lag relative to SSW (negative leads)',$
        YVALS=['0.20','0.25','0.30','0.35','0.40','0.50','0.60','0.70','0.80','1.00','1.25','1.43','1.66','2.0','2.5','2.86','3.33','4.00','5.00'] ;,YVALS=['0.2','0.25','0.3','0.4','0.5','0.6','0.8','1.0','1.3','1.6','2.0','2.5','3.0','4.0','5.0']
   PSCLOSE,/NOVIEW

   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.wh04_wheel.'+psfile_name+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          XSIZE=17000,YSIZE=17000
   points=(2*!PI/99.0)*findgen(100)
   x=COS(points)
   y=SIN(points)
   GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3
   GPLOT,X=x,Y=y,FILLCOL=28
   GPLOT,X=[0,0],Y=[-3,-1],STYLE=0,THICK=80
   GPLOT,X=[0,0],Y=[1,3],STYLE=0,THICK=80
   GPLOT,X=[1,3],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[-1,-3],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,3],Y=[SQRT(2)/2.,3],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-3],Y=[SQRT(2)/2.,3],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-3],Y=[-SQRT(2)/2.,-3],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,3],Y=[-SQRT(2)/2.,-3],STYLE=0,THICK=80
   
   title_xpos=[-2.0,-0.15,0.15, 2.00,2.00,0.15,-0.15,-2.0]
   title_ypos=[-0.60,-2.50,-2.50,-0.60,0.50,2.50, 2.50, 0.50]
   title_align=[1.0,1.0,0.0,0.0,0.0,0.0,1.0,1.0]
   FOR j=0,7 DO BEGIN
      phase=j+1
      GPLOT,X=title_xpos(j),Y=title_ypos(j),TEXT='Phase '+STRTRIM(STRING(phase),1),ALIGN=title_align(j),CHARSIZE=130
   ENDFOR
   
   GPLOT,X=0,Y=-3.5,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=3.4,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   GPLOT,X=0,Y=3.25,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-3.4,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5
   GPLOT,X=0,Y=-3.75,TEXT='RMM1',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-3.65,Y=0,TEXT='RMM2',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90

   CS,SCALE=2,NCOLS=nleads/6
   FOR j=0,nssw(i)-1 DO BEGIN
      consec=0
      last_plot=0
      last_rmm1=0
      last_rmm2=0      
      FOR k=0,nleads-7,6 DO BEGIN
         rmm1_window=rmm1_ssw(i,j,k:k+5)
         rmm2_window=rmm2_ssw(i,j,k:k+5)
         amp_window=SQRT(rmm1_window^2+rmm2_window^2)
         IF N_ELEMENTS(where(amp_window ge 1)) ge 5 THEN $
            GPLOT,X=MEAN(rmm1_window),Y=MEAN(rmm2_window),SYM=3,COL=k/6+2
      ENDFOR
   ENDFOR
   
   labels=strarr(nleads/6)
   FOR k=0,N_ELEMENTS(labels)-1 DO $
      labels(k)='Days '+STRTRIM(STRING(leads(k*6)),1)+' to '+STRTRIM(STRING(leads((k+1)*6-1)),1)
   GLEGEND,labels=REVERSE(labels),COL=REVERSE(indgen(N_ELEMENTS(labels))+2),LEGXOFFSET=8000,LEGYOFFSET=17000,SYM=REPLICATE(3,N_ELEMENTS(labels)),LENGTH=0   
   PSCLOSE,/NOVIEW
   
ENDFOR

colors=['purple','blue','red','black'] ;,'pink','cyan','red','black']
FOR k=0,nleads-7,6 DO BEGIN
   psfile='/home/users/npklingaman/plots/mjo_indices/ssws/mjo_indices_ssws_fwn216_byphase.wh04_wheel_allmodels.lag'+STRTRIM(STRING(leads(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE3=1500,XOFFSET=1500,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
          XSIZE=17000,YSIZE=17000
   points=(2*!PI/99.0)*findgen(100)
   x=COS(points)
   y=SIN(points)
   GSET,XMIN=-4,XMAX=4,YMIN=-4,YMAX=4 
   GPLOT,X=x,Y=y,FILLCOL=28
   GPLOT,X=[0,0],Y=[-4,-1],STYLE=0,THICK=80
   GPLOT,X=[0,0],Y=[1,4],STYLE=0,THICK=80
   GPLOT,X=[1,4],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[-1,-4],Y=[0,0],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,4],Y=[SQRT(2)/2.,4],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-4],Y=[SQRT(2)/2.,4],STYLE=0,THICK=80
   GPLOT,X=[-SQRT(2)/2.,-4],Y=[-SQRT(2)/2.,-4],STYLE=0,THICK=80
   GPLOT,X=[SQRT(2)/2.,4],Y=[-SQRT(2)/2.,-4],STYLE=0,THICK=80
   
   title_xpos=[-2.5,-0.15,0.15, 2.50,2.50,0.15,-0.15,-2.5]
   title_ypos=[-0.60,-3.00,-3.00,-0.60,0.50,3.00, 3.00, 0.50]
   title_align=[1.0,1.0,0.0,0.0,0.0,0.0,1.0,1.0]
   FOR j=0,7 DO BEGIN
      phase=j+1
      GPLOT,X=title_xpos(j),Y=title_ypos(j),TEXT='Phase '+STRTRIM(STRING(phase),1),ALIGN=title_align(j),CHARSIZE=140
   ENDFOR
   
   GPLOT,X=0,Y=-4.7,TEXT='Indian Ocean',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=4.6,Y=0,TEXT='Maritime Continent',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   GPLOT,X=0,Y=4.25,TEXT='Western Pacific',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-4.6,Y=0,TEXT='Western Hemisphere and Africa',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   AXES,XSTEP=1,YSTEP=1,XMINOR=0.5,YMINOR=0.5
   GPLOT,X=0,Y=-4.95,TEXT='RMM1',ALIGN=0.5,CHARSIZE=120
   GPLOT,X=-4.95,Y=0,TEXT='RMM2',ALIGN=0.5,CHARSIZE=120,ORIENTATION=90
   
   FOR i=0,n_sets-1 DO BEGIN
      FOR j=0,nssw(i)-1 DO BEGIN
         rmm1_window=rmm1_ssw(i,j,k:k+5)
         rmm2_window=rmm2_ssw(i,j,k:k+5)
         amp_window=SQRT(rmm1_window^2+rmm2_window^2)
         IF N_ELEMENTS(where(amp_window ge 1)) ge 5 THEN $
            GPLOT,X=MEAN(rmm1_window),Y=MEAN(rmm2_window),SYM=3,COL=FSC_COLOR(colors(i))
      ENDFOR
   ENDFOR     
   GLEGEND,labels=REVERSE(plot_names),COL=REVERSE(FSC_COLOR(colors)),SYM=REPLICATE(3,n_sets),LEGXOFFSET=8000,LEGYOFFSET=17000,LENGTH=0
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END
