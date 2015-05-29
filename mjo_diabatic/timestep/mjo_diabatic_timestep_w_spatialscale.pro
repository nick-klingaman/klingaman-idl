PRO mjo_diabatic_timestep_w_spatialscale

n_models=7
mjodiab_2day='/home/ss901165/um_output6/mjodiab_2day'

nstarts=42
box=[-10,60,10,180]

; For spatial correlations
region_xsize=12000
region_ysize=2000
box_size=1500 ; in km
max_box_npts_side=25
region_corr=fltarr(n_models,max_box_npts_side)
domain_size=7 ; in gridpoints

; For temporal correlations
time_size=3 ; in hours
max_tsteps=25
temporal_corr=fltarr(n_models,max_tsteps)
all_nsteps=intarr(n_models)

; For radius vs. lag
mylevs_radvlag=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
nlags=7

all_dx=intarr(n_models)
all_dt=fltarr(n_models)
all_colors=strarr(n_models)
all_syms=intarr(n_models)
all_npts_side=intarr(n_models)
all_psfile_titles=strarr(n_models)

FOR i=0,n_models-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile=mjodiab_2day+'/metum/MetUM.wap_intrho_lev0-48.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=180
         tstep_length=720
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='MetUM_GA3.0'
         all_dx(i)=65
         all_dt(i)=1/5.
         all_colors(i)='orangered'
         all_syms(i)=6
      END
      1 : BEGIN
         infile=mjodiab_2day+'/mri/MRI-AGCM.wap_intrho_lev0-26.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=72
         tstep_length=1800
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='MRI-AGCM'
         all_dx(i)=125
         all_dt(i)=1/2.
         all_colors(i)='blue'
         all_syms(i)=1
      END
      2 : BEGIN
         infile=mjodiab_2day+'/giss/ModelE.wap_intrho_lev0-25.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=72
         tstep_length=1800
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='ModelE2'
         all_dx(i)=275
         all_dt(i)=1/2.
         all_colors(i)='violetred'
         all_syms(i)=2
      END
      3 : BEGIN
         infile=mjodiab_2day+'/cnrm/CNRM.wap_intrho_lev0-26.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=72
         tstep_length=1800
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='CNRM-AM'
         all_dx(i)=150
         all_dt(i)=1/2.
         all_colors(i)='brown'
         all_syms(i)=7
      END
      4 : BEGIN
         infile=mjodiab_2day+'/cancm4/CanCM4.wap_intrho_lev0-25.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=36
         tstep_length=3600
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='CanCM4'
         all_dx(i)=300
         all_dt(i)=1
         all_colors(i)='dodgerblue'
         all_syms(i)=8
      END      
      5 : BEGIN
         infile=mjodiab_2day+'/spcam/SPCAM3.0.wap_intrho_lev0-21.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=72
         tstep_length=1800
         multiplier=1.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='SPCAM3.0'
         all_dx(i)=300
         all_dt(i)=1/2.
         all_colors(i)='purple'
         all_syms(i)=3
      END      
      6 : BEGIN
         infile=mjodiab_2day+'/nasa/GEOS5_AGCM.wap_intrho_lev0-35.20091020-20100110.lead_12-48hrs.nc'
         tsteps_per_start=108
         tstep_length=1200
         multiplier=86400.
         lon_name='longitude'
         lat_name='latitude'
         psfile_title='GEOS5'
         all_dx(i)=75 ; in km
         all_dt(i)=1/3. ; in hours
         all_colors(i)='cyan'
         all_syms(i)=10
      END
   ENDCASE

   lon=OPEN_AND_EXTRACT(infile,lon_name)
   lat=OPEN_AND_EXTRACT(infile,lat_name)
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)

   wap = OPEN_AND_EXTRACT(infile,'wap_intrho',$
                          offset=[box_tx(1),box_tx(0),0],$
                          count=[nlon,nlat,tsteps_per_start*nstarts])*multiplier
   corr_plot=fltarr(domain_size/2+1,nlags)
   corr_coeffs=fltarr(domain_size/2+1,nlags)

   nregions=0
   ntime=tsteps_per_start*nstarts
   FOR x=0,nlon-domain_size,domain_size DO BEGIN
      central_x=x+domain_size/2
      FOR y=0,nlat-domain_size,domain_size DO BEGIN
         central_y=y+domain_size/2
         central_point_ts=REFORM(wap(central_x,central_y,*))      
         region_wap=wap(central_x-domain_size/2:$
                              central_x+domain_size/2,$
                              central_y-domain_size/2:$
                              central_y+domain_size/2,*)
         distance=fltarr(domain_size,domain_size)
         FOR s=0,domain_size-1 DO $
            FOR t=0,domain_size-1 DO $
               distance(s,t)=MAX([ABS(domain_size/2-s),ABS(domain_size/2-t)])
         FOR s=0,domain_size/2 DO BEGIN
            mean_wap=fltarr(ntime)
            FOR t=0,tsteps_per_start*nstarts-1 DO BEGIN
               temp_wap=REFORM(region_wap(*,*,t))
               mean_wap(t)=MEAN(temp_wap[where(distance eq s)])
            ENDFOR
            FOR t=0,nlags-1 DO $
               corr_coeffs(s,t)=corr_coeffs(s,t)+CORRELATE(central_point_ts(0:ntime-t-1),mean_wap(t:ntime-1))
         ENDFOR                  
         nregions=nregions+1
      ENDFOR
   ENDFOR
   corr_coeffs=corr_coeffs/FLOAT(nregions)


   psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_w_spatialscale.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.corr_coeffs_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=230,MARGIN=2000,/PORTRAIT,XOFFSET=1000,YOFFSET=5000,SPACE2=1000,$
          TCHARSIZE=90,SPACE3=200
   plot_title=psfile_title
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=-0.5,YMAX=nlags-0.5 ;,$
                                ;TITLE='Correlation with central point at t=0 - '+plot_title
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs_radvlag)+1
   LEVS,MANUAL=mylevs_radvlag
   ;FOR p=0,nlags-1 DO BEGIN
   ;   count=fltarr(domain_size/2+1)
   ;   FOR r=0,domain_size-1 DO BEGIN
   ;      FOR s=0,domain_size-1 DO BEGIN
   ;         ;distance=SQRT((r-domain_size/2)^2+(s-domain_size/2)^2)
   ;         distance=MAX([ABS(r-domain_size/2),ABS(s-domain_size/2)])
   ;         corr_plot(distance,p)=corr_coeffs(r,s,p)+corr_plot(distance,p)
   ;         count(distance)=count(distance)+1
   ;      ENDFOR
   ;   ENDFOR
   ;   corr_plot(*,p)=corr_plot(*,p)/FLOAT(count)
   ;ENDFOR
   CON,X=indgen(domain_size/2+1),Y=indgen(nlags),FIELD=corr_coeffs,/BLOCK,/NOLINES,/NOCOLBAR; ,CB_WIDTH=115,$
                                ;CB_TITLE='Mean correlation coefficient'
   black=FSC_COLOR('black',30)
   white=FSC_COLOR('white',31)
   FOR j=0,domain_size/2 DO BEGIN
      FOR k=0,nlags-1 DO BEGIN
         IF corr_coeffs(j,k) ge 0.6 THEN BEGIN
            color=31
         ENDIF ELSE $
            color=30
         GPLOT,X=j,Y=k,TEXT=STRMID(STRTRIM(STRING(corr_coeffs(j,k)),1),0,5),ALIGN=0.5,VALIGN=0.5,COL=color
      ENDFOR
   ENDFOR
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (timesteps)'
   PSCLOSE,/NOVIEW
   
   box_npts_side=box_size/all_dx(i)
   IF ODD(box_npts_side) ne 1 THEN $
      box_npts_side=box_npts_side-1
   IF box_npts_side/2 gt max_box_npts_side THEN BEGIN
      print,'ERROR: box_npts_side/2 > max_box_npts_side'
      STOP
   ENDIF   

   nregions=0
   FOR x=0,nlon-box_npts_side,box_npts_side DO BEGIN
      central_x=x+box_npts_side/2
      FOR y=0,nlat-box_npts_side,box_npts_side DO BEGIN
         central_y=y+box_npts_side/2
         central_point_ts=REFORM(wap(central_x,central_y,*))      
         region_wap=wap(central_x-box_npts_side/2:$
                              central_x+box_npts_side/2,$
                              central_y-box_npts_side/2:$
                              central_y+box_npts_side/2,*)
         distance=fltarr(box_npts_side,box_npts_side)
         FOR s=0,box_npts_side-1 DO $
            FOR t=0,box_npts_side-1 DO $
               distance(s,t)=MAX([ABS(box_npts_side/2-s),ABS(box_npts_side/2-t)])
         FOR s=0,box_npts_side/2 DO BEGIN
            mean_wap=fltarr(tsteps_per_start*nstarts)
            FOR t=0,tsteps_per_start*nstarts-1 DO BEGIN
               temp_wap=REFORM(region_wap(*,*,t))
               mean_wap(t)=MEAN(temp_wap[where(distance eq s)])
            ENDFOR
            region_corr(i,s)=region_corr(i,s)+CORRELATE(central_point_ts,mean_wap)
         ENDFOR                  
         nregions=nregions+1
      ENDFOR
   ENDFOR
   region_corr(i,*)=region_corr(i,*)/FLOAT(nregions)   
   all_npts_side(i)=box_npts_side
   all_psfile_titles(i)=psfile_title

   all_nsteps(i)=FLOOR(time_size/all_dt(i))
   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN
         temporal_corr(i,0:all_nsteps(i))=temporal_corr(i,0:all_nsteps(i))+$
                                          A_CORRELATE(REFORM(wap(x,y,*)),indgen(all_nsteps(i)+1))
      ENDFOR
   ENDFOR
   temporal_corr(i,0:all_nsteps(i,*))=temporal_corr(i,0:all_nsteps(i))/FLOAT(nlon*nlat)
   print,psfile_title,nlon,nlat,box_npts_side,nregions,all_nsteps(i)
ENDFOR


psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_w_spatialscale.many_distance_tstep_egu.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=box_size/2.,YMIN=-0.2,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_npts_side(i)/2+1)*all_dx(i),Y=REFORM(region_corr(i,0:all_npts_side(i)/2)),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=all_syms(i)
   ;GPLOT,X=(indgen(all_npts_side(i)/2)+1)*all_dx(i),Y=REFORM(region_corr(i,1:all_npts_side(i)/2))-0.03,$
   ;      TEXT=STRTRIM(STRING(indgen(all_npts_side(i)/2)+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,box_size/2],Y=[0,0],STYLE=1
GLEGEND,labels=all_psfile_titles,COL=FSC_COLOR(all_colors),length=0,LEGPOS=9,SYM=all_syms
AXES,XSTEP=100,XMINOR=50,XTITLE='Distance (km)',$
     YTITLE='Correlation of timestep W with central point (x=0)',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_w_spatialscale.many_time_tstep_egu.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=time_size*60.,YMIN=-0.2,YMAX=1.01
FOR i=0,n_models-1 DO BEGIN
   GPLOT,X=indgen(all_nsteps(i)+1)*all_dt(i)*60.,Y=REFORM(temporal_corr(i,0:all_nsteps(i))),STYLE=2,$
         COL=FSC_COLOR(all_colors(i)),THICK=200,SYM=all_syms(i)
   ;GPLOT,X=(indgen(all_nsteps(i))+1)*all_dt(i)*60.,Y=REFORM(temporal_corr(i,1:all_nsteps(i)))-0.03,$
   ;      TEXT=STRTRIM(STRING(indgen(all_nsteps(i))+1),1),CHARSIZE=80,COL=FSC_COLOR(all_colors(i))
ENDFOR
GPLOT,X=[0,time_size*60],Y=[0,0],STYLE=1
GLEGEND,labels=all_psfile_titles,COL=FSC_COLOR(all_colors),length=0,LEGPOS=11,SYM=all_syms
AXES,XSTEP=30,XMINOR=15,XTITLE='Time (minutes)',$
     YTITLE='Lag correlation of gridpoint timestep W',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE
         
STOP
END
