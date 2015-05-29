PRO mjo_diabatic_timestep_mcu_spatialscale_bymcu

mjodiab_2day='/home/ss901165/um_output6/mjodiab_2day'

nstarts=42
n_models=1
box=[-10,60,10,180]

mcu_thresholds=[0,0.03,0.05,0.5,0.8,1.0]
;mcu_thresholds=[0.5]
n_thresh=N_ELEMENTS(mcu_thresholds)
pct_valid=fltarr(n_thresh)
colors=['black','cyan','dodgerblue','blue','purple','violetred']

; For spatial correlations
region_xsize=12000
region_ysize=2000
box_size=1500 ; in km
max_box_npts_side=25
region_corr=fltarr(n_thresh,max_box_npts_side)
domain_size=11 ; in gridpoints

; For temporal correlations
time_size=3 ; in hours
max_tsteps=25
temporal_corr=fltarr(n_thresh,max_tsteps)
all_nsteps=intarr(n_models)

; For radius vs. lag
mylevs_radvlag=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']
lags=indgen(13)-6
nlags=N_ELEMENTS(lags)

all_dx=intarr(n_models)
all_dt=fltarr(n_models)
all_colors=strarr(n_models)
all_syms=intarr(n_models)
all_npts_side=intarr(n_models)
all_psfile_titles=strarr(n_models)

mcu_infile=mjodiab_2day+'/metum/MetUM.mcu_intrho_lev0-48.20091020-20100110.lead_12-48hrs.nc'
tsteps_per_start=180
tstep_length=720
multiplier=1.
lon_name='longitude'
lat_name='latitude'
psfile_title='MetUM_GA3.0'
all_dx(0)=65
all_dt(0)=1/5.
all_colors(0)='orangered'
all_syms(0)=6

lon=OPEN_AND_EXTRACT(mcu_infile,lon_name)
lat=OPEN_AND_EXTRACT(mcu_infile,lat_name)
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)

mcu = OPEN_AND_EXTRACT(mcu_infile,'mcu_intrho',$
                       offset=[box_tx(1),box_tx(0),0],$
                       count=[nlon,nlat,tsteps_per_start*nstarts])*multiplier

FOR p=0,n_thresh-1 DO BEGIN
   prob_pos=fltarr(domain_size/2+1,nlags)
   prob_thresh=fltarr(domain_size/2+1,nlags)
   nregions=0
   nvalid=0
   ntime=tsteps_per_start*nstarts
   FOR x=0,nlon-domain_size,domain_size DO BEGIN
      central_x=x+domain_size/2
      FOR y=0,nlat-domain_size,domain_size DO BEGIN
         central_y=y+domain_size/2
         central_point_mcu=REFORM(mcu(central_x,central_y,*))
         valid=where(central_point_mcu gt mcu_thresholds(p))
         IF TOTAL(where(valid gt MAX(lags) and valid lt ntime-MAX(lags))) ge 0 THEN $
            valid=valid[where(valid gt MAX(lags) and valid lt ntime-MAX(lags))]
         IF N_ELEMENTS(valid) gt 1 THEN BEGIN
            nvalid=N_ELEMENTS(valid)+nvalid
            region_mcu=mcu(central_x-domain_size/2:$
                           central_x+domain_size/2,$
                           central_y-domain_size/2:$
                           central_y+domain_size/2,*)
            distance=fltarr(domain_size,domain_size)
            FOR s=0,domain_size-1 DO $
               FOR t=0,domain_size-1 DO $
                  distance(s,t)=MAX([ABS(domain_size/2-s),ABS(domain_size/2-t)])
            FOR s=0,domain_size/2 DO BEGIN
               this_prob_pos=fltarr(ntime)
               this_prob_thresh=fltarr(ntime)
               FOR t=0,tsteps_per_start*nstarts-1 DO BEGIN
                  temp_mcu=REFORM(region_mcu(*,*,t))
                  dist_mcu=temp_mcu[where(distance eq s)]
                  IF TOTAL(where(dist_mcu gt 0)) ge 0 THEN BEGIN
                     this_prob_pos(t)=N_ELEMENTS(where(dist_mcu gt 0))/FLOAT(N_ELEMENTS(dist_mcu))
                  ENDIF ELSE $
                     this_prob_pos(t)=0
                  IF TOTAL(where(dist_mcu gt mcu_thresholds(p))) ge 0 THEN BEGIN
                     this_prob_thresh(t)=N_ELEMENTS(where(dist_mcu gt mcu_thresholds(p)))/FLOAT(N_ELEMENTS(dist_mcu))
                  ENDIF ELSE $
                     this_prob_thresh(t)=0
               ENDFOR
               FOR t=0,nlags-1 DO BEGIN
                  prob_pos(s,t)=prob_pos(s,t)+MEAN(this_prob_pos[valid+lags(t)])
                  prob_thresh(s,t)=prob_thresh(s,t)+MEAN(this_prob_thresh[valid+lags(t)])
               ENDFOR
            ENDFOR                  
            nregions=nregions+1
         ENDIF
      ENDFOR
   ENDFOR

   prob_pos=prob_pos/FLOAT(nregions)
   prob_thresh=prob_thresh/FLOAT(nregions)
   pct_valid(p)=nvalid/(FLOAT(nregions)*ntime)*100.
   
   psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_mcu_spatialscale_bymcu.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.mcu_thresh_'+$
          STRMID(STRTRIM(STRING(mcu_thresholds(p)),1),0,4)+'.prob_pos_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=170,MARGIN=2500,/PORTRAIT,XOFFSET=1000,YOFFSET=1000,SPACE2=300,$
          TCHARSIZE=100,SPACE3=400
   plot_title=psfile_title
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=MIN(lags)-0.5,YMAX=MAX(lags)+0.5
       
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs_radvlag)+1
   LEVS,MANUAL=mylevs_radvlag
   CON,X=indgen(domain_size/2+1),Y=lags,FIELD=prob_pos,/BLOCK,/NOLINES,/NOCOLBAR,$
       TITLE='Prob of M(x,t) > 0.0 when M(0,0) > '+STRMID(STRTRIM(STRING(mcu_thresholds(p)),1),0,4)+$
       ' ('+STRMID(STRTRIM(STRING(pct_valid(p)),1),0,4)+'% of timesteps)'
   
   black=FSC_COLOR('black',30)
   white=FSC_COLOR('white',31)
   FOR j=0,domain_size/2 DO BEGIN
      FOR k=0,nlags-1 DO BEGIN
         IF prob_pos(j,k) ge 0.6 THEN BEGIN
            color=31
         ENDIF ELSE $
            color=30
         GPLOT,X=j,Y=lags(k),TEXT=STRMID(STRTRIM(STRING(prob_pos(j,k)),1),0,5),ALIGN=0.5,VALIGN=0.5,COL=color
      ENDFOR
   ENDFOR
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (timesteps)'
   PSCLOSE

   psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_mcu_spatialscale_bymcu.'+psfile_title+'.'+$
          STRTRIM(STRING(domain_size),1)+'x'+STRTRIM(STRING(domain_size),1)+'.mcu_thresh_'+$
          STRMID(STRTRIM(STRING(mcu_thresholds(p)),1),0,4)+'.prob_thresh_radvlag.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=170,MARGIN=2500,/PORTRAIT,XOFFSET=1000,YOFFSET=1000,SPACE2=300,$
          TCHARSIZE=100,SPACE3=400
   plot_title=psfile_title
   GSET,XMIN=-0.5,XMAX=domain_size/2+0.5,YMIN=MIN(lags)-0.5,YMAX=MAX(lags)+0.5       
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs_radvlag)+1
   LEVS,MANUAL=mylevs_radvlag
   CON,X=indgen(domain_size/2+1),Y=lags,FIELD=prob_thresh,/BLOCK,/NOLINES,/NOCOLBAR,$
       TITLE='Prob of M(x,t) > '+STRMID(STRTRIM(STRING(mcu_thresholds(p)),1),0,4)+$
       ' when M(0,0) > '+STRMID(STRTRIM(STRING(mcu_thresholds(p)),1),0,4)+$
       ' ('+STRMID(STRTRIM(STRING(pct_valid(p)),1),0,4)+'% of timesteps)'
   black=FSC_COLOR('black',30)
   white=FSC_COLOR('white',31)
   FOR j=0,domain_size/2 DO BEGIN
      FOR k=0,nlags-1 DO BEGIN
         IF prob_thresh(j,k) ge 0.6 THEN BEGIN
            color=31
         ENDIF ELSE $
            color=30
         GPLOT,X=j,Y=lags(k),TEXT=STRMID(STRTRIM(STRING(prob_thresh(j,k)),1),0,5),ALIGN=0.5,VALIGN=0.5,COL=color
      ENDFOR
   ENDFOR
   AXES,XSTEP=1,YSTEP=1,XTITLE='Distance from central point (gridpoints)',YTITLE='Lag (timesteps)'
   PSCLOSE
   
   box_npts_side=box_size/all_dx(0)
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
         central_point_mcu=REFORM(mcu(central_x,central_y,*))
         valid=where(central_point_mcu gt mcu_thresholds(p))
         IF TOTAL(valid ge 0) THEN BEGIN
            region_mcu=mcu(central_x-box_npts_side/2:$
                           central_x+box_npts_side/2,$
                           central_y-box_npts_side/2:$
                           central_y+box_npts_side/2,*)
            distance=fltarr(box_npts_side,box_npts_side)
            FOR s=0,box_npts_side-1 DO $
               FOR t=0,box_npts_side-1 DO $
                  distance(s,t)=MAX([ABS(box_npts_side/2-s),ABS(box_npts_side/2-t)])
            FOR s=0,box_npts_side/2 DO BEGIN
               mean_mcu=fltarr(tsteps_per_start*nstarts)
               FOR t=0,tsteps_per_start*nstarts-1 DO BEGIN
                  temp_mcu=REFORM(region_mcu(*,*,t))
                  mean_mcu(t)=MEAN(temp_mcu[where(distance eq s)])
               ENDFOR            
               region_corr(p,s)=region_corr(p,s)+CORRELATE(central_point_mcu[valid],mean_mcu[valid])
            ENDFOR                  
            nregions=nregions+1
         ENDIF
      ENDFOR
   ENDFOR
   region_corr(p,*)=region_corr(p,*)/FLOAT(nregions)
   all_npts_side(0)=box_npts_side
   all_psfile_titles(0)=psfile_title
   
   all_nsteps(0)=FLOOR(time_size/all_dt(0))
   temp_npts=0
   FOR x=0,nlon-1 DO BEGIN
      FOR y=0,nlat-1 DO BEGIN         
         mcu_ts=REFORM(mcu(x,y,*))
         valid=where(mcu_ts gt mcu_thresholds(p))
         IF N_ELEMENTS(valid) gt 1 THEN BEGIN
            FOR t=0,nlags-1 DO $
               temporal_corr(p,t)=temporal_corr(p,t)+CORRELATE(mcu_ts[valid],mcu_ts[valid+lags(t)])
            temp_npts=temp_npts+1
         ENDIF
      ENDFOR
   ENDFOR
   temporal_corr(p,*)=temporal_corr(p,*)/FLOAT(temp_npts)
   print,psfile_title,nlon,nlat,box_npts_side,nregions,nlags
ENDFOR

labels=strarr(n_thresh)

psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_mcu_spatialscale_bymcu.many_distance_tstep.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=0,XMAX=box_size/2.,YMIN=-0.2,YMAX=1.01
FOR i=0,n_thresh-1 DO BEGIN
   GPLOT,X=indgen(all_npts_side(0)/2+1)*all_dx(0),Y=REFORM(region_corr(i,0:all_npts_side(0)/2)),STYLE=2,$
         COL=FSC_COLOR(colors(i)),THICK=200,SYM=all_syms(0)
   labels(i)='M > '+STRMID(STRTRIM(STRING(mcu_thresholds(i)),1),0,4)+' ('+STRMID(STRTRIM(STRING(pct_valid(i)),1),0,4)+'%)'
ENDFOR

GPLOT,X=[0,box_size/2],Y=[0,0],STYLE=1
GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=9,SYM=REPLICATE(all_syms(0),n_thresh),LENGTH=50
AXES,XSTEP=100,XMINOR=50,XTITLE='Distance (km)',$
     YTITLE='Correlation of timestep M with central point (x=0)',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/timestep/mjo_diabatic_timestep_mcu_spatialscale_bymcu.many_time_tstep.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=150,MARGIN=2000,XOFFSET=1000,/PORTRAIT,YOFFSET=1000
GSET,XMIN=MIN(lags)-0.5,XMAX=MAX(lags)+0.5,YMIN=-0.2,YMAX=1.01
FOR i=0,n_thresh-1 DO $
   GPLOT,X=lags,Y=REFORM(temporal_corr(i,*)),STYLE=2,$
                         COL=FSC_COLOR(colors(i)),THICK=200,SYM=all_syms(0)
GPLOT,X=[MIN(lags),MAX(lags)],Y=[0,0],STYLE=1
;GLEGEND,labels=REVERSE(labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=7,SYM=REPLICATE(all_syms(0),n_thresh),LENGTH=50
AXES,XSTEP=1,XMINOR=0.5,XTITLE='Time (timesteps)',$
     YTITLE='Lag correlation of gridpoint timestep M',$
     YSTEP=0.1,YMINOR=0.05,NDECS=1
PSCLOSE
         
STOP
END
