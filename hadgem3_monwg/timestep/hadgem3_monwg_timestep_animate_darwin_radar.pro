PRO hadgem3_monwg_timestep_animate_darwin_radar

infile='/home/ss901165/datasets/DARWIN_RADAR/cpol_rainrate_cappi_2p5km_20051110-20060430_10min.nc'
start_plot=2000
stop_plot=3000
n_times_read=3000

x=OPEN_AND_EXTRACT(infile,'x')
y=OPEN_AND_EXTRACT(infile,'y')
nx=N_ELEMENTS(x)
ny=N_ELEMENTS(y)
precip=OPEN_AND_EXTRACT(infile,'rr',offset=[0,0,0],count=[nx,ny,n_times_read])

mylevs=['0.1','0.2','0.35','0.5','0.7','1.0','2.0','3.5','5.0','7.0','10','20','35','50','70']
FOR i=start_plot,stop_plot-1 DO BEGIN
   print,i
   IF i lt 10 THEN BEGIN
      step='000'+STRTRIM(STRING(i),1)
   ENDIF ELSE IF i lt 100 THEN BEGIN
      step='00'+STRTRIM(STRING(i),1)
   ENDIF ELSE IF i lt 1000 THEN BEGIN
      step='0'+STRTRIM(STRING(i),1)
   ENDIF ELSE $
      step=STRTRIM(STRING(i),1)

   hour=(i MOD (24*6))/6
   hour_str=STRTRIM(STRING(hour),1)
   IF hour lt 10 THEN $
      hour_str='0'+hour_str
   min=(i MOD 6)*10   
   min_str=STRTRIM(STRING(min),1)
   IF min lt 10 THEN $
      min_str='0'+min_str
   day=i/(24*6)
  
   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/darwin_radar_animation/step'+step+'.ps'
   PSOPEN,file=psfile,FONT=6,CHARSIZE=140,MARGIN=3000,XOFFSET=0,YOFFSET=3000,SPACE2=2500,/PORTRAIT,XSIZE=17000,YSIZE=17000,TFONT=6
   CS,SCALE=2,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
   LEVS,MANUAL=mylevs   
   GSET,XMIN=-60*2.5,XMAX=60*2.5,YMIN=-60*2.5,YMAX=60*2.5
   CON,X=indgen(nx)*2.5-(nx*2.5/2),Y=indgen(ny)*2.5-(ny*2.5/2),FIELD=REFORM(precip(*,*,i)),TITLE='Darwin radar-derived rain rate at '+$
       hour_str+':'+min_str+' on day '+STRTRIM(STRING(day),1)+' (since 10 Nov 2005)',$
       CB_TITLE='Precipitation rate (mm hr!U-1!N)',/NOLINES,/BLOCK,CB_WIDTH=110
   points=(2*!Pi/99.0)*findgen(100)
   GPLOT,X=0,Y=0,SYM=3,SIZE=50
   rad=[25,50,75,100,125,150]   
   FOR j=0,N_ELEMENTS(rad)-1 DO BEGIN
      x=COS(points)*rad(j)
      y=SIN(points)*rad(j)
      GPLOT,X=x,Y=y
   ENDFOR
   AXES,XSTEP=25,YSTEP=25,XTITLE='Distance from radar (km)',YTITLE='Distance from radar (km)'
   PSCLOSE,/NOVIEW
ENDFOR


STOP
END

   
