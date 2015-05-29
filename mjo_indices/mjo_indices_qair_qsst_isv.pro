PRO mjo_indices_qair_qsst_isv

xihvp='/home/ss901165/um_output6/xihvp'
; Daily mean q_air
qair_file=xihvp+'/ga3kpp_1.5xentrain_fwgbl.jan-dec_dmeans_ts.years1-25.q1p5m.nc'
; Daily mean SST
sst_file=xihvp+'/ga3kpp_1.5xentrain_fwgbl.jan-dec_dmeans_ts.years1-25.ts.nc'
; Filtered precipitation
precip_file=xihvp+'/ga3kpp_1.5xentrain_fwgbl.jan-dec_dmeans_ts_filter2080.years1-25.precip.nc'
; Mask
mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'

box=[-10,80,0,90]
n_time=23*360.
time_offset=360

; Read mask
lon=OPEN_AND_EXTRACT(mask_file,'longitude')
lat=OPEN_AND_EXTRACT(mask_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
n_lon=N_ELEMENTS(lon)
n_lat=N_ELEMENTS(lat)
lsm=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm',$
                            offset=[box_tx(1),box_tx(0),0,0],count=[n_lon,n_lat,1,1]))

; Read q_air
lon=OPEN_AND_EXTRACT(qair_file,'longitude')
lat=OPEN_AND_EXTRACT(qair_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
n_lon=N_ELEMENTS(lon)
n_lat=N_ELEMENTS(lat)
q_air=OPEN_AND_EXTRACT(qair_file,'q',offset=[box_tx(1),box_tx(0),time_offset],count=[n_lon,n_lat,n_time])

; Read SST
lon=OPEN_AND_EXTRACT(sst_file,'longitude')
lat=OPEN_AND_EXTRACT(sst_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
n_lon=N_ELEMENTS(lon)
n_lat=N_ELEMENTS(lat)
sst=OPEN_AND_EXTRACT(sst_file,'temp_1',offset=[box_tx(1),box_tx(0),time_offset],count=[n_lon,n_lat,n_time])-273.15

; Read filtered precipitation
lon=OPEN_AND_EXTRACT(precip_file,'longitude')
lat=OPEN_AND_EXTRACT(precip_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
n_lon=N_ELEMENTS(lon)
n_lat=N_ELEMENTS(lat)
precip=OPEN_AND_EXTRACT(precip_file,'precip',offset=[box_tx(1),box_tx(0),time_offset-40],count=[n_lon,n_lat,n_time])*86400.

ew=6.1121*(1.0007+3.46e-6*1000.)*exp((17.502*sst)/(240.97+sst))
q_sst=0.62197*(ew/(1000-0.378*ew))

q_sst_aavg=fltarr(n_time)
q_air_aavg=fltarr(n_time)
precip_aavg=fltarr(n_time)
FOR i=0,n_time-1 DO BEGIN
   q_sst_aavg(i)=MEAN(q_sst(*,*,i))*1000.
   q_air_aavg(i)=MEAN(q_air(*,*,i))*1000.
   precip_aavg(i)=MEAN(precip(*,*,i))
ENDFOR

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_qair_qsst_isv.xihvp_indocn_qairvqsst.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=500,XSIZE=17000,YSIZE=17000
xmin=22
xmax=28
GSET,XMIN=xmin,XMAX=xmax,YMIN=14,YMAX=20
precip_stddev=STDDEV(precip_aavg)
red = FSC_COLOR('red',30)
orange = FSC_COLOR('orange',31)
cyan = FSC_COLOR('dodgerblue',32)
blue = FSC_COLOR('purple',33)
FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         toplot=where(precip_aavg le (-2)*precip_stddev)
         color=30   
         y_offset=0
      END
      1 : BEGIN
         toplot=where(precip_aavg gt (-2)*precip_stddev and precip_aavg le (-1.)*precip_stddev)
         color=31
         y_offset=0
      END
      2 : BEGIN
         toplot=where(precip_aavg ge precip_stddev and precip_aavg lt 2*precip_stddev)
         color=32
         y_offset=-0.2
      END
      3 : BEGIN
         toplot=where(precip_aavg ge 2*precip_stddev)
         color=33
         y_offset=0.2
      END
   ENDCASE
   regression=REGRESS(q_sst_aavg[toplot],q_air_aavg[toplot],const=constant)
   correlation=CORRELATE(q_sst_aavg[toplot],q_air_aavg[toplot])
   GPLOT,X=q_sst_aavg[toplot],Y=q_air_aavg[toplot],COL=color,SIZE=30,/NOLINES,SYM=3
   GPLOT,X=[xmin,xmax],Y=[xmin*regression(0)+constant,xmax*regression(0)+constant],$
         COL=color,STYLE=2
   GPLOT,X=xmax+1.6,Y=(xmax)*regression(0)+constant+y_offset,TEXT='q!Dair!N = '+$
         STRMID(STRTRIM(STRING(regression(0)),1),0,4)+'*q!DSST!N+'+STRMID(STRTRIM(STRING(constant),1),0,4)+'  r='+$
         STRMID(STRTRIM(STRING(correlation),1),0,4),COL=color

ENDFOR
AXES,XSTEP=0.5,YSTEP=0.5,XTITLE='q!U*!N!DSST!N',YTITLE='q!Dair!N',NDECS=1,ORIENTATION=30
GLEGEND,labels=['pr >= 2*stddev','2*stddev > pr >= stddev','-2*stddev < pr <= -stddev','pr <= -2*stddev'],$
        COL=[33,32,31,30],LENGTH=0,SYM=[3,3,3,3],LEGXOFFSET=10000,LEGYOFFSET=3000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_qair_qsst_isv.xihvp_indocn_dqvqsst.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=500,XSIZE=17000,YSIZE=17000
xmin=22
xmax=28
GSET,XMIN=xmin,XMAX=xmax,YMIN=5,YMAX=11
precip_stddev=STDDEV(precip_aavg)
red = FSC_COLOR('red',30)
orange = FSC_COLOR('orange',31)
cyan = FSC_COLOR('dodgerblue',32)
blue = FSC_COLOR('purple',33)
FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         toplot=where(precip_aavg le (-2)*precip_stddev)
         color=30   
         y_offset=0
      END
      1 : BEGIN
         toplot=where(precip_aavg gt (-2)*precip_stddev and precip_aavg le (-1.)*precip_stddev)
         color=31
         y_offset=0
      END
      2 : BEGIN
         toplot=where(precip_aavg ge precip_stddev and precip_aavg lt 2*precip_stddev)
         color=32
         y_offset=0.2
      END
      3 : BEGIN
         toplot=where(precip_aavg ge 2*precip_stddev)
         color=33
         y_offset=-0.2
      END
   ENDCASE
   regression=REGRESS(q_sst_aavg[toplot],q_sst_aavg[toplot]-q_air_aavg[toplot],const=constant)
   correlation=CORRELATE(q_sst_aavg[toplot],q_sst_aavg[toplot]-q_air_aavg[toplot])
   GPLOT,X=q_sst_aavg[toplot],Y=q_sst_aavg[toplot]-q_air_aavg[toplot],COL=color,SIZE=30,/NOLINES,SYM=3
   GPLOT,X=[xmin,xmax],Y=[xmin*regression(0)+constant,xmax*regression(0)+constant],$
         COL=color,STYLE=2
   GPLOT,X=xmax+1.6,Y=(xmax)*regression(0)+constant+y_offset,TEXT='dq = '+$
         STRMID(STRTRIM(STRING(regression(0)),1),0,4)+'*q!DSST!N'+STRMID(STRTRIM(STRING(constant),1),0,5)+'  r='+$
         STRMID(STRTRIM(STRING(correlation),1),0,4),COL=color

ENDFOR
AXES,XSTEP=0.5,YSTEP=0.5,XTITLE='q!U*!N!DSST!N',YTITLE='q!U*!N!DSST!N - q!Dair!N',NDECS=1,ORIENTATION=30
GLEGEND,labels=['pr >= 2*stddev','2*stddev > pr >= stddev','-2*stddev < pr <= -stddev','pr <= -2*stddev'],$
        COL=[33,32,31,30],LENGTH=0,SYM=[3,3,3,3],LEGXOFFSET=10000,LEGYOFFSET=3000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_qair_qsst_isv.xihvp_indocn_dqvqair.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=500,XSIZE=17000,YSIZE=17000
xmin=14
xmax=20
GSET,XMIN=xmin,XMAX=xmax,YMIN=5,YMAX=11
precip_stddev=STDDEV(precip_aavg)
red = FSC_COLOR('red',30)
orange = FSC_COLOR('orange',31)
cyan = FSC_COLOR('dodgerblue',32)
blue = FSC_COLOR('purple',33)
FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         toplot=where(precip_aavg le (-2)*precip_stddev)
         color=30   
         y_offset=0.2
      END
      1 : BEGIN
         toplot=where(precip_aavg gt (-2)*precip_stddev and precip_aavg le (-1.)*precip_stddev)
         color=31
         y_offset=-0.1
      END
      2 : BEGIN
         toplot=where(precip_aavg ge precip_stddev and precip_aavg lt 2*precip_stddev)
         color=32
         y_offset=0
      END
      3 : BEGIN
         toplot=where(precip_aavg ge 2*precip_stddev)
         color=33
         y_offset=0.2
      END
   ENDCASE
   regression=REGRESS(q_air_aavg[toplot],q_sst_aavg[toplot]-q_air_aavg[toplot],const=constant)
   correlation=CORRELATE(q_air_aavg[toplot],q_sst_aavg[toplot]-q_air_aavg[toplot])
   GPLOT,X=q_air_aavg[toplot],Y=q_sst_aavg[toplot]-q_air_aavg[toplot],COL=color,SIZE=30,/NOLINES,SYM=3
   GPLOT,X=[xmin,xmax],Y=[xmin*regression(0)+constant,xmax*regression(0)+constant],$
         COL=color,STYLE=2
   GPLOT,X=xmax+1.6,Y=(xmax)*regression(0)+constant+y_offset,TEXT='dq = '+$
         STRMID(STRTRIM(STRING(regression(0)),1),0,5)+'*q!Dair!N+'+STRMID(STRTRIM(STRING(constant),1),0,4)+'  r='+$
         STRMID(STRTRIM(STRING(correlation),1),0,5),COL=color

ENDFOR
AXES,XSTEP=0.5,YSTEP=0.5,XTITLE='q!U*!N!Dair!N',YTITLE='q!U*!N!DSST!N - q!Dair!N',NDECS=1,ORIENTATION=30
GLEGEND,labels=['pr >= 2*stddev','2*stddev > pr >= stddev','-2*stddev < pr <= -stddev','pr <= -2*stddev'],$
        COL=[33,32,31,30],LENGTH=0,SYM=[3,3,3,3],LEGXOFFSET=10000,LEGYOFFSET=18000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_qair_qsst_isv.xihvp_indocn_divvqsst.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=500,XSIZE=17000,YSIZE=17000
xmin=22
xmax=28
GSET,XMIN=xmin,XMAX=xmax,YMIN=0.56,YMAX=0.8
precip_stddev=STDDEV(precip_aavg)
red = FSC_COLOR('red',30)
orange = FSC_COLOR('orange',31)
cyan = FSC_COLOR('dodgerblue',32)
blue = FSC_COLOR('purple',33)
FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         toplot=where(precip_aavg le (-2)*precip_stddev)
         color=30   
         y_offset=0
      END
      1 : BEGIN
         toplot=where(precip_aavg gt (-2)*precip_stddev and precip_aavg le (-1.)*precip_stddev)
         color=31
         y_offset=0
      END
      2 : BEGIN
         toplot=where(precip_aavg ge precip_stddev and precip_aavg lt 2*precip_stddev)
         color=32
         y_offset=-0.01
      END
      3 : BEGIN
         toplot=where(precip_aavg ge 2*precip_stddev)
         color=33
         y_offset=0.01
      END
   ENDCASE
   regression=REGRESS(q_sst_aavg[toplot],q_air_aavg[toplot]/q_sst_aavg[toplot],const=constant)
   correlation=CORRELATE(q_sst_aavg[toplot],q_air_aavg[toplot]/q_sst_aavg[toplot])
   GPLOT,X=q_sst_aavg[toplot],Y=q_air_aavg[toplot]/q_sst_aavg[toplot],COL=color,SIZE=30,/NOLINES,SYM=3
   GPLOT,X=[xmin,xmax],Y=[xmin*regression(0)+constant,xmax*regression(0)+constant],$
         COL=color,STYLE=2
   GPLOT,X=xmax+1.7,Y=(xmax)*regression(0)+constant+y_offset,TEXT='q!Dair!N/q!U*!N!DSST!N = '+$
         STRMID(STRTRIM(STRING(regression(0)),1),0,6)+'*q!DSST!N+'+STRMID(STRTRIM(STRING(constant),1),0,4)+'  r='+$
         STRMID(STRTRIM(STRING(correlation),1),0,5),COL=color,CHARSIZE=90

ENDFOR
AXES,XSTEP=0.5,YSTEP=0.02,XTITLE='q!U*!N!DSST!N',YTITLE='q!Dair!N / q!DSST!N',NDECS=2,ORIENTATION=30
GLEGEND,labels=['pr >= 2*stddev','2*stddev > pr >= stddev','-2*stddev < pr <= -stddev','pr <= -2*stddev'],$
        COL=[33,32,31,30],LENGTH=0,SYM=[3,3,3,3],LEGXOFFSET=10000,LEGYOFFSET=18000
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_qair_qsst_isv.xihvp_indocn_divvqair.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=500,XSIZE=17000,YSIZE=17000
xmin=14
xmax=20
GSET,XMIN=xmin,XMAX=xmax,YMIN=0.56,YMAX=0.8
precip_stddev=STDDEV(precip_aavg)
red = FSC_COLOR('red',30)
orange = FSC_COLOR('orange',31)
cyan = FSC_COLOR('dodgerblue',32)
blue = FSC_COLOR('purple',33)
FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         toplot=where(precip_aavg le (-2)*precip_stddev)
         color=30   
         y_offset=0
      END
      1 : BEGIN
         toplot=where(precip_aavg gt (-2)*precip_stddev and precip_aavg le (-1.)*precip_stddev)
         color=31
         y_offset=0.004
      END
      2 : BEGIN
         toplot=where(precip_aavg ge precip_stddev and precip_aavg lt 2*precip_stddev)
         color=32
         y_offset=-0.004
      END
      3 : BEGIN
         toplot=where(precip_aavg ge 2*precip_stddev)
         color=33
         y_offset=-0.004
      END
   ENDCASE
   regression=REGRESS(q_air_aavg[toplot],q_air_aavg[toplot]/q_sst_aavg[toplot],const=constant)
   correlation=CORRELATE(q_air_aavg[toplot],q_air_aavg[toplot]/q_sst_aavg[toplot])
   GPLOT,X=q_air_aavg[toplot],Y=q_air_aavg[toplot]/q_sst_aavg[toplot],COL=color,SIZE=30,/NOLINES,SYM=3
   GPLOT,X=[xmin,xmax],Y=[xmin*regression(0)+constant,xmax*regression(0)+constant],$
         COL=color,STYLE=2
   GPLOT,X=xmax+1.7,Y=(xmax)*regression(0)+constant+y_offset,TEXT='q!Dair!N/q!U*!N!DSST!N = '+$
         STRMID(STRTRIM(STRING(regression(0)),1),0,6)+'*q!Dair!N+'+STRMID(STRTRIM(STRING(constant),1),0,4)+'  r='+$
         STRMID(STRTRIM(STRING(correlation),1),0,5),COL=color,CHARSIZE=90

ENDFOR
AXES,XSTEP=0.5,YSTEP=0.02,XTITLE='q!U*!N!Dair!N',YTITLE='q!Dair!N / q!DSST!N',NDECS=2,ORIENTATION=30
GLEGEND,labels=['pr >= 2*stddev','2*stddev > pr >= stddev','-2*stddev < pr <= -stddev','pr <= -2*stddev'],$
        COL=[33,32,31,30],LENGTH=0,SYM=[3,3,3,3],LEGXOFFSET=10000,LEGYOFFSET=3000
PSCLOSE

STOP
END
