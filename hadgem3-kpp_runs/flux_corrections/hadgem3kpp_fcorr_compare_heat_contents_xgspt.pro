PRO hadgem3kpp_fcorr_compare_heat_contents_xgspt

pt=[-10,70]

kpp_infile='/home/ss901165/um_output6/xgspt/15_day/KPPocean.jan-dec_mmeans.i2-i6.nc'
kpp_inst_infile=kpp_infile
n_time=60

longitude=OPEN_AND_EXTRACT(kpp_infile,'longitude')
latitude=OPEN_AND_EXTRACT(kpp_infile,'latitude')
lon_pt=NEAREST(longitude,pt(1))
lat_pt=NEAREST(latitude,pt(0))
z=OPEN_AND_EXTRACT(kpp_infile,'z')
nz=N_ELEMENTS(z)
h=OPEN_AND_EXTRACT(kpp_infile,'h')

inst_T=fltarr(nz,n_time)
inst_rho=fltarr(nz,n_time)
inst_cp=fltarr(nz,n_time)

FOR i=0,n_time-1 DO BEGIN
   time=30*i+10
   IF time lt 100 THEN BEGIN
      time='00'+STRTRIM(STRING(time),1)
   ENDIF ELSE IF time lt 1000 THEN BEGIN
      time='0'+STRTRIM(STRING(time),1)
   ENDIF ELSE $
      time=STRTRIM(STRING(time),1)
   infile='/home/ss901165/um_output6/xgspt/15_day/KPPocean_'+time+'_means.nc'
   inst_T(*,i)=REFORM(OPEN_AND_EXTRACT(infile,'T',$
                                   offset=[lon_pt,lat_pt,0,0],$
                                   count=[1,1,nz,1]))
   inst_rho(*,i)=REFORM(OPEN_AND_EXTRACT(kpp_infile,'rho',$
                                         offset=[lon_pt,lat_pt,0,0],$
                                         count=[1,1,nz,1]))
   inst_cp(*,i)=REFORM(OPEN_AND_EXTRACT(kpp_infile,'cp',$
                                        offset=[lon_pt,lat_pt,0,0],$
                                        count=[1,1,nz,1]))
ENDFOR
mmean_fcorr=REFORM(OPEN_AND_EXTRACT(kpp_infile,'tinc_fcorr',$
                                    offset=[lon_pt,lat_pt,0,0],$
                                    count=[1,1,nz,n_time]))
mmean_rho=REFORM(OPEN_AND_EXTRACT(kpp_infile,'rho',$
                                  offset=[lon_pt,lat_pt,0,0],$
                                  count=[1,1,nz,n_time]))
mmean_cp=REFORM(OPEN_AND_EXTRACT(kpp_infile,'cp',$
                                 offset=[lon_pt,lat_pt,0,0],$
                                 count=[1,1,nz,n_time]))
mmean_solar=REFORM(OPEN_AND_EXTRACT(kpp_inst_infile,'solar_in',$
                                    offset=[lon_pt,lat_pt,0],$
                                    count=[1,1,n_time]))
mmean_nsolar=REFORM(OPEN_AND_EXTRACT(kpp_inst_infile,'nsolar_in',$
                                     offset=[lon_pt,lat_pt,0],$
                                     count=[1,1,n_time]))

vint_heat_content=fltarr(n_time)
dh_dt=fltarr(n_time-1)
vint_fcorr=fltarr(n_time)
flux_in=fltarr(n_time)
FOR i=0,n_time-1 DO BEGIN
   FOR j=0,nz-1 DO BEGIN
      vint_heat_content(i)=vint_heat_content(i)+$
                           inst_T(j,i)*inst_rho(j,i)*inst_cp(j,i)*h(j) ;/TOTAL(h)
      IF j lt nz-1 THEN $
         vint_fcorr(i)=vint_fcorr(i)+$
         mmean_fcorr(j,i)*mmean_rho(j,i)*mmean_cp(j,i)*h(j)*720.;/TOTAL(h)*720.
   ENDFOR
   IF i gt 0 THEN $
      dh_dt(i-1)=vint_heat_content(i)-vint_heat_content(i-1)
   flux_in(i)=(mmean_solar(i)+mmean_nsolar(i))*86400.*30.
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents.xgspt_mmean.70E_10S.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
GSET,XMIN=6,XMAX=n_time-6,YMIN=-1E2,YMAX=1E2,TITLE='Monthly change (with annual smoothing) in heat content at 10S, 70E in xgspt'
dh_dt=SMOOTH(dh_dt,13)
vint_fcorr=SMOOTH(vint_fcorr,13)
flux_in=SMOOTH(flux_in,13)
GPLOT,X=indgen(n_time-13)+6.5,Y=dh_dt(6:n_time-7)/1E6,COL=FSC_COLOR('black')
GPLOT,X=indgen(n_time-13)+6.5,Y=vint_fcorr(6:n_time-7)/1E6,COL=FSC_COLOR('blue')
GPLOT,X=indgen(n_time-13)+6.5,Y=flux_in(6:n_time-7)/1E6,COL=FSC_COLOR('red')
;GPLOT,X=indgen(n_time-13)+13.5,Y=dh_dt-vint_fcorr(13:n_time-14),COL=FSC_COLOR('red')
AXES,XSTEP=12,YSTEP=1E1,NDECS=1,YTITLE='Change in 200m integrated heat content (MJ), annual smoothing',XTITLE='Month of integration'
GLEGEND,labels=['Net input from atmosphere','Net input from flux correction','Change in ocean heat content'],$
        col=[FSC_COLOR('red'),FSC_COLOR('blue'),FSC_COLOR('black')],LEGPOS=3
PSCLOSE

vint_heat_content_amean=fltarr(n_time/12)
dh_dt_amean=fltarr(n_time/12-1)
vint_fcorr_amean=fltarr(n_time/12)
flux_in_amean=fltarr(n_time/12)
FOR i=0,n_time/12-1 DO BEGIN
   FOR j=0,nz-1 DO BEGIN
      vint_heat_content_amean(i)=vint_heat_content_amean(i)+$
                                 inst_T(j,i*12)*inst_rho(j,i*12)*$
                                 inst_cp(j,i*12)*h(j) ;/TOTAL(h)
      IF j lt nz-1 THEN $
         vint_fcorr_amean(i)=vint_fcorr_amean(i)+$
                             MEAN(mmean_fcorr(j,i*12:(i+1)*12-1))*MEAN(mmean_rho(j,i*12:(i+1)*12-1))*$
                             MEAN(mmean_cp(j,i*12:(i+1)*12-1))*h(j)*720.*12.;/TOTAL(h)
   ENDFOR
   IF i gt 0 THEN $
      dh_dt_amean(i-1)=vint_heat_content_amean(i)-vint_heat_content_amean(i-1)
   flux_in_amean(i)=(MEAN(mmean_solar(i*12:(i+1)*12-1))+MEAN(mmean_nsolar(i*12:(i+1)*12-1)))*86400.*360.
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_heat_contents.xgspt_amean.70E_10S.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110
GSET,XMIN=1,XMAX=n_time/12,YMIN=-500,YMAX=500,TITLE='Annual change in heat content at 10S, 70E in xgspt'
GPLOT,X=indgen(n_time/12-1)+1.5,Y=dh_dt_amean(0:n_time/12-2)/1E6,COL=FSC_COLOR('black')
GPLOT,X=indgen(n_time/12-1)+1.5,Y=vint_fcorr_amean(0:n_time/12-2)/1E6,COL=FSC_COLOR('blue')
GPLOT,X=indgen(n_time/12-1)+1.5,Y=flux_in_amean(0:n_time/12-2)/1E6,COL=FSC_COLOR('red')
GPLOT,X=indgen(n_time/12-1)+1.5,Y=(dh_dt_amean(0:n_time/12-2)-(vint_fcorr_amean(0:n_time/12-2)+flux_in_amean(0:n_time/12-2)))/1E6,$
      COL=FSC_COLOR('black'),STYLE=2
GLEGEND,labels=['Residual (bottom heat correction?)','Net input from atmosphere','Net input from flux correction','Change in ocean heat content'],COL=[FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('blue'),FSC_COLOR('black')],STYLE=[2,0,0,0],LEGPOS=3
AXES,XSTEP=1,XMINOR=0.5,YSTEP=50,NDECS=1,YTITLE='Change in 200m integrated heat content (MJ)',XTITLE='Year of integration'
PSCLOSE

STOP
END

