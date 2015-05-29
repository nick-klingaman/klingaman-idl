PRO mjo_diabatic_synthesis_rhprecip,models,lead_times

; Calculate the RH-precip diagnostic from Xianan's paper using my 
; 20-day hindcast dataset.

n_models=N_ELEMENTS(models)
n_leads=N_ELEMENTS(lead_times)

basedir='/home/ss901165/um_output6/mjodiab_20day'

all_codes=strarr(n_models)
all_colors=strarr(n_models)
all_fidelity=intarr(n_models)

top5_rh=fltarr(n_models)
top5_pr=fltarr(n_models)
bot10_rh=fltarr(n_models)
bot10_pr=fltarr(n_models)

box=[-15,60,15,180]

FOR m=0,n_models-1 DO BEGIN
   CASE models(m) OF
      'cam5zm' : BEGIN
         dir=basedir+'/cam5zm'
         name='cam5zm'
         all_codes(m)='CZ'
         all_colors(m)='forestgreen'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1.
         nt=94     
         all_fidelity(m)=20
      END
      'cam5' : BEGIN
         dir=basedir+'/cam5'
         name='cam5'
         all_codes(m)='C5'
         all_colors(m)='forestgreen'
         z_name='levels'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1.
         nt=94     
         all_fidelity(m)=20
      END
      'cancm4' : BEGIN
         dir=basedir+'/cancm4'
         name='cancm4'
         all_codes(m)='CC'
         all_colors(m)='firebrick'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=0.01
         nt=94     
         all_fidelity(m)=6
      END
      'cnrm_atmos' : BEGIN
         dir=basedir+'/cnrm_atmos'
         name='cnrm_atmos'
         all_codes(m)='CN'
         all_colors(m)='orangered'
         z_name='lev'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1.
         nt=94     
         all_fidelity(m)=15
      END
      'ecearth' : BEGIN
         dir=basedir+'/ecearth'
         name='ecearth'
         all_codes(m)='E3'
         all_colors(m)='orangered'
         z_name='lev'
         lon_name='lon'
         lat_name='lat'
         z_mult=0.01
         nt=94     
         all_fidelity(m)=16
      END      
      'ecmwf' : BEGIN
         dir=basedir+'/ecmwf'
         name='ecmwf'
         all_codes(m)='EC'
         all_colors(m)='forestgreen'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94    
         all_fidelity(m)=18
      END      
      'giss' : BEGIN
         dir=basedir+'/giss'
         name='giss'
         all_codes(m)='GI'
         all_colors(m)='orangered'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94     
         all_fidelity(m)=16
      END
      'metum' : BEGIN
         dir=basedir+'/metum'
         name='metum'
         all_codes(m)='MO'
         all_colors(m)='orangered'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94     
         all_fidelity(m)=16
      END
      'miroc' : BEGIN
         dir=basedir+'/miroc'
         name='miroc'
         all_codes(m)='MI'
         all_colors(m)='firebrick'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94
         all_fidelity(m)=14
      END
      'mri' : BEGIN
         dir=basedir+'/mri'
         name='mri'
         all_codes(m)='MR'
         all_colors(m)='orangered'
         z_name='plev'
         lon_name='lon'
         lat_name='lat'
         z_mult=0.01
         nt=94
         all_fidelity(m)=16
      END
      'nasa' : BEGIN
         dir=basedir+'/nasa'
         name='nasa'
         all_codes(m)='NA'
         all_colors(m)='forestgreen'
         z_name='lev'
         lon_name='lon'
         lat_name='lat'
         z_mult=1
         nt=94
         all_fidelity(m)=18
      END
      'nrl' : BEGIN
         dir=basedir+'/nrl'
         name='nrl'
         all_codes(m)='NR'
         all_colors(m)='firebrick'
         z_name='level'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94
         all_fidelity(m)=10
      END
      'spcam' : BEGIN
         dir=basedir+'/spcam'
         name='spcam'
         all_codes(m)='SP'
         all_colors(m)='firebrick'
         z_name='lev_p'
         lon_name='longitude'
         lat_name='latitude'
         z_mult=1
         nt=94
         all_fidelity(m)=13
      END      
   ENDCASE
   
   grid_flag=0
   FOR l=0,n_leads-1 DO BEGIN
      lead_str=STRTRIM(STRING(lead_times(l)),1)
      hus_file=dir+'/'+name+'.20091010-20100125_dmeans.hus_lead'+lead_str+'.nc'
      ta_file=dir+'/'+name+'.20091010-20100125_dmeans.ta_lead'+lead_str+'.nc'
      pr_file=dir+'/'+name+'.20091010-20100125_dmeans.pr_lead'+lead_str+'.nc'

      lon=OPEN_AND_EXTRACT(hus_file,lon_name)
      lat=OPEN_AND_EXTRACT(hus_file,lat_name)
      DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
      nlon=N_ELEMENTS(lon)
      nlat=N_ELEMENTS(lat)
      p=OPEN_AND_EXTRACT(hus_file,z_name)*z_mult
      pbot=NEAREST(p,850.)
      ptop=NEAREST(p,500.)
      np=ptop-pbot+1

      IF grid_flag eq 0 THEN BEGIN
         all_pr=fltarr(nlon,nlat,nt*n_leads)
         all_rh=fltarr(nlon,nlat,nt*n_leads)
         grid_flag=1
      ENDIF

      ; Read hus, ta, pr
      hus=OPEN_AND_EXTRACT(hus_file,'hus',offset=[box_tx(1),box_tx(0),pbot,0],$
                           count=[nlon,nlat,np,nt])
      ta=OPEN_AND_EXTRACT(ta_file,'ta',offset=[box_tx(1),box_tx(0),pbot,0],$
                           count=[nlon,nlat,np,nt])
      pr=OPEN_AND_EXTRACT(pr_file,'pr',offset=[box_tx(1),box_tx(0),0],$
                          count=[nlon,nlat,nt])

      IF TOTAL(where(ABS(ta) ge 1000)) ge 0 THEN $
         ta[where(ABS(ta) ge 1000)]=!Values.F_NaN
      IF TOTAL(where(ABS(hus) ge 1000)) ge 0 THEN $
         hus[where(ABS(hus) ge 1000)]=!Values.F_NaN

      rh=fltarr(nlon,nlat,np,nt)
      FOR z=0,np-1 DO BEGIN
         FOR x=0,nlon-1 DO BEGIN
            FOR y=0,nlat-1 DO BEGIN
               FOR t=0,nt-1 DO BEGIN
                  temp=ta(x,y,z,t)-273.15
                  es=6.112*exp(17.67*temp/(temp+243.5))
                  e=hus(x,y,z,t)*p(z)/(0.378*hus(x,y,z,t)+0.622)
                  rh(x,y,z,t)=e/es*100.
               ENDFOR
            ENDFOR
         ENDFOR
      ENDFOR

      ; Store RH and pr
      FOR x=0,nlon-1 DO BEGIN
         FOR y=0,nlat-1 DO BEGIN
            FOR t=0,nt-1 DO BEGIN               
               all_pr(x,y,l*nt+t)=pr(x,y,t)
               all_rh(x,y,l*nt+t)=MEAN(rh(x,y,*,t),/NaN)
            ENDFOR
         ENDFOR
      ENDFOR      
      
   ENDFOR

   ; Find top 5% and bottom 10% of precipitation events
   valid=where(FINITE(all_rh) eq 1)
   all_pr=all_pr[valid]
   all_rh=all_rh[valid]
   ;print,models(m),MAX(all_rh)
   all_rh=all_rh/MAX(all_rh)*100.

   pr_sort=SORT(all_pr)
   npr=N_ELEMENTS(all_pr)
   top5=pr_sort(FLOOR(0.95*npr):npr-1)
   bot10=pr_sort(0:FLOOR(0.10*npr))
 
   top5_rh(m)=MEAN(all_rh[top5])
   bot10_rh(m)=MEAN(all_rh[bot10])

ENDFOR

psfile='/home/ss901165/idl/mjo_diabatic/synthesis/mjo_diabatic_synthesis_rhprecip.20day_top5-minus-bot10.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,TFONT=6,MARGIN=3000,XSIZE=16000,YSIZE=16000
GSET,XMIN=40,XMAX=60,YMIN=5,YMAX=21
FOR m=0,n_models-1 DO BEGIN
   GPLOT,X=top5_rh(m)-bot10_rh(m),Y=all_fidelity(m),SYM=3,/NOLINES
   IF models(m) eq 'giss' THEN BEGIN
      GPLOT,X=top5_rh(m)-bot10_rh(m)+0.8,Y=all_fidelity(m)-0.15,TEXT=all_codes(m),$
            COL=FSC_COLOR(all_colors(m))
   ENDIF ELSE $
      GPLOT,X=top5_rh(m)-bot10_rh(m),Y=all_fidelity(m)+0.2,TEXT=all_codes(m),$
            COL=FSC_COLOR(all_colors(m))
   print,models(m),top5_rh(m)-bot10_rh(m)
ENDFOR
coeff=REGRESS((top5_rh-bot10_rh),all_fidelity,CONST=constant)
GPLOT,X=[40,57.1],Y=[40,57.1]*coeff(0)+constant,STYLE=2
GPLOT,X=47,Y=11,TEXT='r = '+STRMID(STRTRIM(STRING(CORRELATE(top5_rh-bot10_Rh,all_fidelity)),1),0,4)
AXES,XSTEP=4,XMINOR=1,YSTEP=3,YMINOR=1,$
     XTITLE='RH difference between top 5% and bottom 10% rain (%)',$
     YTITLE='Fidelity (lead of RMM bivar corr < 0.7; days)'
PSCLOSE

STOP
END
