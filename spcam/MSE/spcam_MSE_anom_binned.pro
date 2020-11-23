PRO spcam_MSE_anom_binned

box=[-10,80,10,90]
ntime=10*365

bins=[-4,-3,-2,-1,0,1,2,3,4]
nbins=N_ELEMENTS(bins)

casename='spcam_iceedge_free'
date_range='00010101-00101231'
basedir='/group_workspaces/jasmin2/klingaman/cam/spcam_iceedge_free/airsea'
;basedir='/group_workspaces/jasmin2/klingaman/cam/spccsm_mjodiab/airsea'
MSE_anom_file=basedir+'/'+casename+'.Vmse.'+date_range+'.30S-30N.day.anom.nc'
LH_anom_file=basedir+'/'+casename+'.LHFLX.'+date_range+'.30S-30N.day.mean.nc'
SH_anom_file=basedir+'/'+casename+'.SHFLX.'+date_range+'.30S-30N.day.mean.nc'
uadv_anom_file=basedir+'/'+casename+'.Vudmdx.'+date_range+'.30S-30N.day.mean.nc'
vadv_anom_file=basedir+'/'+casename+'.Vvdmdy.'+date_range+'.30S-30N.day.mean.nc'
wadv_anom_file=basedir+'/'+casename+'.Vomegadmdp.'+date_range+'.30S-30N.day.mean.nc'
vsw_anom_file=basedir+'/'+casename+'.Vsw.'+date_range+'.30S-30N.day.mean.nc'
vlw_anom_file=basedir+'/'+casename+'.Vlw.'+date_range+'.30S-30N.day.mean.nc'

latitude=OPEN_AND_EXTRACT(MSE_anom_file,'lat')
longitude=OPEN_AND_EXTRACT(MSE_anom_file,'lon')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
nlon=N_ELEMENTS(longitude)
nlat=N_ELEMENTS(latitude)

; Read MSE
MSE_anom=REFORM(OPEN_AND_EXTRACT(MSE_anom_file,'Vmse_anom_sm',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))/1E7
; Read LH
LH_anom=REFORM(OPEN_AND_EXTRACT(LH_anom_file,'LHFLX',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read SH
SH_anom=REFORM(OPEN_AND_EXTRACT(SH_anom_file,'SHFLX',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read -U*dH/dx
uadv_anom=REFORM(OPEN_AND_EXTRACT(uadv_anom_file,'Vudmdx',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read -V*dH/dx
vadv_anom=REFORM(OPEN_AND_EXTRACT(vadv_anom_file,'Vvdmdy',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read -W*dH/dp
wadv_anom=REFORM(OPEN_AND_EXTRACT(wadv_anom_file,'Vomegadmdp',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read Vsw
Vsw_anom=REFORM(OPEN_AND_EXTRACT(vsw_anom_file,'Vsw',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))
; Read Vlw
Vlw_anom=REFORM(OPEN_AND_EXTRACT(vlw_anom_file,'Vlw',offset=[box_tx(1),box_tx(0),0],count=[nlon,nlat,ntime]))

LH_anom_binned=fltarr(nbins+1)
SH_anom_binned=fltarr(nbins+1)
uadv_anom_binned=fltarr(nbins+1)
vadv_anom_binned=fltarr(nbins+1)
wadv_anom_binned=fltarr(nbins+1)
vsw_anom_binned=fltarr(nbins+1)
vlw_anom_binned=fltarr(nbins+1)
MSEpdf=fltarr(nbins+1)
FOR i=1,nbins-1 DO BEGIN
   valid=where(MSE_anom ge bins(i-1) and MSE_anom lt bins(i))
   IF TOTAL(valid) ge 0 THEN BEGIN
      LH_anom_binned(i)=MEAN(LH_anom[valid])
      SH_anom_binned(i)=MEAN(SH_anom[valid])
      uadv_anom_binned(i)=MEAN(uadv_anom[valid])
      vadv_anom_binned(i)=MEAN(vadv_anom[valid])
      wadv_anom_binned(i)=MEAN(wadv_anom[valid])
      vsw_anom_binned(i)=MEAN(vsw_anom[valid])
      vlw_anom_binned(i)=MEAN(vlw_anom[valid])
      MSEpdf(i)=N_ELEMENTS(valid)
   ENDIF ELSE BEGIN
      LH_anom_binned(i)=!Values.F_NaN
      SH_anom_binned(i)=!Values.F_NaN
      uadv_anom_binned(i)=!Values.F_NaN
      vadv_anom_binned(i)=!Values.F_NaN
      wadv_anom_binned(i)=!Values.F_NaN
      vsw_anom_binned(i)=!Values.F_NaN
      vlw_anom_binned(i)=!Values.F_NaN
      MSEpdf(i)=0
   ENDELSE
ENDFOR
valid=where(MSE_anom lt bins(0))
IF TOTAL(valid) ge 0 THEN BEGIN
   LH_anom_binned(0)=MEAN(LH_anom[valid])
   SH_anom_binned(0)=MEAN(SH_anom[valid])
   uadv_anom_binned(0)=MEAN(uadv_anom[valid])
   vadv_anom_binned(0)=MEAN(vadv_anom[valid])
   wadv_anom_binned(0)=MEAN(wadv_anom[valid])  
   vsw_anom_binned(0)=MEAN(vsw_anom[valid])
   vlw_anom_binned(0)=MEAN(vlw_anom[valid])  
   MSEpdf(0)=N_ELEMENTS(valid)
ENDIF ELSE BEGIN
   LH_anom_binned(0)=!Values.F_NaN
   SH_anom_binned(0)=!Values.F_NaN
   uadv_anom_binned(0)=!Values.F_NaN
   vadv_anom_binned(0)=!Values.F_NaN
   wadv_anom_binned(0)=!Values.F_NaN
   vsw_anom_binned(0)=!Values.F_NaN
   vlw_anom_binned(0)=!Values.F_NaN
   MSEpdf(0)=0
ENDELSE
valid=where(MSE_anom ge bins(nbins-1))
IF TOTAL(valid) ge 0 THEN BEGIN
   LH_anom_binned(nbins)=MEAN(LH_anom[valid])
   SH_anom_binned(nbins)=MEAN(SH_anom[valid])
   uadv_anom_binned(nbins)=MEAN(uadv_anom[valid])
   vadv_anom_binned(nbins)=MEAN(vadv_anom[valid])
   wadv_anom_binned(nbins)=MEAN(wadv_anom[valid])         
   vsw_anom_binned(nbins)=MEAN(vsw_anom[valid])
   vlw_anom_binned(nbins)=MEAN(vlw_anom[valid]) 
   MSEpdf(nbins)=N_ELEMENTS(valid)
ENDIF ELSE BEGIN
   LH_anom_binned(nbins)=!Values.F_NaN
   SH_anom_binned(nbins)=!Values.F_NaN
   uadv_anom_binned(nbins)=!Values.F_NaN
   vadv_anom_binned(nbins)=!Values.F_NaN   
   wadv_anom_binned(nbins)=!Values.F_NaN
   vsw_anom_binned(nbins)=!Values.F_NaN   
   vlw_anom_binned(nbins)=!Values.F_NaN
   MSEpdf(nbins)=0
ENDELSE
MSEpdf=MSEpdf/FLOAT(TOTAL(MSEpdf))

;valid=where(MSE_nointerp lt bins(0))
;IF TOTAL(valid) ge 0 THEN BEGIN
;   MSEadv_nointerp_binned(0)=MEAN(MSEadv_nointerp[valid])
;ENDIF ELSE $
;   MSEadv_nointerp_binned(0)=!Values.F_NaN
;valid=where(MSE_nointerp ge bins(nbins-1))
;IF TOTAL(valid) ge 0 THEN BEGIN
;   MSEadv_nointerp_binned(nbins)=MEAN(MSEadv_nointerp[valid])
;ENDIF ELSE $
;   MSEadv_nointerp_binned(nbins)=!Values.F_NaN

psfile='/home/users/npklingaman/plots/spcam/MSE/spcam_MSE_anom_binned.'+casename+'.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,MARGIN=3000,XOFFSET=0,XSIZE=17000,CHARSIZE=130
GSET,XMIN=0,XMAX=nbins+1,YMIN=-200,YMAX=200
GPLOT,X=indgen(nbins+1)+0.5,Y=LH_anom_binned,COL=FSC_COLOR('blue'),SYM=3
GPLOT,X=indgen(nbins+1)+0.5,Y=SH_anom_binned,COL=FSC_COLOR('cyan'),SYM=3
GPLOT,X=indgen(nbins+1)+0.5,Y=uadv_anom_binned,COL=FSC_COLOR('purple'),SYM=3,STYLE=1
GPLOT,X=indgen(nbins+1)+0.5,Y=vadv_anom_binned,COL=FSC_COLOR('purple'),SYM=3,STYLE=2
GPLOT,X=indgen(nbins+1)+0.5,Y=wadv_anom_binned,COL=FSC_COLOR('purple'),SYM=3
GPLOT,X=indgen(nbins+1)+0.5,Y=vsw_anom_binned,COL=FSC_COLOR('orange'),SYM=3
GPLOT,X=indgen(nbins+1)+0.5,Y=vlw_anom_binned,COL=FSC_COLOR('red'),SYM=3
GPLOT,X=[0,nbins+1],Y=[0,0],COL=FSC_COLOR('black'),STYLE=1
AXES,XVALS=indgen(nbins+2),XLABELS=["<"+STRMID(STRTRIM(STRING(bins(0)),1),0,5),STRMID(STRTRIM(STRING(bins),1),0,5),$
                                   ">"+STRMID(STRTRIM(STRING(bins(nbins-1)),1),0,5)],YSTEP=25,$
     XTITLE='MSE anom (J/m!U2!N /1E7)',YTITLE='Full field (W/m!U2!N)',/NORIGHT
GSET,XMIN=0,XMAX=nbins+1,YMIN=0,YMAX=0.3
GPLOT,X=indgen(nbins+1)+0.5,Y=MSEpdf,COL=FSC_COLOR('black'),SYM=3,STYLE=2
AXES,YSTEP=0.05,/ONLYRIGHT,NDECS=2,YTITLE='Probability'
GLEGEND,LABELS=['LH','SH','Vudmdx','Vvdmdy','Vomegadmdp','Vsw','Vlw','PDF'],$
        COL=[FSC_COLOR('blue'),FSC_COLOR('cyan'),FSC_COLOR('purple'),FSC_COLOR('purple'),FSC_COLOR('purple'),$
             FSC_COLOR('orange'),FSC_COLOR('red'),FSC_COLOR('black')],STYLE=[0,0,1,2,0,0,0,2],SYM=[3,3,3,3,3,3,3,3],$
        LEGXOFFSET=10000,LEGYOFFSET=13000
PSCLOSE

STOP
END
