PRO spcam_MSE_check_interpZ3

pt=[0,80]
ntime=4380

bins=[3.25,3.26,3.27,3.28,3.29,3.30,3.31,3.32,3.33,3.34,3.35,3.36,3.37,3.38]
nbins=N_ELEMENTS(bins)

basedir='/group_workspaces/jasmin2/klingaman/cam/spcam_iceedge_free_spccsm_elnino/h1'
MSE_interp_file=basedir+'/MSE_interpZ3.nc'
MSEadv_interp_file=basedir+'/MSEadv_interpZ3.nc'
MSE_nointerp_file=basedir+'/MSE_nointerpZ3.nc'
MSEadv_nointerp_file=basedir+'/MSEadv_nointerpZ3.nc'

latitude=OPEN_AND_EXTRACT(MSE_interp_file,'lat')
longitude=OPEN_AND_EXTRACT(MSE_interp_file,'lon')
latpt=NEAREST(latitude,pt(0))
lonpt=NEAREST(longitude,pt(1))

; Read MSE
MSE_interp=REFORM(OPEN_AND_EXTRACT(MSE_interp_file,'MSEint',offset=[lonpt,latpt,0],count=[1,1,ntime]))/1E9
MSE_nointerp=REFORM(OPEN_AND_EXTRACT(MSE_nointerp_file,'MSEint',offset=[lonpt,latpt,0],count=[1,1,ntime]))/1E9
MSEadv_interp=REFORM(OPEN_AND_EXTRACT(MSEadv_interp_file,'Vomegadmdp',offset=[lonpt,latpt,0],count=[1,1,ntime]))
MSEadv_nointerp=REFORM(OPEN_AND_EXTRACT(MSEadv_nointerp_file,'Vomegadmdp',offset=[lonpt,latpt,0],count=[1,1,ntime]))

MSEadv_interp_binned=fltarr(nbins+1)
MSEadv_nointerp_binned=fltarr(nbins+1)
FOR i=1,nbins-1 DO BEGIN
   valid=where(MSE_interp ge bins(i-1) and MSE_interp lt bins(i))
   IF TOTAL(valid) ge 0 THEN BEGIN
      MSEadv_interp_binned(i)=MEAN(MSEadv_interp[valid])
   ENDIF ELSE $
      MSEadv_interp_binned(i)=!Values.F_NaN
   valid=where(MSE_nointerp ge bins(i-1) and MSE_nointerp lt bins(i))
   IF TOTAL(valid) ge 0 THEN BEGIN
      MSEadv_nointerp_binned(i)=MEAN(MSEadv_nointerp[valid])
   ENDIF ELSE $
      MSEadv_nointerp_binned(i)=!Values.F_NaN
ENDFOR
valid=where(MSE_interp lt bins(0))
IF TOTAL(valid) ge 0 THEN BEGIN
   MSEadv_interp_binned(0)=MEAN(MSEadv_interp[valid])
ENDIF ELSE $
   MSEadv_interp_binned(0)=!Values.F_NaN
valid=where(MSE_interp ge bins(nbins-1))
IF TOTAL(valid) ge 0 THEN BEGIN
   MSEadv_interp_binned(nbins)=MEAN(MSEadv_interp[valid])
ENDIF ELSE $
   MSEadv_interp_binned(nbins)=!Values.F_NaN

valid=where(MSE_nointerp lt bins(0))
IF TOTAL(valid) ge 0 THEN BEGIN
   MSEadv_nointerp_binned(0)=MEAN(MSEadv_nointerp[valid])
ENDIF ELSE $
   MSEadv_nointerp_binned(0)=!Values.F_NaN
valid=where(MSE_nointerp ge bins(nbins-1))
IF TOTAL(valid) ge 0 THEN BEGIN
   MSEadv_nointerp_binned(nbins)=MEAN(MSEadv_nointerp[valid])
ENDIF ELSE $
   MSEadv_nointerp_binned(nbins)=!Values.F_NaN

psfile='/home/users/npklingaman/plots/spcam/MSE/spcam_MSE_check_interpZ3.0N_80E.ps'
PSOPEN,file=psfile,FONT=6,TFONT=6,MARGIN=3000,XOFFSET=1000
GSET,XMIN=0,XMAX=nbins+1,YMIN=-100,YMAX=100
GPLOT,X=indgen(nbins+1)+0.5,Y=MSEadv_interp_binned,COL=FSC_COLOR('red'),SYM=3
GPLOT,X=indgen(nbins)+0.5,Y=MSEadv_nointerp_binned,COL=FSC_COLOR('blue'),SYM=3
AXES,XVALS=indgen(nbins+2),XLABELS=["<"+STRMID(STRTRIM(STRING(bins(0)),1),0,5),STRMID(STRTRIM(STRING(bins),1),0,5),$
                                   ">"+STRMID(STRTRIM(STRING(bins(nbins-1)),1),0,5)],YSTEP=10,$
     XTITLE='MSE (/1E9)',YTITLE='Vomegadmdp'
GLEGEND,COL=[FSC_COLOR('red'),FSC_COLOR('blue')],LABELS=['Interp Z3','Daily Z3'],LEGPOS=11,SYM=[3,3]
PSCLOSE

STOP
END
