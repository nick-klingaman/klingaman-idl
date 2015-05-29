PRO mjo_diabatic_precip_bin_qanom_patterncorr

; Valid pressure levels for pattern correlation
;plevs=[1000,950,900,850,800,700,600,500,400,300,250,200,150,100,50]
plevs=[1000,900,800,700,600,500,400,300,200,100]
np=N_ELEMENTS(plevs)

; ECMWF YoTC file
ecmwf_yotc_file='/home/ss901165/um_output6/mjodiab_20day/ECMWF_YOTC_TRMM_qanom_byprecip_IndoPac.nc'
bin_offset=0
bin_count=16
bins=OPEN_AND_EXTRACT(ecmwf_yotc_file,'precip',offset=[bin_offset],count=[bin_count])
nbins=N_ELEMENTS(bins)

yotc_p=OPEN_AND_EXTRACT(ecmwf_yotc_file,'p')
yotc_np=N_ELEMENTS(yotc_p)
yotc_temp=OPEN_AND_EXTRACT(ecmwf_yotc_file,'qanom',offset=[bin_offset,0],count=[bin_count,yotc_np])
yotc_qanom_net=fltarr(nbins,np)
p=0
FOR i=yotc_np-1,1,-1 DO BEGIN
   IF p lt np THEN BEGIN
      IF FLOOR(yotc_p(i)) eq plevs(p) THEN BEGIN
         yotc_qanom_net(*,p)=yotc_temp(*,i)
         p=p+1
      ENDIF
   ENDIF
ENDFOR

n_models=14
mjodiab_20day='/home/ss901165/um_output6/mjodiab_20day'
mjodiab_20year='/home/ss901165/um_output6/mjodiab_20year'

pcorr=fltarr(n_models)
pregress=fltarr(n_models)
all_codes=strarr(n_models)
all_colors=strarr(n_models)
all_fidelity=intarr(n_models)

FOR m=0,n_models-1 DO BEGIN
   print,m
   CASE m OF
      0 : BEGIN
         infile=mjodiab_20day+'/cam5zm/CAM5ZMMicroCAPT_qanom_byprecip_IndoPac.nc'
         all_codes(m)='CZ'
         all_colors(m)='forestgreen'
         p_offset=0
         a=1.5
         all_fidelity(m)=20
      END      
      1 : BEGIN
         infile=mjodiab_20day+'/cam5/NCAR.CAM5_qanom_byprecip_IndoPac.nc'
         all_codes(m)='C5'
         all_colors(m)='forestgreen'
         p_offset=0
         a=1.5
         all_fidelity(m)=20
      END
;      1 : BEGIN
;         infile=mjodiab_20year+'/cam5zm/CAM5ZMMicroCAPT_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='CZ'
;         all_colors(m)='orangered'
;         a=0.9
;      END      
      2 : BEGIN
         infile=mjodiab_20day+'/cancm4/CanCM4_qanom_byprecip_IndoPac.nc'
         all_codes(m)='CC'
         all_colors(m)='firebrick'
         p_offset=0
         a=1
         all_fidelity(m)=6
      END
;      3 : BEGIN
;         infile=mjodiab_20year+'/cancm4/CanCM4_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='CC'
;         all_colors(m)='firebrick'
;         a=1
;      END
      3 : BEGIN
         infile=mjodiab_20day+'/cnrm_atmos/CNRM_qanom_byprecip_IndoPac.nc'
         all_codes(m)='CN'
         all_colors(m)='orangered'
         a=0.4
         all_fidelity(m)=15
      END
;      5 : BEGIN
;         infile=mjodiab_20year+'/cnrm_atmos/CNRM_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='CN'
;         all_colors(m)='orangered'
;         a=0.4
;      END
      4 : BEGIN
         infile=mjodiab_20day+'/ecearth/ecearth3_qanom_byprecip_IndoPac.nc'
         all_codes(m)='E3'
         all_colors(m)='orangered'
         p_offset=0
         a=0.8
         all_fidelity(m)=16
      END
;      7 : BEGIN
;         infile=mjodiab_20year+'/ecearth/ecearth3_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='E3'
;         all_colors(m)='orangered'
;         p_offset=0
;         a=0.8
;      END
      5 : BEGIN
         infile=mjodiab_20day+'/giss/ModelE_qanom_byprecip_IndoPac.nc'
         all_codes(m)='GI'
         all_colors(m)='orangered'
         p_offset=0
         a=1.0
         all_fidelity(m)=16
      END
;      9 : BEGIN
;         infile=mjodiab_20year+'/giss/ModelE_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='GI'
;         all_colors(m)='forestgreen'
;         a=1.2
;      END
      6 : BEGIN
         infile=mjodiab_20day+'/metum/MetUM_qanom_byprecip_IndoPac.nc'
         all_codes(m)='MO'
         all_colors(m)='orangered'
         a=0.7
         all_fidelity(m)=16
      END
;      11 : BEGIN
;         infile=mjodiab_20year+'/metum/MetUM_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='MO'
;         all_colors(m)='firebrick'
;         a=0.5
;      END
      7 : BEGIN
         infile=mjodiab_20day+'/miroc/miroc5_qanom_byprecip_IndoPac.nc'
         all_codes(m)='MI'
         all_colors(m)='firebrick'
         a=0.7
         all_fidelity(m)=14
      END
;      13 : BEGIN
;         infile=mjodiab_20year+'/miroc/miroc5_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='MI'
;         all_colors(m)='firebrick'
;         a=0.7
;      END      
      8 : BEGIN
         infile=mjodiab_20day+'/mri/MRI-AGCM_qanom_byprecip_IndoPac.nc'
         all_codes(m)='MR'
         all_colors(m)='orangered'
         a=1.3
         p_offset=0
         all_fidelity(m)=16
      END
;      15 : BEGIN
;         infile=mjodiab_20year+'/mri/MRI-AGCM_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='MR'
;         all_colors(m)='forestgreen'
;         a=1.5
;         p_offset=0
;      END
      9 : BEGIN
         infile=mjodiab_20day+'/nasa/GEOS5_AGCM_qanom_byprecip_IndoPac.nc'
         all_codes(m)='NA'
         all_colors(m)='forestgreen'
         a=1.7
         p_offset=0
         all_fidelity(m)=18
      END
      10 : BEGIN
         infile=mjodiab_20day+'/ecmwf/ECMWF_IFS_qanom_byprecip_IndoPac.nc'
         all_codes(m)='EC'
         all_colors(m)='forestgreen'
         a=1.0
         all_fidelity(m)=18
      END
      11 : BEGIN
         infile=mjodiab_20day+'/nicam/nicam_qanom_byprecip_IndoPac.nc'
         all_codes(m)='NI'
         all_colors(m)='black'
         a=1.0
         all_fidelity(m)=0
      END
      12 : BEGIN
         infile=mjodiab_20day+'/nrl/NGEM01_qanom_byprecip_IndoPac.nc'
         all_codes(m)='NR'
         all_colors(m)='firebrick'
         a=1.0
         all_fidelity(m)=10
      END
      13 : BEGIN
         infile=mjodiab_20day+'/spcam/SPCAM3.0_qanom_byprecip_IndoPac.nc'
         all_codes(m)='SP'
         all_colors(m)='firebrick'
         a=1.0
         all_fidelity(m)=12
      END
;      17 : BEGIN
;         infile=mjodiab_20year+'/nasa/GEOS5_AGCM_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='NA'
;         all_colors(m)='orangered'
;         a=0.8
;      END      
;      18 : BEGIN
;         infile=mjodiab_20day+'/cam5zm/CAM5ZMMicroCAPT_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='CZ'
;         all_colors(m)='forestgreen'
;         p_offset=0
;         a=1.5
;      END
;      19 : BEGIN
;         infile=mjodiab_20year+'/cam5_short/NCAR-CAM5_qanom_byprecip_IndoPac.nc'
;         all_codes(m)='C5'
;         all_colors(m)='orangered'
;         a=0.8
;      END
   ENDCASE
   
   ;print,'--------- ',m,'-------------'
   a=1.0

   ; Read composite increments
   model_p=OPEN_AND_EXTRACT(infile,'p')+p_offset
   model_np=N_ELEMENTS(model_p)   
   model_temp=OPEN_AND_EXTRACT(infile,'qanom',offset=[bin_offset,0],count=[bin_count,model_np])
   model_qanom_net=fltarr(nbins,np)
   p=0
   print,model_p
   FOR i=0,model_np-1 DO BEGIN
      IF p lt np THEN BEGIN
         IF FLOOR(model_p(i)) eq plevs(p) and p lt np THEN BEGIN
            print,plevs(p),model_p(i)
            model_qanom_net(*,p)=model_temp(*,i)
            p=p+1
         ENDIF
      ENDIF
   ENDFOR
   
   pregress(m)=REGRESS(REFORM(yotc_qanom_net,[np*nbins]),REFORM(model_qanom_net,[np*nbins]))*a
   pcorr(m)=CORRELATE(REFORM(yotc_qanom_net,[np*nbins]),REFORM(model_qanom_net,[np*nbins]))*a

ENDFOR

FOR m=0,n_models-1 DO $
   print,all_codes(m),pcorr(m),pregress(m)

psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_qanom_patterncorr.hindcast_models_corr.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,YOFFSET=1000,XOFFSET=1500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.75,XMAX=1.0,YMIN=0,YMAX=22
FOR m=0,n_models-1 DO BEGIN
   GPLOT,X=pcorr(m),Y=all_fidelity(m),SYM=3,/NOLINES
   IF all_codes(m) eq 'EC' THEN BEGIN
      GPLOT,X=pcorr(m)+0.01,Y=all_fidelity(m)-0.3,TEXT=all_codes(m),COL=FSC_COLOR(all_colors(m))
   ENDIF ELSE $
      GPLOT,X=pcorr(m),Y=all_fidelity(m)+0.5,TEXT=all_codes(m),COL=FSC_COLOR(all_colors(m))
ENDFOR
GPLOT,X=[0,0],Y=[0,22],STYLE=1
; Switch to n_models-2 to eliminate NICAM from regression/correlation, once all models are added
coeff=REGRESS(pcorr(0:n_models-2),all_fidelity(0:n_models-2),CONST=constant)
corr=CORRELATE(pcorr(0:n_models-2),all_fidelity(0:n_models-2))
GPLOT,X=0.78,Y=9,TEXT='r='+STRMID(STRTRIM(STRING(corr),1),0,4)
GPLOT,X=[0.75,1.0],Y=[0.75,1.0]*coeff(0)+constant,STYLE=2
AXES,XSTEP=0.05,XMINOR=0.01,YSTEP=4,YMINOR=1,YTITLE='Lead of RMM bivariate correlation < 0.7 (days)',$
     XTITLE='Pattern correlation of specific humidity diagnostic',NDECS=2
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_qanom_patterncorr.hindcast_models_regress.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=160,MARGIN=2000,YOFFSET=1000,XOFFSET=1500,XSIZE=16000,YSIZE=16000
GSET,XMIN=0.7,XMAX=1.5,YMIN=0,YMAX=22
FOR m=0,n_models-1 DO BEGIN
   GPLOT,X=pregress(m),Y=all_fidelity(m),SYM=3,/NOLINES
   GPLOT,X=pregress(m),Y=all_fidelity(m)+0.5,TEXT=all_codes(m),COL=FSC_COLOR(all_colors(m))
ENDFOR
GPLOT,X=[0,0],Y=[0,22],STYLE=1
coeff=REGRESS(pregress(0:n_models-1),all_fidelity(0:n_models-1),CONST=constant)
corr=CORRELATE(pregress(0:n_models-1),all_fidelity(0:n_models-1))
GPLOT,X=0.9,Y=10,TEXT='r='+STRMID(STRTRIM(STRING(corr),1),0,4)
GPLOT,X=[0.7,1.5],Y=[0.7,1.5]*coeff(0)+constant,STYLE=2
AXES,XSTEP=0.2,XMINOR=0.05,YSTEP=4,YMINOR=1,YTITLE='Lead of RMM bivariate correlation < 0.7 (days)',$
     XTITLE='Pattern regression of specific humidity diagnostic',NDECS=1
PSCLOSE

; psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_qanom_patterncorr.common_models_corr.ps'
; PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,YOFFSET=1000,XOFFSET=1500,XSIZE=16000,YSIZE=16000
; GSET,XMIN=-0.1,XMAX=0.8,YMIN=-0.1,YMAX=0.8
; FOR m=0,n_models-1,2 DO BEGIN
;    IF m ne 18 THEN BEGIN
;       GPLOT,X=pcorr(m)-0.012,Y=pcorr(m+1)+0.015,TEXT=STRMID(all_codes(m),0,1),ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
; ;   GPLOT,X=pcorr(m),Y=pcorr(m+1)+0.02,TEXT='/',ALIGN=0.5
;       GPLOT,X=pcorr(m)+0.012,Y=pcorr(m+1)+0.015,TEXT=STRMID(all_codes(m+1),1,1),ALIGN=0.5,COL=FSC_COLOR(all_colors(m+1))
;    ENDIF ELSE BEGIN
;       GPLOT,X=pcorr(m)-0.03,Y=pcorr(m+1)+0.015,TEXT='CZ',ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
;       GPLOT,X=pcorr(m),Y=pcorr(m+1)+0.015,TEXT='/',ALIGN=0.5
;       GPLOT,X=pcorr(m)+0.03,Y=pcorr(m+1)+0.015,TEXT='C5',ALIGN=0.5,COL=FSC_COLOR(all_colors(m+1))
;    ENDELSE
;    GPLOT,X=pcorr(m),Y=pcorr(m+1),SYM=3
;    ;print,all_codes(m),pcorr(m),pcorr(m+1),pregress(m),pregress(m+1)
; ENDFOR
; GPLOT,X=[-0.1,0.8],Y=[-0.1,0.8],STYLE=1
; GPLOT,X=[-0.1,0.8],Y=[0,0],STYLE=1
; GPLOT,X=[0,0],Y=[-0.1,0.8],STYLE=1
; AXES,XSTEP=0.1,YSTEP=0.1,XMINOR=0.05,YMINOR=0.05,XTITLE='Pattern correlation for 20-day hindcasts',$
;      YTITLE='Pattern correlation for 20-year climate simulations',NDECS=2
; PSCLOSE

; psfile='/home/ss901165/idl/mjo_diabatic/precip_bin/mjo_diabatic_precip_bin_qanom_patterncorr.common_models_regress.ps'
; PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=160,MARGIN=2000,YOFFSET=1000,XOFFSET=1500,XSIZE=16000,YSIZE=16000
; GSET,XMIN=-0.1,XMAX=0.8,YMIN=-0.1,YMAX=0.8
; FOR m=0,n_models-1,2 DO BEGIN
;    IF m ne 18 THEN BEGIN
;       GPLOT,X=pregress(m)-0.012,Y=pregress(m+1)+0.015,TEXT=STRMID(all_codes(m),0,1),ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
; ;   GPLOT,X=pregress(m),Y=pregress(m+1)+0.02,TEXT='/',ALIGN=0.5
;       GPLOT,X=pregress(m)+0.012,Y=pregress(m+1)+0.015,TEXT=STRMID(all_codes(m+1),1,1),ALIGN=0.5,COL=FSC_COLOR(all_colors(m+1))
;    ENDIF ELSE BEGIN
;       GPLOT,X=pregress(m)-0.03,Y=pregress(m+1)+0.015,TEXT='CZ',ALIGN=0.5,COL=FSC_COLOR(all_colors(m))
;       GPLOT,X=pregress(m),Y=pregress(m+1)+0.015,TEXT='/',ALIGN=0.5
;       GPLOT,X=pregress(m)+0.03,Y=pregress(m+1)+0.015,TEXT='C5',ALIGN=0.5,COL=FSC_COLOR(all_colors(m+1))
;    ENDELSE
;    GPLOT,X=pregress(m),Y=pregress(m+1),SYM=3
      
; ENDFOR
; GPLOT,X=[-0.1,0.8],Y=[-0.1,0.8],STYLE=1
; GPLOT,X=[-0.1,0.8],Y=[0,0],STYLE=1
; GPLOT,X=[0,0],Y=[-0.1,0.8],STYLE=1
; AXES,XSTEP=0.1,YSTEP=0.1,XMINOR=0.05,YMINOR=0.05,XTITLE='Pattern regression for 20-day hindcasts',$
;      YTITLE='Pattern regression for 20-year climate simulations',NDECS=2
; PSCLOSE

STOP
END

