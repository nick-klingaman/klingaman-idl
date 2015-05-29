PRO mjo_diabatic_timestep_wbin_twodpdf

metum='/home/ss901165/um_output6/mjodiab_2day/metum'
; Density and vertical velocity
rho_file=metum+'/MetUM.rho.20091020-20100110.lead_12-48hrs.nc'
wap_file=metum+'/MetUM.wap.20091020-20100110.lead_12-48hrs.nc'
; Threshold density-weighted value of pressure vertical velocity for defining convection
;all_thresh=[0.01,0.02,0.03,0.04]
all_thresh_bins=[5+3./14.,5+13./14.,6+9./14.,7+5/14.]
all_thresh=[0.02,0.04,0.06,0.08]
n_thresh=N_ELEMENTS(all_thresh)
; Bins of density-weighted pressure vertical velocity
;wap_bins=[-4.9,-3.5,-2.1,-0.7,0.7,2.1,3.5,4.9,6.3,7.7,9.1,10.5,11.9,13.3]/100.
wap_bins=[-9.8,-7.0,-4.2,-1.4,1.4,4.2,7.0,9.8,12.6,15.4,18.2,21.0,23.8,26.6]/100.
nwap_bins=N_ELEMENTS(wap_bins)

box=[-10,60,10,90]
; Read longitude, latitude, height
lat=OPEN_AND_EXTRACT(rho_file,'latitude')
lon=OPEN_AND_EXTRACT(rho_file,'longitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlon=N_ELEMENTS(lon)
nlat=N_ELEMENTS(lat)
z=OPEN_AND_EXTRACT(rho_file,'level')
nz=50
offset_time=0
nt=180*22

; Read wap and rho
rho=OPEN_AND_EXTRACT(rho_file,'rho',offset=[box_tx(1),box_tx(0),0,offset_time],$
                     count=[nlon,nlat,nz,nt])
wap=OPEN_AND_EXTRACT(wap_file,'wap',offset=[box_tx(1),box_tx(0),0,offset_time],$
                     count=[nlon,nlat,nz,nt])
p=REFORM(OPEN_AND_EXTRACT(metum+'/MetUM.p.20100110.00Z.nc','p',offset=[box_tx(1),box_tx(0),0,0],$
                          count=[nlon,nlat,nz,1]))

; Compute rho-weighted wap (0-48 for troposphere, 20-26 for 3-5km, 27-48 for > 5km)
int_bot=27
int_top=48
wap_intrho=fltarr(nlon,nlat,nt)
p_bot=MEAN(p(*,*,int_bot))/100.
p_top=MEAN(p(*,*,int_top))/100.
FOR i=0,nlon-1 DO BEGIN
   FOR j=0,nlat-1 DO BEGIN
      FOR k=0,nt-1 DO BEGIN
         rhosum=TOTAL(rho(i,j,int_bot:int_top,k))
         wap_intrho(i,j,k)=TOTAL(rho(i,j,int_bot:int_top,k)*wap(i,j,int_bot:int_top,k))/rhosum
      ENDFOR
   ENDFOR
ENDFOR

FOR t=0,n_thresh-1 DO BEGIN
conv_thresh=REFORM(all_thresh(t))
conv_count=0
oned_pdf_prct=fltarr(nwap_bins+1)
twod_pdf_prct=fltarr(nwap_bins+1,nwap_bins+1)
FOR i=0,nlon-1 DO BEGIN
   FOR j=0,nlat-1 DO BEGIN
      ts=REFORM(wap_intrho(i,j,*))
      conv=where(ts ge conv_thresh(0))
      IF TOTAL(conv) gt 0 THEN BEGIN
         conv_count=conv_count+N_ELEMENTS(conv)
         FOR m=0,nwap_bins-2 DO BEGIN
            IF TOTAL(where(ts[conv-1] ge wap_bins(m) and ts[conv-1] lt wap_bins(m+1))) ge 0 THEN BEGIN
               valid=where(ts[conv-1] ge wap_bins(m) and ts[conv-1] lt wap_bins(m+1))
               prev=ts[conv[valid]-1]
               next=ts[conv[valid]+1]
               FOR n=0,nwap_bins-2 DO $
                  IF TOTAL(where(next ge wap_bins(n) and next lt wap_bins(n+1))) ge 0 THEN $
                     twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge wap_bins(n) and next lt wap_bins(n+1)))+$
                  twod_pdf_prct(m+1,n+1)
               IF TOTAL(where(next lt wap_bins(0))) ge 0 THEN $
                  twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt wap_bins(0)))+twod_pdf_prct(m+1,0)
               IF TOTAL(where(next ge wap_bins(nwap_bins-1))) ge 0 THEN $
                  twod_pdf_prct(m+1,nwap_bins)=N_ELEMENTS(where(next ge wap_bins(nwap_bins-1)))+$
                                               twod_pdf_prct(m+1,nwap_bins)
            ENDIF
         ENDFOR
         IF TOTAL(where(ts[conv-1] lt wap_bins(0))) ge 0 THEN BEGIN
            valid=where(ts[conv-1] lt wap_bins(0))
            prev=ts[conv[valid]-1]
            next=ts[conv[valid]+1]               
            FOR n=0,nwap_bins-2 DO $
               IF TOTAL(where(next ge wap_bins(n) and next lt wap_bins(n+1))) ge 0 THEN $
                  twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge wap_bins(n) and next lt wap_bins(n+1)))+$
               twod_pdf_prct(0,n+1)
            IF TOTAL(where(next lt wap_bins(0))) ge 0 THEN $
               twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt wap_bins(0)))+twod_pdf_prct(0,0)
            IF TOTAL(where(next ge wap_bins(nwap_bins-1))) ge 0 THEN $
               twod_pdf_prct(0,nwap_bins)=N_ELEMENTS(where(next ge wap_bins(nwap_bins-1)))+$
                                          twod_pdf_prct(0,nwap_bins)
         ENDIF
         IF TOTAL(where(ts[conv-1] ge wap_bins(nwap_bins-1))) ge 0 THEN BEGIN
            valid=where(ts[conv-1] ge wap_bins(nwap_bins-1))
            prev=ts[conv[valid]-1]
            next=ts[conv[valid]+1]               
            FOR n=0,nwap_bins-2 DO $
               IF TOTAL(where(next ge wap_bins(n) and next lt wap_bins(n+1))) ge 0 THEN $
                  twod_pdf_prct(nwap_bins,n+1)=N_ELEMENTS(where(next ge wap_bins(n) and next lt wap_bins(n+1)))+$
               twod_pdf_prct(nwap_bins,n+1)
            IF TOTAL(where(next lt wap_bins(0))) ge 0 THEN $
               twod_pdf_prct(nwap_bins,0)=N_ELEMENTS(where(next lt wap_bins(0)))+twod_pdf_prct(nwap_bins,0)
            IF TOTAL(where(next ge wap_bins(nwap_bins-1))) ge 0 THEN $
                  twod_pdf_prct(nwap_bins,nwap_bins)=N_ELEMENTS(where(next ge wap_bins(nwap_bins-1)))+$
                                                     twod_pdf_prct(nwap_bins,nwap_bins)
         ENDIF
      ENDIF
   ENDFOR
ENDFOR

;oned_pdf=oned_pdf_prct/TOTAL(oned_pdf_prct)
twod_pdf_prct=twod_pdf_prct/TOTAL(twod_pdf_prct)

mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_twodpdf.metum_ga3_prct.thresh'+$
       STRMID(STRTRIM(STRING(conv_thresh),1),0,5)+'_intlev'+STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,XSIZE=15000,YSIZE=15000,SPACE2=1000,YOFFSET=2000,XOFFSET=2000,SPACE1=400
GSET,XMIN=0,XMAX=nwap_bins+1,YMIN=0,YMAX=nwap_bins+1
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
LEVS,MANUAL=mylevs_prct
CON,X=indgen(nwap_bins+1)+0.5,Y=indgen(nwap_bins+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,$
    CB_TITLE='Probability',/CB_RIGHT ;,TITLE=psfile_title
GPLOT,X=[0,nwap_bins+1],Y=[0,nwap_bins+1],STYLE=1
GPLOT,X=[4.5,4.5],Y=[0,nwap_bins+1],STYLE=1
GPLOT,X=[0,nwap_bins+1],Y=[4.5,4.5],STYLE=1
GPLOT,X=[all_thresh_bins(t),all_thresh_bins(t)],Y=[0,nwap_bins+1],STYLE=2
GPLOT,X=[0,nwap_bins+1],Y=[all_thresh_bins(t),all_thresh_bins(t)],STYLE=2
GPLOT,X=all_thresh_bins(t)-3,Y=all_thresh_bins(t)-3,$
      TEXT=STRMID(STRTRIM(STRING(TOTAL(twod_pdf_prct(0:all_thresh_bins(t)-1,0:all_thresh_bins(t)))),1),0,5),$
      COL=FSC_COLOR('black'),CHARSIZE=150
GPLOT,X=all_thresh_bins(t)+3,Y=all_thresh_bins(t)+3,$
      TEXT=STRMID(STRTRIM(STRING(TOTAL(twod_pdf_prct(all_thresh_bins(t):nwap_bins,all_thresh_bins(t):nwap_bins))),1),0,5),$
      COL=FSC_COLOR('black'),CHARSIZE=150
GPLOT,X=all_thresh_bins(t)-3,Y=all_thresh_bins(t)+3,$
      TEXT=STRMID(STRTRIM(STRING(TOTAL(twod_pdf_prct(0:all_thresh_bins(t)-1,all_thresh_bins(t):nwap_bins))),1),0,5),$
      COL=FSC_COLOR('black'),CHARSIZE=150
GPLOT,X=all_thresh_bins(t)+3,Y=all_thresh_bins(t)-3,$
      TEXT=STRMID(STRTRIM(STRING(TOTAL(twod_pdf_prct(all_thresh_bins(t):nwap_bins,0:all_thresh_bins(t)-1))),1),0,5),$
      COL=FSC_COLOR('black'),CHARSIZE=150

labels=strarr(nwap_bins+2)
FOR m=1,nwap_bins DO BEGIN
   IF wap_bins(m-1) lt 0 THEN BEGIN
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m-1)),1),0,6)
   ENDIF ELSE $
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m-1)),1),0,5)
ENDFOR
labels(0)='<'+STRMID(STRTRIM(STRING(wap_bins(0)),1),0,6)
labels(nwap_bins+1)='>'+STRMID(STRTRIM(STRING(wap_bins(nwap_bins-1)),1),0,5)
AXES,XVALS=indgen(nwap_bins+2),YVALS=indgen(nwap_bins+2),YLABELS=labels,XLABELS=labels,$
     XTITLE='W at timestep t-1 (Pa s!U-1!N)',YTITLE='W at timestep t+1 (Pa s!U-1!N)',$ ;,/NORIGHT,$
     ORIENTATION=35
GPLOT,X=-4.0,Y=-3.3,TEXT='W integrated over levels '+STRTRIM(STRING(int_bot+1),1)+' to '+STRTRIM(STRING(int_top+1),1)+$
      ', hybrid height '+STRTRIM(STRING(z(int_bot)),1)+' to '+STRTRIM(STRING(z(int_top)),1)+', pressure '+$
      STRMID(STRTRIM(STRING(p_bot),1),0,4)+' to '+STRMID(STRTRIM(STRING(p_top),1),0,4)+' hPa',ALIGN=0.0
GPLOT,X=-4.0,Y=-4.0,TEXT='Convection threshold: '+STRMID(STRTRIM(STRING(conv_thresh),1),0,5)+' (Pa s!U-1!N)',ALIGN=0.0
GPLOT,X=-4.0,Y=-4.7,TEXT='Threshold is exceeded on '+$
      STRMID(STRTRIM(STRING(FLOAT(conv_count)/(FLOAT(nlon)*FLOAT(nlat)*FLOAT(nt))*100),1),0,6)+'% of timesteps',ALIGN=0.0

;GSET,XMIN=0,XMAX=nwap_bins+1,YMIN=0.001,YMAX=1,/YLOG
;IF TOTAL(where(oned_pdf_prct lt 0.001)) ge 0 THEN $
;   oned_pdf_prct[where(oned_pdf_prct lt 0.001)]=!Values.F_NaN
;GPLOT,X=indgen(nwap_bins+1)+0.5,Y=oned_pdf_prct,STYLE=2,SYM=3
;AXES,YVALS=['1.0e-3','1.4e-3','2.0e-3','3.0e-3','4.5e-3','7.0e-3','1.0e-2','1.4e-2','2.0e-2','3.0e-2','4.5e-2',$
;            '7.0e-2','1.0e-1','1.4e-1','2.0e-1','3.0e-1','4.5e-1','7.0e-1','1.0e+0'],$
;     YTITLE='Probability of precipitation in bin',/ONLYRIGHT,NDECS=2
PSCLOSE
ENDFOR

STOP
END
