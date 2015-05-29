PRO mjo_diabatic_timestep_wbin_cumulative

metum='/home/ss901165/um_output6/mjodiab_2day/metum'
; Density and vertical velocity
rho_file=metum+'/MetUM.rho.20091020-20100110.lead_12-48hrs.nc'
wap_file=metum+'/MetUM.wap.20091020-20100110.lead_12-48hrs.nc'
; Threshold density-weighted value of pressure vertical velocity for defining convection
;all_thresh=[0.01,0.02,0.03,0.04]
;all_thresh_bins=[5+3./14.,5+13./14.,6+9./14.,7+5/14.]
;all_thresh=[0.02,0.04,0.06,0.08]
n_thresh=N_ELEMENTS(all_thresh)
; Bins of density-weighted pressure vertical velocity
wap_bins=['-2.7','-2.1','-1.5','-0.9','-0.3','0.3','0.9','1.5','2.1','2.7','3.3','3.9','4.5','5.1','5.7','6.3']/100.
;wap_bins=['-5.4','-4.2','-3.0','-1.8','-0.6','0.6','1.8','3.0','4.2','5.4','6.6','7.8','9.0','10.2','11.4','12.6']/100.
;wap_bins=[-4.9,-3.5,-2.1,-0.7,0.7,2.1,3.5,4.9,6.3,7.7,9.1,10.5,11.9,13.3]/100.
;wap_bins=[-9.8,-7.0,-4.2,-1.4,1.4,4.2,7.0,9.8,12.6,15.4,18.2,21.0,23.8,26.6]/100.
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
int_bot=0
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

oned_pdf_prct=fltarr(nwap_bins+1)
twod_pdf_prct=fltarr(nwap_bins+1,nwap_bins+1)
FOR i=0,nlon-1 DO BEGIN
   FOR j=0,nlat-1 DO BEGIN
      ts=REFORM(wap_intrho(i,j,*))
      FOR m=0,nwap_bins-1 DO BEGIN
         IF TOTAL(where(ts ge wap_bins(m))) ge 0 THEN BEGIN
            valid=where(ts ge wap_bins(m))
            next=ts[valid+1]
            FOR n=0,nwap_bins-1 DO $
               IF TOTAL(where(next ge wap_bins(n))) ge 0 THEN $
                  twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge wap_bins(n)))+twod_pdf_prct(m+1,n+1)
            IF TOTAL(where(next lt wap_bins(0))) ge 0 THEN $
               twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt wap_bins(0)))+twod_pdf_prct(m+1,0)
         ENDIF
      ENDFOR
      IF TOTAL(where(ts lt wap_bins(0))) ge 0 THEN BEGIN
         valid=where(ts lt wap_bins(0))
         next=ts[valid+1]
         FOR n=0,nwap_bins-1 DO $
            IF TOTAL(where(next ge wap_bins(n))) ge 0 THEN $
               twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge wap_bins(n)))+twod_pdf_prct(0,n+1)
         IF TOTAL(where(next lt wap_bins(0))) ge 0 THEN $
            twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt wap_bins(0)))+twod_pdf_prct(0,0)
      ENDIF
   ENDFOR
ENDFOR

FOR m=0,nwap_bins DO $
   oned_pdf_prct(m)=twod_pdf_prct(1,m)/FLOAT(TOTAL(twod_pdf_prct(1,1)))
FOR m=0,nwap_bins DO $
   twod_pdf_prct(m,*)=twod_pdf_prct(m,*)/FLOAT(twod_pdf_prct(m,1))

mylevs_prct=['1.0e-2','1.5e-2','2.0e-2','2.5e-2','3.0e-2','4.0e-2','5.0e-2','6.5e-2','8.0e-2',$
             '1.0e-1','1.5e-1','2.0e-1','2.5e-1','3.0e-1','4.0e-1','5.0e-1','6.5e-1','8.0e-1','9.9e-1']
psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_cumulative.metum_ga3_prct.intlev'+$
       STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,XSIZE=15000,YSIZE=15000,SPACE2=4500,YOFFSET=2000,XOFFSET=2000,SPACE1=400
GSET,XMIN=0,XMAX=nwap_bins,YMIN=0,YMAX=nwap_bins
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
LEVS,MANUAL=mylevs_prct
CON,X=indgen(nwap_bins)+0.5,Y=indgen(nwap_bins)+0.5,FIELD=twod_pdf_prct(1:nwap_bins,1:nwap_bins),/BLOCK,/NOLINES,CB_WIDTH=135,$
    CB_TITLE='Probability',/CB_RIGHT ;,TITLE=psfile_title
labels=strarr(nwap_bins+1)
FOR m=0,nwap_bins-1 DO BEGIN
   IF wap_bins(m) lt 0 THEN BEGIN
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m)),1),0,6)
   ENDIF ELSE $
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m)),1),0,5)
ENDFOR
GPLOT,X=[0,nwap_bins],Y=[0,nwap_bins],STYLE=1
GPLOT,X=[5.0,5.0],Y=[0,nwap_bins],STYLE=1
GPLOT,X=[0,nwap_bins],Y=[5.0,5.0],STYLE=1
AXES,XVALS=indgen(nwap_bins)+0.5,YVALS=indgen(nwap_bins)+0.5,YLABELS=labels,XLABELS=labels,$
     XTITLE='W!Dt!N (Pa s!U-1!N) greater than ',YTITLE='W!Dt+1!N (Pa s!U-1!N) greater than',$ ;,/NORIGHT,$
     ORIENTATION=30,/NORIGHT
GSET,XMIN=0,XMAX=nwap_bins,YMIN=0,YMAX=1
toplot=fltarr(nwap_bins)
FOR m=0,nwap_bins-1 DO $
   toplot(m)=twod_pdf_prct(m+1,m+1)
GPLOT,X=indgen(nwap_bins)+0.5,Y=toplot,STYLE=2,SYM=3
AXES,YSTEP=0.1,YMINOR=0.05,YTITLE='Probability of W!Dt+1!N greater than or equal to W!Dt!N',/ONLYRIGHT,NDECS=2
PSCLOSE

mylevs_ratio=['1.00','1.10','1.25','1.50','2.00','3.00','5.00','7.00','10.00','15.00','20.00','30.00','50.00']
psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_cumulative.metum_ga3_ratio.intlev'+$
       STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,XSIZE=15000,YSIZE=15000,SPACE2=500,YOFFSET=2000,XOFFSET=2000,SPACE1=400
GSET,XMIN=0,XMAX=nwap_bins,YMIN=0,YMAX=nwap_bins
CS,SCALE=25,NCOLS=N_ELEMENTS(mylevs_ratio)+1,white=[2]
LEVS,MANUAL=mylevs_ratio
toplot=fltarr(nwap_bins,nwap_bins)
FOR m=1,nwap_bins DO $
   toplot(0:nwap_bins-1,m-1)=twod_pdf_prct(1:nwap_bins,m)/oned_pdf_prct(m)
CON,X=indgen(nwap_bins)+0.5,Y=indgen(nwap_bins)+0.5,FIELD=toplot,/BLOCK,/NOLINES,CB_WIDTH=135,$
    CB_TITLE='Ratio',/CB_RIGHT ;,TITLE=psfile_title
labels=strarr(nwap_bins+1)
FOR m=0,nwap_bins-1 DO BEGIN
   IF wap_bins(m) lt 0 THEN BEGIN
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m)),1),0,6)
   ENDIF ELSE $
      labels(m)=STRMID(STRTRIM(STRING(wap_bins(m)),1),0,5)
ENDFOR
GPLOT,X=[0,nwap_bins],Y=[0,nwap_bins],STYLE=1
GPLOT,X=[5.0,5.0],Y=[0,nwap_bins],STYLE=1
GPLOT,X=[0,nwap_bins],Y=[5.0,5.0],STYLE=1
AXES,XVALS=indgen(nwap_bins)+0.5,YVALS=indgen(nwap_bins)+0.5,YLABELS=labels,XLABELS=labels,$
     XTITLE='W!Dt!N (Pa s!U-1!N) greater than ',YTITLE='W!Dt+1!N (Pa s!U-1!N) greater than',$ ;,/NORIGHT,$
     ORIENTATION=25
PSCLOSE

STOP
END
