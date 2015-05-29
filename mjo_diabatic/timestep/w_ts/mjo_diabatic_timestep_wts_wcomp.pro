PRO mjo_diabatic_timestep_wts_wcomp

box=[-9.5,60,9.5,90]
;box=[-5,65,5,70]
metum='/home/ss901165/um_output6/mjodiab_2day/metum'
wap_file=metum+'/MetUM.wap.20091020-20100110.lead_12-48hrs.nc'
pr_file=metum+'/MetUM.pr.20091020-20100110.lead_12-48hrs.nc'
p_file=metum+'/MetUM.p.20100110.00Z.nc'
pr_thresh=2
nt=180*22
nz=50

wap_lon=OPEN_AND_EXTRACT(wap_file,'longitude')
wap_lat=OPEN_AND_EXTRACT(wap_file,'latitude')
DEFINE_BOUNDARIES,box,wap_lat,wap_lon,wap_box_tx,/LIMIT
wap_nlat=N_ELEMENTS(wap_lat)
wap_nlon=N_ELEMENTS(wap_lon)

pr_lon=OPEN_AND_EXTRACT(pr_file,'longitude')
pr_lat=OPEN_AND_EXTRACT(pr_file,'latitude')
DEFINE_BOUNDARIES,box,pr_lat,pr_lon,pr_box_tx,/LIMIT
pr_nlat=N_ELEMENTS(pr_lat)
pr_nlon=N_ELEMENTS(pr_lon)

wap=OPEN_AND_EXTRACT(wap_file,'wap',offset=[wap_box_tx(1),wap_box_tx(0),0,0],$
                     count=[wap_nlon,wap_nlat,nz,nt])
pr=OPEN_AND_EXTRACT(pr_file,'pr',offset=[pr_box_tx(1),pr_box_tx(0),0],$
                    count=[pr_nlon,pr_nlat,nt])*86400.
pressure=REFORM(OPEN_AND_EXTRACT(p_file,'p',$
                                 offset=[wap_box_tx(1),wap_box_tx(0),0,0],count=[1,1,nz,1]))*0.01

lags=indgen(13)-6
nlags=N_ELEMENTS(lags)
wap_comp=fltarr(nlags,nz)
wap_comp_nothresh=fltarr(nlags,nz)
sample_size=lonarr(nlags)
sample_size_nothresh=LONG(0)
FOR i=0,wap_nlon-1 DO BEGIN
   FOR j=0,wap_nlat-1 DO BEGIN
      pr_ts=REFORM(pr(i,j,*))
      pr_mask=fltarr(nt)
      pr_mask[where(pr_ts ge pr_thresh)]=1
      pr_mask[where(pr_ts lt pr_thresh)]=0
      wap_ts=REFORM(wap(i,j,*,*))
      FOR t=nlags,nt-nlags DO BEGIN
         IF pr_mask(t) eq 1 THEN BEGIN
            FOR p=0,nz-1 DO BEGIN
               wap_comp(nlags/2,p)=wap_ts(p,t)+wap_comp(nlags/2,p)
               wap_comp_nothresh(nlags/2,p)=wap_ts(p,t)+wap_comp_nothresh(nlags/2,p)
            ENDFOR
            sample_size(nlags/2)=sample_size(nlags/2)+LONG(1)
            sample_size_nothresh=sample_size_nothresh+LONG(1)
            FOR k=0,nlags/2-1 DO BEGIN
               IF TOTAL(pr_mask(t+lags(k):t-1)) eq 0 THEN BEGIN
                  FOR p=0,nz-1 DO $
                     wap_comp(k,p)=wap_ts(p,t+lags(k))+wap_comp(k,p)
                  sample_size(k)=sample_size(k)+LONG(1)
               ENDIF
               FOR p=0,nz-1 DO $
                  wap_comp_nothresh(k,p)=wap_ts(p,t+lags(k))+wap_comp_nothresh(k,p)
            ENDFOR
            FOR k=nlags/2+1,nlags-1 DO BEGIN
               IF TOTAL(pr_mask(t+1:t+lags(k))) eq 0 THEN BEGIN
                  FOR p=0,nz-1 DO $
                     wap_comp(k,p)=wap_ts(p,t+lags(k))+wap_comp(k,p)
                  sample_size(k)=sample_size(k)+LONG(1)
               ENDIF
               FOR p=0,nz-1 DO $
                  wap_comp_nothresh(k,p)=wap_ts(p,t+lags(k))+wap_comp_nothresh(k,p)
            ENDFOR
         ENDIF
      ENDFOR
   ENDFOR
ENDFOR

FOR i=0,nlags-1 DO BEGIN
   wap_comp(i,*)=wap_comp(i,*)/FLOAT(sample_size(i))
   wap_comp_nothresh(i,*)=wap_comp_nothresh(i,*)/FLOAT(sample_size_nothresh)
ENDFOR

wap_levs=['-1e-1','-7e-2','-5e-2','-3e-2','-2e-2','-1e-2','-5e-3','5e-3','1e-2','2e-2','3e-2','5e-2','7e-2','1e-1']

psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_ts/mjo_diabatic_timestep_wts_wcomp.metum_mjodiab-2day_IndOcn_thresh.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=1500,YOFFSET=3000,/PORTRAIT
GSET,XMIN=MIN(lags)-0.5,XMAX=MAX(lags)+0.5,YMIN=1000.,YMAX=100.
CS,SCALE=1,NCOLS=N_ELEMENTS(wap_levs)+1,white=[9]
LEVS,MANUAL=wap_levs
CON,X=lags,Y=pressure,FIELD=wap_comp,/NOLINES,/BLOCK,CB_WIDTH=120,/NOCOLBAR
GPLOT,X=[0,0],Y=[100.,1000.],STYLE=2
COLBAR,COORDS=[1000,2000,20000,2500],TITLE='Vertical velocity (Pa s!U-1!N, positive up)',/ALT
AXES,XVALS=lags,YSTEP=-100.,YMINOR=-25.,YTITLE='Pressure (hPa)',XTITLE='Timestep (lag wrt convection)',NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_ts/mjo_diabatic_timestep_wts_wcomp.metum_mjodiab-2day_IndOcn_nothresh.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=1500,YOFFSET=3000,/PORTRAIT
GSET,XMIN=MIN(lags)-0.5,XMAX=MAX(lags)+0.5,YMIN=1000.,YMAX=100.
CS,SCALE=1,NCOLS=N_ELEMENTS(wap_levs)+1,white=[9]
LEVS,MANUAL=wap_levs
CON,X=lags,Y=pressure,FIELD=wap_comp_nothresh,/NOLINES,/BLOCK,CB_WIDTH=120,/NOCOLBAR
GPLOT,X=[0,0],Y=[100.,1000.],STYLE=2
COLBAR,COORDS=[1000,2000,20000,2500],TITLE='Vertical velocity (Pa s!U-1!N, positive up)',/ALT
AXES,XVALS=lags,YSTEP=-100.,YMINOR=-25.,YTITLE='Pressure (hPa)',XTITLE='Timestep (lag wrt convection)',NDECS=1
PSCLOSE

psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_ts/mjo_diabatic_timestep_wts_wcomp.metum_mjodiab-2day_IndOcn_thresh-minus-nothresh.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,MARGIN=2500,XOFFSET=1500,YOFFSET=3000,/PORTRAIT
GSET,XMIN=MIN(lags)-0.5,XMAX=MAX(lags)+0.5,YMIN=1000.,YMAX=100.
CS,SCALE=1,NCOLS=N_ELEMENTS(wap_levs)+1,white=[9]
LEVS,MANUAL=wap_levs
CON,X=lags,Y=pressure,FIELD=wap_comp-wap_comp_nothresh,/NOLINES,/BLOCK,CB_WIDTH=120,/NOCOLBAR
GPLOT,X=[0,0],Y=[100.,1000.],STYLE=2
COLBAR,COORDS=[1000,2000,20000,2500],TITLE='Vertical velocity (Pa s!U-1!N, positive up)',/ALT
AXES,XVALS=lags,YSTEP=-100.,YMINOR=-25.,YTITLE='Pressure (hPa)',XTITLE='Timestep (lag wrt convection)',NDECS=1
PSCLOSE

STOP
END

      
               
