PRO mjo_diabatic_timestep_wbin_binmcu_twodpdf


; Threshold density-weighted value of updraught mass flux for detecting convection
;all_thresh=[0.01,0.02,0.03,0.04]

all_thresh_bins=[4,4,4,4]
all_thresh=[0.01,0.4,0.8,1.2]
n_thresh=N_ELEMENTS(all_thresh)

; Bins of density-weighted pressure vertical velocity
;wap_bins=[-4.9,-3.5,-2.1,-0.7,0.7,2.1,3.5,4.9,6.3,7.7,9.1,10.5,11.9,13.3]/100.
;mcu_bins=[0.01,0.015,0.02,0.03,0.045,0.065,0.1,0.15,0.2,0.3,0.45,0.65,1.0]
;mcu_bins=[0.001,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5,1.6]
mcu_bins=[0.001,0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10]
;wap_bins=[-7.5,-5.5,-3.5,-1.5,1.5,3.5,5.5,7.5,9.5,11.5,13.5,15.5,17.5]/100.
;wap_bins=[-7.5,-6.5,-5.5,-4.5,-3.5,-2.5,-1.5,-0.5,0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5]/100.
wap_bins=[-22.5,-19.5,-16.5,-13.5,-10.5,-7.5,-4.5,-1.5,1.5,4.5,7.5,10.5,13.5,16.5,19.5,22.5]/100.
;wap_bins=[-15,-11,-9,-5,5,9,11,15,19,23,27,31,35]/100.
;wap_bins=[-9.8,-7.0,-4.2,-1.4,1.4,4.2,7.0,9.8,12.6,15.4,18.2,21.0,23.8,26.6]/100.

box=[-10,60,10,160]
nmodels=4

FOR p=0,nmodels-1 DO BEGIN
   print,p
   CASE p OF
      0 : BEGIN
         metum='/home/ss901165/um_output6/mjodiab_2day/metum'
                                ; Density and vertical velocity and mass flux
         rho_file=metum+'/MetUM.rho.20091020-20100110.lead_12-48hrs.nc'
         wap_file=metum+'/MetUM.wap.20091020-20100110.lead_12-48hrs.nc'
         mcu_file=metum+'/MetUM.mcu.20091020-20100110.lead_12-48hrs.nc'
         model_name='metum_ga3'
;         int_bot=27
         int_bot=0
         int_top=48
         nt=180*44
         mcu_bins=[0.001,0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10]/4.
         wap_bins=[-22.5,-19.5,-16.5,-13.5,-10.5,-7.5,-4.5,-1.5,1.5,4.5,7.5,10.5,13.5,16.5,19.5,22.5]/100.
         max_prob=0.3
         latitude_name='latitude'
         longitude_name='longitude'
         level_name='level'
      END
      1 : BEGIN
         giss='/home/ss901165/um_output6/mjodiab_2day/giss'
         rho_file=giss+'/ModelE.rho.20091020-20100110.lead_12-48hrs.nc'
         wap_file=giss+'/ModelE.wap.20091020-20100110.lead_12-48hrs.nc'
         mcu_file=giss+'/ModelE.mcu.20091020-20100110.lead_12-48hrs.nc'
         model_name='giss_modele2'
;         int_bot=11
         int_bot=0
         int_top=25
         nt=72*44
         mcu_bins=[0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10]/40.
         mcu_bins=[0.001,mcu_bins]
         wap_bins=[-22.5,-19.5,-16.5,-13.5,-10.5,-7.5,-4.5,-1.5,1.5,4.5,7.5,10.5,13.5,16.5,19.5,22.5]/2000.
         max_prob=0.3         
         latitude_name='latitude'
         longitude_name='longitude'         
         level_name='level'
      END
      2 : BEGIN
         mri='/home/ss901165/um_output6/mjodiab_2day/mri'
         rho_file=mri+'/MRI-AGCM.rho.20091020-20100110.lead_12-48hrs.nc'
         wap_file=mri+'/MRI-AGCM.wap.20091020-20100110.lead_12-48hrs.nc'
         mcu_file=mri+'/MRI-AGCM.mcu.20091020-20100110.lead_12-48hrs.nc'
         model_name='mri_agcm'
;         int_bot=12
         int_bot=0
         int_top=26
         nt=72*43
         mcu_bins=[0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10]/40.
         mcu_bins=[0.001,mcu_bins]
         wap_bins=[-22.5,-19.5,-16.5,-13.5,-10.5,-7.5,-4.5,-1.5,1.5,4.5,7.5,10.5,13.5,16.5,19.5,22.5]/500.
         max_prob=0.3
         latitude_name='lat'
         longitude_name='lon'
         level_name='lev'
      END
      3 : BEGIN
         cancm4='/home/ss901165/um_output6/mjodiab_2day/cancm4'
         rho_file=cancm4+'/CanCM4.rho.20091020-20100110.lead_12-48hrs.nc'
         wap_file=cancm4+'/CanCM4.wap.20091020-20100110.lead_12-48hrs.nc'
         mcu_file=cancm4+'/CanCM4.mcu.20091020-20100110.lead_12-48hrs.nc'
         model_name='cccma_cancm4'
;         int_bot=16
         int_bot=0
         int_top=25
         nt=36*44
         mcu_bins=[0.15,0.30,0.45,0.60,0.75,0.90,1.05,1.20,1.35,1.50,1.65,1.80,1.95,2.10]/40.
         mcu_bins=[0.001,mcu_bins]
         wap_bins=[-22.5,-19.5,-16.5,-13.5,-10.5,-7.5,-4.5,-1.5,1.5,4.5,7.5,10.5,13.5,16.5,19.5,22.5]/100.
         max_prob=0.3
         latitude_name='latitude'
         longitude_name='longitude'         
         level_name='level'
      END
   ENDCASE
   
   nmcu_bins=N_ELEMENTS(mcu_bins)
   nwap_bins=N_ELEMENTS(wap_bins)
   IF p eq 0 THEN BEGIN
      diffwap_mean=fltarr(nmodels,nmcu_bins)
      mcu_count=fltarr(nmodels,nmcu_bins)
   ENDIF

                                ; Read longitude, latitude, height
   lat=OPEN_AND_EXTRACT(rho_file,latitude_name)
   lon=OPEN_AND_EXTRACT(rho_file,longitude_name)
   DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
   nlon=N_ELEMENTS(lon)
   nlat=N_ELEMENTS(lat)
   z=OPEN_AND_EXTRACT(rho_file,level_name)
   nz=N_ELEMENTS(z)
   offset_time=0
   
; Read wap and rho
   rho=OPEN_AND_EXTRACT(rho_file,'rho',offset=[box_tx(1),box_tx(0),0,offset_time],$
                        count=[nlon,nlat,nz,nt])
   wap=OPEN_AND_EXTRACT(wap_file,'wap',offset=[box_tx(1),box_tx(0),0,offset_time],$
                        count=[nlon,nlat,nz,nt])
   mcu=OPEN_AND_EXTRACT(mcu_file,'mcu',offset=[box_tx(1),box_tx(0),0,offset_time],$
                        count=[nlon,nlat,nz,nt])
                                ;p=REFORM(OPEN_AND_EXTRACT(metum+'/MetUM.p.20100110.00Z.nc','p',offset=[box_tx(1),box_tx(0),0,0],$
                                ;                          count=[nlon,nlat,nz,1]))
   
; Compute rho-weighted wap (0-48 for troposphere, 20-26 for 3-5km, 27-48 for > 5km)
   
   wap_intrho=fltarr(nlon,nlat,nt)
   mcu_intrho=fltarr(nlon,nlat,nt)
                                ;p_bot=MEAN(p(*,*,int_bot))/100.
                                ;p_top=MEAN(p(*,*,int_top))/100.
   FOR i=0,nlon-1 DO BEGIN
      FOR j=0,nlat-1 DO BEGIN
         FOR k=0,nt-1 DO BEGIN
            rhosum=TOTAL(rho(i,j,int_bot:int_top,k))
            wap_intrho(i,j,k)=TOTAL(rho(i,j,int_bot:int_top,k)*wap(i,j,int_bot:int_top,k))/rhosum
            mcu_intrho(i,j,k)=TOTAL(rho(i,j,int_bot:int_top,k)*mcu(i,j,int_bot:int_top,k))/rhosum
         ENDFOR
      ENDFOR
   ENDFOR
      
   diffwap_intrho=fltarr(nlon,nlat,nt)
   twod_pdf_prct=fltarr(nmcu_bins,nwap_bins+1)
   FOR i=0,nlon-1 DO $
      FOR j=0,nlat-1 DO $
         FOR k=1,nt-2 DO $
            diffwap_intrho(i,j,k)=wap_intrho(i,j,k)-wap_intrho(i,j,k-1)
                                ;diffwap_intrho(i,j,k)=wap_intrho(i,j,k+1)-wap_intrho(i,j,k)

   FOR k=0,nmcu_bins-2 DO BEGIN
      IF TOTAL(where(mcu_intrho ge mcu_bins(k) and mcu_intrho lt mcu_bins(k+1))) ge 0 THEN BEGIN
         mcu_count(p,k)=N_ELEMENTS(where(mcu_intrho ge mcu_bins(k) and mcu_intrho lt mcu_bins(k+1)))
         this_diffwap=diffwap_intrho[where(mcu_intrho ge mcu_bins(k) and mcu_intrho lt mcu_bins(k+1))]
         diffwap_mean(p,k)=MEAN(this_diffwap)
         FOR m=0,nwap_bins-2 DO BEGIN
            IF TOTAL(where(this_diffwap ge wap_bins(m) and this_diffwap lt wap_bins(m+1))) ge 0 THEN $
               twod_pdf_prct(k,m+1)=N_ELEMENTS(where(this_diffwap ge wap_bins(m) and this_diffwap lt wap_bins(m+1)))
         ENDFOR
         IF TOTAL(where(this_diffwap lt wap_bins(0))) ge 0 THEN $
            twod_pdf_prct(k,0)=N_ELEMENTS(where(this_diffwap lt wap_bins(0)))
         IF TOTAL(where(this_diffwap ge wap_bins(nwap_bins-1))) ge 0 THEN $
            twod_pdf_prct(k,nwap_bins)=N_ELEMENTS(where(this_diffwap ge wap_bins(nwap_bins-1)))
      ENDIF
   ENDFOR
   IF TOTAL(where(mcu_intrho ge mcu_bins(nmcu_bins-1))) ge 0 THEN BEGIN
      mcu_count(p,k)=N_ELEMENTS(where(mcu_intrho ge mcu_bins(nmcu_bins-1)))
      this_diffwap=diffwap_intrho[where(mcu_intrho ge mcu_bins(nmcu_bins-1))]
      diffwap_mean(p,k)=MEAN(this_diffwap)
      FOR m=0,nwap_bins-2 DO BEGIN
         IF TOTAL(where(this_diffwap ge wap_bins(m) and this_diffwap lt wap_bins(m+1))) ge 0 THEN $
            twod_pdf_prct(nmcu_bins-1,m+1)=N_ELEMENTS(where(this_diffwap ge wap_bins(m) and this_diffwap lt wap_bins(m+1)))
      ENDFOR
      IF TOTAL(where(this_diffwap lt wap_bins(0))) ge 0 THEN $
         twod_pdf_prct(nmcu_bins-1,0)=N_ELEMENTS(where(this_diffwap lt wap_bins(0)))
      IF TOTAL(where(this_diffwap ge wap_bins(nwap_bins-1))) ge 0 THEN $
         twod_pdf_prct(nmcu_bins-1,nwap_bins)=N_ELEMENTS(where(this_diffwap ge wap_bins(nwap_bins-1)))
   ENDIF
   
   FOR k=0,nmcu_bins-1 DO $
      twod_pdf_prct(k,*)=twod_pdf_prct(k,*)/TOTAL(twod_pdf_prct(k,*))
   
;mylevs_prct=['1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
   wap_step=wap_bins(1)-wap_bins(0)

   mylevs_prct=['0.010','0.015','0.020','0.030','0.045','0.065','0.100','0.150','0.200','0.300','0.450','0.650','1.000']
   psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_binmcu_twodpdf.'+model_name+'_prct_wtm1_wt_'+$
          'intlev'+STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,XSIZE=15000,YSIZE=15000,SPACE2=3000,YOFFSET=2500,XOFFSET=2000,SPACE1=400
   GSET,XMIN=0,XMAX=nmcu_bins,YMIN=MIN(wap_bins)-wap_step,YMAX=MAX(wap_bins)+wap_step
   CS,SCALE=3,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
   LEVS,MANUAL=mylevs_prct
   CON,X=indgen(nmcu_bins)+0.5,Y=[MIN(wap_bins)-wap_step,wap_bins]+wap_step/2.,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,$
       CB_TITLE='Normalised probability',/CB_RIGHT ;,TITLE=psfile_title
   
   mcu_labels=strarr(nmcu_bins+1)
   FOR m=0,nmcu_bins-1 DO $
      mcu_labels(m)=STRMID(STRTRIM(STRING(mcu_bins(m)),1),0,5)
   mcu_labels(nmcu_bins)='>'+STRMID(STRTRIM(STRING(mcu_bins(nmcu_bins-1)),1),0,5)
   
   wap_labels=strarr(nwap_bins+2)
   FOR m=1,nwap_bins DO BEGIN
      IF wap_bins(m-1) lt 0 THEN BEGIN
         wap_labels(m)=STRMID(STRTRIM(STRING(wap_bins(m-1)),1),0,7)
      ENDIF ELSE $
         wap_labels(m)=STRMID(STRTRIM(STRING(wap_bins(m-1)),1),0,6)
   ENDFOR
   wap_labels(0)='<'+STRMID(STRTRIM(STRING(wap_bins(0)),1),0,7)
   wap_labels(nwap_bins+1)='>'+STRMID(STRTRIM(STRING(wap_bins(nwap_bins-1)),1),0,6)
   
   GPLOT,X=indgen(nmcu_bins)+0.5,Y=REFORM(diffwap_mean(p,*)),STYLE=0,SYM=4
   GPLOT,X=[0,nmcu_bins],Y=[0,0],STYLE=1
   
   AXES,XVALS=indgen(nmcu_bins+1),YVALS=(indgen(nwap_bins+2)-1)*wap_step+MIN(wap_bins),YLABELS=wap_labels,XLABELS=mcu_labels,$
        XTITLE='M at timestep t (kg m!U-2!N s!U-1!N)',YTITLE='W(t) - W(t-1) (Pa s!U-1!N)',$ 
        ORIENTATION=35,/NORIGHT
   
   GSET,XMIN=0,XMAX=nmcu_bins,YMIN=0,YMAX=max_prob
   mcu_count(p,*)=mcu_count(p,*)/(FLOAT(nt-2)*FLOAT(nlon)*FLOAT(nlat))
   GPLOT,X=indgen(nmcu_bins)+0.5,Y=REFORM(mcu_count(p,*)),STYLE=2,SYM=3
   AXES,YSTEP=max_prob/10.,/ONLYRIGHT,YTITLE='Probability (considering all timesteps)',NDECS=3,YMINOR=max_prob/20.
   ;GPLOT,X=-4.0,Y=-3.3,TEXT='W and M computed over levels '+STRTRIM(STRING(int_bot+1),1)+' to '+STRTRIM(STRING(int_top+1),1)
         ;', hybrid ht '+STRTRIM(STRING(z(int_bot)),1)+' to '+STRTRIM(STRING(z(int_top)),1)+', '+$
         ;STRMID(STRTRIM(STRING(p_bot),1),0,4)+' to '+STRMID(STRTRIM(STRING(p_top),1),0,4)+' hPa',ALIGN=0.0
   GLEGEND,labels=['Probability of M','Mean change in W'],STYLE=[2,0],SYM=[3,4],LEGXOFFSET=-8000,LEGYOFFSET=-2000
   
   PSCLOSE,/NOVIEW
Endfor

colors=['orangered','purple','blue','dodgerblue']
psfile='/home/ss901165/idl/mjo_diabatic/timestep/w_bin/mjo_diabatic_timestep_wbin_binmcu_twodpdf.diffwap_wtm1_wt_'+$
       'intlev'+STRTRIM(STRING(int_bot),1)+'-'+STRTRIM(STRING(int_top),1)+'.ps'
PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,TCHARSIZE=100,XSIZE=15000,YSIZE=15000,$
       SPACE2=3000,YOFFSET=2500,XOFFSET=2000,SPACE1=400
GSET,XMIN=0,XMAX=nmcu_bins,YMIN=MIN(wap_bins)/2.,YMAX=MAX(wap_bins)/2.
FOR p=0,nmodels-1 DO $
   GPLOT,X=indgen(nmcu_bins)+0.5,Y=REFORM(diffwap_mean(p,*)),STYLE=0,SYM=4,COL=FSC_COLOR(colors(p))
GPLOT,X=[0,nmcu_bins],Y=[0,0],STYLE=2
AXES,XVALS=indgen(nmcu_bins+1),YVALS=wap_bins/2.,YLABELS=STRMID(STRTRIM(STRING(wap_bins/2.),1),0,7),$
     XLABELS=mcu_labels,XTITLE='M at timestep t (kg m!U-2!N s!U-1!N)',$
     YTITLE='Mean of [W(t) - W(t-1)] (Pa s!U-1!N)',ORIENTATION=35,/NORIGHT,NDECS=4
GSET,XMIN=0,XMAX=nmcu_bins,YMIN=0,YMAX=max_prob
FOR p=0,nmodels-1 DO $
   GPLOT,X=indgen(nmcu_bins)+0.5,Y=REFORM(mcu_count(p,*)),STYLE=2,SYM=3,COL=FSC_COLOR(colors(p))
AXES,YSTEP=max_prob/10.,YMINOR=max_prob/20.,YTITLE='Probability (considering all timesteps)',/ONLYRIGHT,NDECS=2
GLEGEND,labels=['Probability of M','Mean change in W'],STYLE=[2,0],SYM=[3,4],LEGXOFFSET=-8000,LEGYOFFSET=-2000
GLEGEND,labels=['MetUM GA3','GISS ModelE2','MRI-AGCM','CanCM4'],COL=FSC_COLOR(colors),SYM=[3,3,3,3],$
        LENGTH=0,LEGXOFFSET=9000,LEGYOFFSET=0
PSCLOSE

STOP
END
