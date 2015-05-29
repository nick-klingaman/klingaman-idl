PRO hadgem3a_monwg_jjas_precip_ts_indwpac
  
dmean_infiles=['/home/ss901165/um_output6/xgspj/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmeans.years1-20.precip.nc',$
               '/home/ss901165/um_output6/xihvf/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-27.precip.nc',$
               '/home/ss901165/um_output6/xihvg/hadgem3a_kpp50N50S_1.5xentrain_ga30.jan-dec_dmeans.years1-19.precip.nc']
imd_clim_file='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004_mjjas_daily_clim.nc'
descs=['A-ENT-OBS',$
       'K-ENT-OBS-50_fr',$
       'A-ENT-K50']

all_nyears=[20,27,19]
offset_jun1=[150,150,150]
n_time=120
varname='precip'
multiplier=86400.
all_colors=['dodgerblue','red','navyblue']
n96_mask_file='/home/ss901165/um_output/mask_n96.nc'
; Box to area average
ind_box=[10,65,30,90]
pac_box=[10,120,30,160]
eqi_box=[-10,60,10,80]

max_years=27
n_sets=N_ELEMENTS(dmean_infiles)
ind_jjas_ts=fltarr(n_sets,max_years)
pac_jjas_ts=fltarr(n_sets,max_years)
eqi_jjas_ts=fltarr(n_sets,max_years)

FOR k=0,n_sets-1 DO BEGIN   
   ind_longitude=OPEN_AND_EXTRACT(dmean_infiles(k),'longitude')
   ind_latitude=OPEN_AND_EXTRACT(dmean_infiles(k),'latitude')
   DEFINE_BOUNDARIES,ind_box,ind_latitude,ind_longitude,ind_box_tx,/LIMIT
   ind_nlon=N_ELEMENTS(ind_longitude)
   ind_nlat=N_ELEMENTS(ind_latitude)

   pac_longitude=OPEN_AND_EXTRACT(dmean_infiles(k),'longitude')
   pac_latitude=OPEN_AND_EXTRACT(dmean_infiles(k),'latitude')
   DEFINE_BOUNDARIES,pac_box,pac_latitude,pac_longitude,pac_box_tx,/LIMIT
   pac_nlon=N_ELEMENTS(pac_longitude)
   pac_nlat=N_ELEMENTS(pac_latitude)

   eqi_longitude=OPEN_AND_EXTRACT(dmean_infiles(k),'longitude')
   eqi_latitude=OPEN_AND_EXTRACT(dmean_infiles(k),'latitude')
   DEFINE_BOUNDARIES,eqi_box,eqi_latitude,eqi_longitude,eqi_box_tx,/LIMIT
   eqi_nlon=N_ELEMENTS(eqi_longitude)
   eqi_nlat=N_ELEMENTS(eqi_latitude)
                                ; Get mask grid information
                                ;   mask_longitude=OPEN_AND_EXTRACT(n96_mask_file,'longitude')
                                ;   mask_latitude=OPEN_AND_EXTRACT(n96_mask_file,'latitude')
                                ;   DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
                                ;   mask_nlon=N_ELEMENTS(mask_longitude)
                                ;   mask_nlat=N_ELEMENTS(mask_latitude)
                                ;   n96_mask=REFORM(OPEN_AND_EXTRACT(n96_mask_file,'lsm',offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                ;                                    count=[mask_nlon,mask_nlat,1,1]))
                                ;   n96_mask_rev=fltarr(mask_nlon,mask_nlat)
                                ;   FOR i=0,nlon-1 DO $
                                ;      FOR j=0,nlat-1 DO $
                                ;         n96_mask_rev(i,nlat-j-1)=n96_mask(i,j)      
   FOR i=0,all_nyears(k)-1 DO BEGIN
      ind_jjas_ts(k,i)=MEAN(REFORM(OPEN_AND_EXTRACT(dmean_infiles(k),'precip',$
                                                    offset=[ind_box_tx(1),ind_box_tx(0),150,i],$
                                                    count=[ind_nlon,ind_nlat,120,1])))*86400.
      pac_jjas_ts(k,i)=MEAN(REFORM(OPEN_AND_EXTRACT(dmean_infiles(k),'precip',$
                                                    offset=[pac_box_tx(1),pac_box_tx(0),150,i],$
                                                    count=[pac_nlon,pac_nlat,120,1])))*86400.      
      eqi_jjas_ts(k,i)=MEAN(REFORM(OPEN_AND_EXTRACT(dmean_infiles(k),'precip',$
                                                    offset=[eqi_box_tx(1),eqi_box_tx(0),150,i],$
						    count=[eqi_nlon,eqi_nlat,120,1])))*86400.
   ENDFOR
   IF all_nyears(k) lt max_years THEN BEGIN
      ind_jjas_ts(k,all_nyears(k):max_years-1)=!Values.F_NaN
      pac_jjas_ts(k,all_nyears(k):max_years-1)=!Values.F_NaN
      eqi_jjas_ts(k,all_nyears(k):max_years-1)=!Values.F_NaN
   ENDIF
ENDFOR

; Build plot
psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_indwpac.ent_kpp.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=1,XMAX=8,YMIN=6,YMAX=16,TITLE='JJAS area-avg precip over India (10-30N, 70-90E) and W.Pac. (10-30N, 120-160E)'
FOR i=0,n_sets-1 DO BEGIN
   GPLOT,X=ind_jjas_ts(i,*),Y=pac_jjas_ts(i,*),COL=FSC_COLOR(all_colors(i)),SYM=3,/NOLINES
   regcoeff=REGRESS(REFORM(ind_jjas_ts(i,0:all_nyears(i)-1)),REFORM(pac_jjas_ts(i,0:all_nyears(i)-1)),$
                    CONST=regconst)
   corcoeff=CORRELATE(ind_jjas_ts(i,0:all_nyears(i)-1),pac_jjas_ts(i,0:all_nyears(i)-1))
   GPLOT,X=[MIN(ind_jjas_ts(i,*),/NaN)*0.8,MAX(ind_jjas_ts(i,*),/NaN)*1.2],$
         Y=[MIN(ind_jjas_ts(i,*),/NaN)*0.8,MAX(ind_jjas_ts(i,*),/NaN)*1.2]*regcoeff(0)+regconst,$
         COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=1.5,Y=8-i*0.5,TEXT=STRMID(STRTRIM(STRING(corcoeff),1),0,5),COL=FSC_COLOR(all_colors(i)),ALIGN=0.0
ENDFOR
AXES,XSTEP=1,YSTEP=1,YTITLE='JJAS rainfall in West Pacific (mm day!U-1!N)',XTITLE='JJAS rainfall in India (mm day!U-1!N)',$
     /NORIGHT,/NOUPPER,XMINOR=0.5,YMINOR=0.5
GLEGEND,labels=REVERSE(descs),col=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_indeqio.ent_kpp.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=1,XMAX=8,YMIN=3,YMAX=10,TITLE='JJAS area-avg precip over India (10-30N, 70-90E) and Eq.IO (10S-10N, 60-80E)'
FOR i=0,n_sets-1 DO BEGIN
   GPLOT,X=ind_jjas_ts(i,*),Y=eqi_jjas_ts(i,*),COL=FSC_COLOR(all_colors(i)),SYM=3,/NOLINES
   regcoeff=REGRESS(REFORM(ind_jjas_ts(i,0:all_nyears(i)-1)),REFORM(eqi_jjas_ts(i,0:all_nyears(i)-1)),$
                    CONST=regconst)
   corcoeff=CORRELATE(ind_jjas_ts(i,0:all_nyears(i)-1),eqi_jjas_ts(i,0:all_nyears(i)-1))
   GPLOT,X=[MIN(ind_jjas_ts(i,*),/NaN)*0.8,MAX(ind_jjas_ts(i,*),/NaN)*1.2],$
         Y=[MIN(ind_jjas_ts(i,*),/NaN)*0.8,MAX(ind_jjas_ts(i,*),/NaN)*1.2]*regcoeff(0)+regconst,$
         COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=1.5,Y=8-i*0.5,TEXT=STRMID(STRTRIM(STRING(corcoeff),1),0,5),COL=FSC_COLOR(all_colors(i)),ALIGN=0.0
ENDFOR
AXES,XSTEP=1,YSTEP=1,YTITLE='JJAS rainfall in equatorial Indian Ocean (mm day!U-1!N)',$
     XTITLE='JJAS rainfall in India (mm day!U-1!N)',/NORIGHT,/NOUPPER,XMINOR=0.5,YMINOR=0.5
GLEGEND,labels=REVERSE(descs),col=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_indwpac.ent_kpp_anom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=-3,XMAX=3,YMIN=-4,YMAX=4,TITLE='JJAS area-avg precip over India (10-30N, 70-90E) and W.Pac. (10-30N, 120-160E)'
FOR i=0,n_sets-1 DO BEGIN
   ind_temp=REFORM(ind_jjas_ts(i,0:all_nyears(i)-1))-MEAN(ind_jjas_ts(i,*),/NaN)
   pac_temp=REFORM(pac_jjas_ts(i,0:all_nyears(i)-1))-MEAN(pac_jjas_ts(i,*),/NaN)
   GPLOT,X=ind_temp,Y=pac_temp,COL=FSC_COLOR(all_colors(i)),SYM=3,/NOLINES
   regcoeff=REGRESS(ind_temp,pac_temp,CONST=regconst)
   corcoeff=CORRELATE(ind_temp,pac_temp)
   GPLOT,X=[MIN(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1,$
            MAX(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1],$
         Y=[MIN(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1,$
            MAX(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1]*regcoeff(0)+regconst,$
         COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=-2.5,Y=-2-i*0.5,TEXT=STRMID(STRTRIM(STRING(corcoeff),1),0,5),COL=FSC_COLOR(all_colors(i)),ALIGN=0.0
ENDFOR
GPLOT,X=[-3,3],Y=[0,0],STYLE=2
GPLOT,Y=[-4,4],X=[0,0],STYLE=2
AXES,XSTEP=0.5,YSTEP=1,YMINOR=0.25,YTITLE='Anom in JJAS W Pac rainfall (mm day!U-1!N)',$
     XTITLE='Anom in JJAS India rainfall (mm day!U-1!N)',/NORIGHT,/NOUPPER,XMINOR=0.2,NDECS=1
GLEGEND,labels=REVERSE(descs),col=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_ts_indeqio.ent_kpp_anom.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=2000,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
GSET,XMIN=-3,XMAX=3,YMIN=-3,YMAX=3,TITLE='JJAS area-avg precip over India (10-30N, 70-90E) and Eq.IO (10S-10N, 60-80E)'
FOR i=0,n_sets-1 DO BEGIN
   ind_temp=REFORM(ind_jjas_ts(i,0:all_nyears(i)-1))-MEAN(ind_jjas_ts(i,*),/NaN)
   eqi_temp=REFORM(eqi_jjas_ts(i,0:all_nyears(i)-1))-MEAN(eqi_jjas_ts(i,*),/NaN)
   GPLOT,X=ind_temp,Y=eqi_temp,COL=FSC_COLOR(all_colors(i)),SYM=3,/NOLINES
   regcoeff=REGRESS(ind_temp,eqi_temp,CONST=regconst)
   corcoeff=CORRELATE(ind_temp,eqi_temp)
   GPLOT,X=[MIN(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1,$
            MAX(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1],$
         Y=[MIN(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1,$
            MAX(ind_jjas_ts(i,*)-MEAN(ind_jjas_ts(i,*),/NaN),/NaN)*1.1]*regcoeff(0)+regconst,$
         COL=FSC_COLOR(all_colors(i)),STYLE=0
   GPLOT,X=-2.5,Y=-2-i*0.5,TEXT=STRMID(STRTRIM(STRING(corcoeff),1),0,5),COL=FSC_COLOR(all_colors(i)),ALIGN=0.0
ENDFOR
GPLOT,X=[-3,3],Y=[0,0],STYLE=2
GPLOT,Y=[-3,3],X=[0,0],STYLE=2
AXES,XSTEP=0.5,YSTEP=0.5,YMINOR=0.25,YTITLE='Anom in JJAS Eq Ind Ocn rainfall (mm day!U-1!N)',$
     XTITLE='Anom in JJAS India rainfall (mm day!U-1!N)',/NORIGHT,/NOUPPER,XMINOR=0.2,NDECS=1
GLEGEND,labels=REVERSE(descs),col=REVERSE(FSC_COLOR(all_colors)),LEGPOS=9
PSCLOSE,/NOVIEW
   
STOP

END
