PRO hadgem3_monwg_t500_gradients

n_sets=1
xihvd='/home/ss901165/um_output6/xihvd'
xgspj='/home/ss901165/um_output6/xgspj'
xhwob='/home/ss901165/um_output6/xhwob'
eraint='/home/ss901165/datasets/ERA-INTERIM/T500'

mylevs_raw=['250','252','254','256','258','260','262','264','266','268','270','272','274','276']
mylevs_diff=['-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2']
box_lonavg=[-20,70,50,100]

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         expt_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.T500.nc'
         eraint_file=eraint+'/T500.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         expt_varname='temp'
         ctrl_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.T500.nc'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         ctrl_varname='temp'
         ctrl_shift=0
         expt_shift=0
      END
      1 : BEGIN
         expt_file=xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.T500.nc'
         expt_name='hadgem3kpp_1.5xentrain_ga30_50N50S_free'
         expt_varname='temp'
         ctrl_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.T500.nc'
         ctrl_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         ctrl_varname='temp' 
         ctrl_shift=15
      END
      2 : BEGIN
         expt_file=xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.T500.nc'
         eraint_file=eraint+'/T500.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc'
         expt_name='hadgem3kpp_1.5xentrain_ga30_30N30S'
         expt_varname='temp'
         expt_shift=15
         ctrl_file=xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.T500.nc'
         ctrl_name='hadgem3a_ukmo_1.5xentrain_ga30'
         ctrl_varname='temp'
         ctrl_shift=0
      END
   ENDCASE

   expt_longitude=OPEN_AND_EXTRACT(expt_file,'longitude')
   expt_latitude=OPEN_AND_EXTRACT(expt_file,'latitude')
   DEFINE_BOUNDARIES,box_lonavg,expt_latitude,expt_longitude,expt_box_tx,/LIMIT
   expt_nlon=N_ELEMENTS(expt_longitude)
   expt_nlat=N_ELEMENTS(expt_latitude)
   
   eraint_longitude=OPEN_AND_EXTRACT(eraint_file,'longitude')
   eraint_latitude=OPEN_AND_EXTRACT(eraint_file,'latitude')
   DEFINE_BOUNDARIES,box_lonavg,eraint_latitude,eraint_longitude,eraint_box_tx,/LIMIT
   eraint_nlon=N_ELEMENTS(eraint_longitude)
   eraint_nlat=N_ELEMENTS(eraint_latitude)

   ctrl_longitude=OPEN_AND_EXTRACT(ctrl_file,'longitude')
   ctrl_latitude=OPEN_AND_EXTRACT(ctrl_file,'latitude')
   DEFINE_BOUNDARIES,box_lonavg,ctrl_latitude,ctrl_longitude,ctrl_box_tx,/LIMIT
   ctrl_nlon=N_ELEMENTS(ctrl_longitude)
   ctrl_nlat=N_ELEMENTS(ctrl_latitude)
   
   expt_var=OPEN_AND_EXTRACT(expt_file,expt_varname,$
                             offset=[expt_box_tx(1),expt_box_tx(0),0],$
                             count=[expt_nlon,expt_nlat,360])
   eraint_var=OPEN_AND_EXTRACT(eraint_file,'T',$
                             offset=[eraint_box_tx(1),eraint_box_tx(0),0],$
                             count=[eraint_nlon,eraint_nlat,360])
   ctrl_var=OPEN_AND_EXTRACT(ctrl_file,ctrl_varname,$
                             offset=[ctrl_box_tx(1),ctrl_box_tx(0),0],$
                             count=[ctrl_nlon,ctrl_nlat,360])
   IF ctrl_shift gt 0 THEN BEGIN
      temp=ctrl_var
      ctrl_var(*,*,0:ctrl_shift-1)=temp(*,*,360-ctrl_shift:359)
      ctrl_var(*,*,ctrl_shift:359)=temp(*,*,0:360-ctrl_shift-1)
   ENDIF
   IF expt_shift gt 0 THEN BEGIN
      temp=expt_var
      expt_var(*,*,0:expt_shift-1)=temp(*,*,360-expt_shift:359)
      expt_var(*,*,expt_shift:359)=temp(*,*,0:360-expt_shift-1)
   ENDIF
   
   expt_var_lonavg=fltarr(360,expt_nlat)
   eraint_var_lonavg=fltarr(360,eraint_nlat)
   ctrl_var_lonavg=fltarr(360,expt_nlat)
   FOR j=0,eraint_nlat-1 DO BEGIN
      FOR k=0,359 DO $
         eraint_var_lonavg(k,j)=MEAN(eraint_var(*,j,k))
      eraint_var_lonavg(*,j)=SMOOTH(eraint_var_lonavg(*,j),11)
   ENDFOR
   FOR j=0,expt_nlat-1 DO BEGIN
      FOR k=0,359 DO $
         expt_var_lonavg(k,j)=MEAN(expt_var(*,j,k))
      expt_var_lonavg(*,j)=SMOOTH(expt_var_lonavg(*,j),11)
   ENDFOR
   FOR j=0,ctrl_nlat-1 DO BEGIN
      FOR k=0,359 DO $
         ctrl_var_lonavg(k,j)=MEAN(ctrl_var(*,j,k))
      ctrl_var_lonavg(*,j)=SMOOTH(ctrl_var_lonavg(*,j),11)
   ENDFOR
   
   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_'+expt_name+'_clim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=expt_latitude,FIELD=expt_var_lonavg,TITLE='70-100E 500 hPa temperature from '+expt_name
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_eraint_clim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=expt_latitude,FIELD=eraint_var_lonavg,TITLE='70-100E 500 hPa temperature from ERA-Interim (1989-2008)'
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_'+ctrl_name+'_clim.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   LEVS,MANUAL=mylevs_raw
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=ctrl_latitude,FIELD=ctrl_var_lonavg,TITLE='70-100E 500 hPa temperature from '+ctrl_name
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_'+expt_name+'-minus-'+ctrl_name+'_clim.ps'
    PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   LEVS,MANUAL=mylevs_diff
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=ctrl_latitude,FIELD=expt_var_lonavg-ctrl_var_lonavg,TITLE='Diff in 70-100E T500 for '+$
       expt_name+' minus '+ctrl_name
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_'+expt_name+'-minus-eraint_clim.ps'
    PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   LEVS,MANUAL=mylevs_diff
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=ctrl_latitude,FIELD=expt_var_lonavg-eraint_var_lonavg,TITLE='Diff in 70-100E T500 for '+$
       expt_name+' minus ERA-Interim'
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE

   psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_'+ctrl_name+'-minus-eraint_clim.ps'
    PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,FONT=2,SPACE2=800
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   LEVS,MANUAL=mylevs_diff
   GSET,XMIN=0,XMAX=360,YMIN=box_lonavg(0),YMAX=box_lonavg(2)
   CON,X=indgen(360)+0.5,Y=ctrl_latitude,FIELD=ctrl_var_lonavg-eraint_var_lonavg,TITLE='Diff in 70-100E T500 for '+$
       ctrl_name+' minus ERA-Interim'
   AXES,XSTEP=30,XMINOR=15,YSTEP=5,YTITLE='Latitude (degrees north)',XTITLE='Day in year'
   PSCLOSE
   
ENDFOR
STOP

gradbox1=[-10,70,10,100]
gradbox2=[20,70,40,100]
infiles=[xihvd+'/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmean_clim.years1-20.T500.nc',$
         xhwob+'/hadgem3kpp_1.5xentrain_ga30_30N30S.jan-dec_dmean_clim.years1-14.T500.nc',$
         xgspj+'/hadgem3a_ukmo_1.5xentrain_vn78.jan-dec_dmean_clim.years1-20.T500.nc',$
         eraint+'/T500.jan-dec_dmean_clim.1989-2008.50S50N_domain.n96.nc']
descs=['K(50)-ENT-OBS(fr)',$
       'K(30)-ENT-OBS(fr)',$
       'A-ENT-OBS',$
       'ERA-Interim']
varnames=['temp','temp','temp','T']
colors=['red','brown','dodgerblue','black']
shifts=[0,15,0,0]
n_runs=N_ELEMENTS(infiles)
gradient_ts=fltarr(n_runs,360)
box1_ts=fltarr(n_runs,360)
box2_ts=fltarr(n_runs,360)
FOR i=0,n_runs-1 DO BEGIN
   longitude=OPEN_AND_EXTRACT(infiles(i),'longitude')
   latitude=OPEN_AND_EXTRACT(infiles(i),'latitude')
   DEFINE_BOUNDARIES,gradbox1,latitude,longitude,gradbox1_tx,/LIMIT
   gradbox1_nlon=N_ELEMENTS(longitude)
   gradbox1_nlat=N_ELEMENTS(latitude)

   longitude=OPEN_AND_EXTRACT(infiles(i),'longitude')
   latitude=OPEN_AND_EXTRACT(infiles(i),'latitude')
   DEFINE_BOUNDARIES,gradbox2,latitude,longitude,gradbox2_tx,/LIMIT
   gradbox2_nlon=N_ELEMENTS(longitude)
   gradbox2_nlat=N_ELEMENTS(latitude)

   T_box1=OPEN_AND_EXTRACT(infiles(i),varnames(i),$
                           offset=[gradbox1_tx(1),gradbox1_tx(0),0],$
                           count=[gradbox1_nlon,gradbox1_nlat,360])
   T_box2=OPEN_AND_EXTRACT(infiles(i),varnames(i),$
                           offset=[gradbox2_tx(1),gradbox2_tx(0),0],$
                           count=[gradbox2_nlon,gradbox2_nlat,360])
   FOR j=0,359 DO BEGIN
      gradient_ts(i,j)=MEAN(T_box2(*,*,j)-T_box1(*,*,j))
      box1_ts(i,j)=MEAN(T_box1(*,*,j))
      box2_ts(i,j)=MEAN(T_box2(*,*,j))
   ENDFOR
   IF shifts(i) gt 0 THEN BEGIN
      temp=gradient_ts(i,*)
      gradient_ts(i,0:shifts(i)-1)=temp(360-shifts(i):359)
      gradient_ts(i,shifts(i):359)=temp(0:360-shifts(i)-1)
      temp=box1_ts(i,*)
      box1_ts(i,0:shifts(i)-1)=temp(360-shifts(i):359)
      box1_ts(i,shifts(i):359)=temp(0:360-shifts(i)-1)
      temp=box2_ts(i,*)
      box2_ts(i,0:shifts(i)-1)=temp(360-shifts(i):359)
      box2_ts(i,shifts(i):359)=temp(0:360-shifts(i)-1)      
   ENDIF
ENDFOR

FOR i=0,n_runs-1 DO BEGIN
   temp=[REFORM(gradient_ts(i,*)),REFORM(gradient_ts(i,*))]
   temp=SMOOTH(temp,31)
   gradient_ts(i,*)=[temp(360:375),temp(16:359)]
   temp=[REFORM(box1_ts(i,*)),REFORM(box1_ts(i,*))]
   temp=SMOOTH(temp,31)
   box1_ts(i,*)=[temp(360:375),temp(16:359)]
   temp=[REFORM(box2_ts(i,*)),REFORM(box2_ts(i,*))]
   temp=SMOOTH(temp,31)
   box2_ts(i,*)=[temp(360:375),temp(16:359)]
ENDFOR

psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_20N-40N-minus-10S-10N_ts.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=120
GSET,XMIN=0,XMAX=360,YMIN=-14,YMAX=4,TITLE='Meridional T500 gradients: 20-40N minus 10S-10N over 70-100E'
FOR i=0,n_runs-1 DO $
   GPLOT,X=indgen(360)+0.5,Y=SMOOTH(REFORM(gradient_ts(i,*)),7),COL=FSC_COLOR(colors(i))
GPLOT,X=[0,360],Y=[0,0],STYLE=1
AXES,XSTEP=30,XMINOR=15,YSTEP=2,YMINOR=1,YTITLE='Meridional T500 gradient (K)',XTITLE='Day of year',NDECS=1
GLEGEND,labels=descs,LEGPOS=7,COL=FSC_COLOR(colors)
PSCLOSE

psfile='/home/ss901165/idl/hadgem3_monwg/t500/hadgem3_monwg_t500_gradients.lonavg70-100_20N-40N-minus-10S-10N_tserror.ps'
PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=120
GSET,XMIN=0,XMAX=360,YMIN=-2.4,YMAX=1,TITLE='Biases in T500 against ERA-Interim (70-100E average)'
FOR i=0,n_runs-2 DO BEGIN
   GPLOT,X=indgen(360)+0.5,Y=REFORM(gradient_ts(i,*))-REFORM(gradient_ts(3,*)),COL=FSC_COLOR(colors(i))
   GPLOT,X=indgen(360)+0.5,Y=REFORM(box1_ts(i,*))-REFORM(box1_ts(3,*)),COL=FSC_COLOR(colors(i)),STYLE=1
   GPLOT,X=indgen(360)+0.5,Y=REFORM(box2_ts(i,*))-REFORM(box2_ts(3,*)),COL=FSC_COLOR(colors(i)),STYLE=2
ENDFOR
GPLOT,X=[0,360],Y=[0,0],STYLE=1
AXES,XSTEP=30,XMINOR=15,YSTEP=0.2,YMINOR=0.1,YTITLE='Bias against ERA-Interim (K)',XTITLE='Day of year',NDECS=1
GLEGEND,labels=descs,LEGPOS=7,COL=FSC_COLOR(colors)
GLEGEND,labels=['20-40N','10S-10N','Gradient'],STYLE=[2,1,0],LEGPOS=9
PSCLOSE

STOP
END
