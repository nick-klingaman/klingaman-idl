PRO hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist

; Box to read (consider all gridpoints within)
box=[-5,70,5,90]

updraught_massflux_levels=['0','0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.0']
downdraught_massflux_levels=['0','0.015','0.03','0.045','0.06','0.075','0.09','0.105','0.12','0.135','0.15','0.165','0.18','0.195']                           
updraught_massflux_bins=[0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2]
; For 200 hPa
;downdraught_massflux_bins=[0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10]
downdraught_massflux_bins=[0,0.02,0.04,0.06,0.08,0.10,0.12,0.14,0.16,0.18,0.20]
n_updraught_massflux_bins=N_ELEMENTS(updraught_massflux_bins)
n_downdraught_massflux_bins=N_ELEMENTS(downdraught_massflux_bins)

n_bins=20
single_level=700

n_models=2
hist_bins=fltarr(n_models,n_bins+1)
hist_bins(*,0)=0

um3_dir='/home/ss901165/um_output3'
updraught_massflux_file_suffix='a.06apr09-10apr09.updraught_massflux_thlvl.nc'
downdraught_massflux_file_suffix='a.06apr09-10apr09.downdraught_massflux_thlvl.nc'
thlvl_pres_file_suffix='a.06apr09-10apr09.p_thlvl.nc'
precip_file_suffix='a.06apr09-10apr09.cvrain.nc'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runid='xfioz'
         dirname='xfioz'
         description='Time-varying OSTIA SSTs (xfadh)'
         precip_latname='latitude_1'
      END
      1 : BEGIN
         runid='xfadt'
         dirname='xfadt'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk)'
         precip_latname='latitude_1'
      END
   ENDCASE
   print,runid
   model_count=360

   thlvl_pres_file=um3_dir+'/'+dirname+'/'+runid+thlvl_pres_file_suffix   
   model_lon=OPEN_AND_EXTRACT(thlvl_pres_file,'longitude')
   model_lat=OPEN_AND_EXTRACT(thlvl_pres_file,'latitude')
   model_nz=N_ELEMENTS(OPEN_AND_EXTRACT(thlvl_pres_file,'hybrid_ht_1'))   
   DEFINE_BOUNDARIES,box,model_lat,model_lon,model_box_tx,/LIMIT
   model_nlat=N_ELEMENTS(model_lat)
   model_nlon=N_ELEMENTS(model_lon)
   
   print,'Reading pressure of model theta levels ...'
   thlvl_pres=REFORM(OPEN_AND_EXTRACT(thlvl_pres_file,'p_thlvl',$
                                       offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                       count=[model_nlon,model_nlat,model_nz,model_count]))/100.
   thlvl_pres_mean=fltarr(model_nz)
   FOR j=0,model_nz-1 DO $
      thlvl_pres_mean(j)=MEAN(thlvl_pres(*,*,j,*))

   model_nz=NEAREST(thlvl_pres_mean,150)+1
   updraught_massflux_file=um3_dir+'/'+dirname+'/'+runid+updraught_massflux_file_suffix
   downdraught_massflux_file=um3_dir+'/'+dirname+'/'+runid+downdraught_massflux_file_suffix
   precip_file=um3_dir+'/'+dirname+'/'+runid+precip_file_suffix

   model_zsing=NEAREST(thlvl_pres_mean,single_level)
   IF i eq 0 THEN BEGIN
      binned_model_updraught_massflux=fltarr(n_models,n_bins+1,model_nz)
      binned_model_downdraught_massflux=fltarr(n_models,n_bins+1,model_nz)
      updraught_singlelvl_joint_pdf=fltarr(n_models,n_updraught_massflux_bins,n_bins+1)
      downdraught_singlelvl_joint_pdf=fltarr(n_models,n_downdraught_massflux_bins,n_bins+1)
      updraught_singlelvl_pdf=fltarr(n_models,41)
      downdraught_singlelvl_pdf=fltarr(n_models,41)
      bins_npoints=fltarr(n_models,n_bins+1)
      model_runids=strarr(n_models)
      model_legend_labels=strarr(n_models)      
   ENDIF   
   model_preciplat=OPEN_AND_EXTRACT(precip_file,precip_latname)
   model_preciplon=OPEN_AND_EXTRACT(precip_file,'longitude')
   DEFINE_BOUNDARIES,box,model_preciplat,model_preciplon,model_precipbox_tx,/LIMIT
   model_npreciplat=N_ELEMENTS(model_preciplat)
   model_npreciplon=N_ELEMENTS(model_preciplon)
   
   print,'Reading updraught massflux ...'
   model_updraught_massflux=REFORM(OPEN_AND_EXTRACT(updraught_massflux_file,'updraught_massflux_thlvl',$
                                            offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                            count=[model_nlon,model_nlat,model_nz,model_count]))
   print,'Reading downdraught massflux ...'
   model_downdraught_massflux=REFORM(OPEN_AND_EXTRACT(downdraught_massflux_file,'downdraught_massflux_thlvl',$
                                           offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                           count=[model_nlon,model_nlat,model_nz,model_count]))
   print,'Reading precipitation ...'
   model_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'cvrain',$
                                        offset=[model_precipbox_tx(1),model_precipbox_tx(0),0,0],$
                                        count=[model_npreciplon,model_npreciplat,1,model_count]))*(3.)
                                   
   model_zsing_updraught_massflux=REFORM(model_updraught_massflux(*,*,model_zsing,*))
   model_zsing_downdraught_massflux=REFORM(model_downdraught_massflux(*,*,model_zsing,*))
   model_nonzero_precip=model_precip[where(model_precip gt 0.001)]
   ;model_zsing_massflux_nonzero_precip=model_zsing_massflux[where(model_precip gt 0.001)]
   model_nonzero_precip_sorted=SORT(model_nonzero_precip)
   FOR j=0,n_bins-1 DO $
      hist_bins(i,j+1)=model_nonzero_precip(model_nonzero_precip_sorted(FLOOR(N_ELEMENTS(model_nonzero_precip)*j/FLOAT(n_bins))))
   ;print,hist_bins
 
   model_precip_indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)
   model_zsing_updraught_massflux_indices=VALUE_LOCATE(updraught_massflux_bins,model_zsing_updraught_massflux)
   model_zsing_downdraught_massflux_indices=VALUE_LOCATE(downdraught_massflux_bins,model_zsing_downdraught_massflux)
   FOR j=0,n_bins DO BEGIN
      FOR k=0,n_updraught_massflux_bins-1 DO $
         updraught_singlelvl_joint_pdf(i,k,j)=N_ELEMENTS(where(model_zsing_updraught_massflux_indices eq k and $
                                                               model_precip_indices eq j))
      FOR k=0,n_downdraught_massflux_bins-1 DO $
      downdraught_singlelvl_joint_pdf(i,k,j)=N_ELEMENTS(where(model_zsing_downdraught_massflux_indices eq k and $
                                                              model_precip_indices eq j))
   ENDFOR

   PDF,REFORM(model_zsing_updraught_massflux,[model_nlon*model_nlat*model_count]),xrange=[0,1],npdf=41,pdf=temp,/noplot
   updraught_singlelvl_pdf(i,*)=temp
   PDF,REFORM(model_zsing_downdraught_massflux,[model_nlon*model_nlat*model_count]),xrange=[0,0.20],npdf=41,pdf=temp,/noplot
   downdraught_singlelvl_pdf(i,*)=temp

   indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
      print,'Calculating for level '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(model_nz),1)
      thislev_updraught_massflux=REFORM(model_updraught_massflux(*,*,j,*))
      thislev_downdraught_massflux=REFORM(model_downdraught_massflux(*,*,j,*))
      FOR k=0,n_bins DO BEGIN
         IF TOTAL(where(indices eq k)) gt 0 THEN BEGIN
            binned_model_updraught_massflux(i,k,j)=MEAN(thislev_updraught_massflux[where(indices eq k)],/NaN)
            binned_model_downdraught_massflux(i,k,j)=MEAN(thislev_downdraught_massflux[where(indices eq k)],/NaN)
            bins_npoints(i,k)=N_ELEMENTS(where(indices eq k))
         ENDIF
      ENDFOR
   ENDFOR
   model_legend_labels(i)=description
   model_runids(i)=runid
ENDFOR

;precip_legend_labels=strarr(n_bins)
;FOR i=0,n_bins-2 DO $
;   precip_legend_labels(i)=STRMID(STRTRIM(STRING(hist_bins(i)),1),0,4)+' mm hr!U-1!N <= precip rate < '+$
;   STRMID(STRTRIM(STRING(hist_bins(i+1)),1),0,4)+' mm hr!U-1!N'
;precip_legend_labels(n_bins-1)=STRMID(STRTRIM(STRING(hist_bins(n_bins-1)),1),0,4)+' mm hr!U-1!N <= precip rate'

FOR i=0,n_models-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.'+model_runids(i)+'_updraught.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(updraught_massflux_levels)+1
   LEVS,MANUAL=updraught_massflux_levels
   CON,X=indgen(n_bins+1)+0.5,Y=thlvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_updraught_massflux(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Updraught massflux (Pa s!U-1!N) binned by precip - 5S-5N, 70E-90E - first 5 days of run only - timestep data',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],YVALS=indgen(18)*(-50)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.'+model_runids(i)+'_downdraught.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(downdraught_massflux_levels)+1
   LEVS,MANUAL=downdraught_massflux_levels
   CON,X=indgen(n_bins+1)+0.5,Y=thlvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_downdraught_massflux(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Downdraught massflux (Pa s!U-1!N) binned by precip - 5S-5N, 70E-90E - first 5 days of run only - timestep data',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],YVALS=indgen(18)*(-50)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.'+model_runids(i)+'_updraught_singlelvl_'+$
          STRTRIM(STRING(single_level),1)+'hPa.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_updraught_massflux_bins,YMIN=0,YMAX=n_bins+1
   pdf_levs=['0.005','0.010','0.015','0.020','0.025','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.15','0.20','0.50']
   CS,SCALE=30,NCOLS=N_ELEMENTS(pdf_levs)+1,white=[2]
   LEVS,MANUAL=pdf_levs
   FOR j=0,n_bins DO $
      updraught_singlelvl_joint_pdf(i,*,j)=updraught_singlelvl_joint_pdf(i,*,j)/TOTAL(updraught_singlelvl_joint_pdf(i,*,j))   
   CON,X=indgen(n_updraught_massflux_bins)+0.5,Y=indgen(n_bins+1)+0.5,FIELD=REFORM(updraught_singlelvl_joint_pdf(i,*,*)),/BLOCK,/NOLINES,$
       TITLE='Joint PDF of updraught massflux and precipitation rate at '+STRTRIM(STRING(single_level),1)+' hPa - 5S-5N, 70E-90E - first 5 days only'
   AXES,XVALS=indgen(n_updraught_massflux_bins+1),XLABELS=[STRMID(STRTRIM(STRING(updraught_massflux_bins),1),0,6),$
                                                           '>'+STRMID(STRTRIM(STRING(updraught_massflux_bins(n_updraught_massflux_bins-1)),1),0,6)],$
        YVALS=indgen(n_bins+2),YLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],$
        XTITLE='Updraught massflux from convection (Pa s!U-1!N)',YTITLE='Precipitation rate (mm hr!U-1!N)',NDECS=3,$
        ORIENTATION=40
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.'+model_runids(i)+'_downdraught_singlelvl_'+$
          STRTRIM(STRING(single_level),1)+'hPa.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_downdraught_massflux_bins,YMIN=0,YMAX=n_bins+1
   pdf_levs=['0.005','0.010','0.015','0.020','0.025','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.15','0.20','0.50']
   CS,SCALE=30,NCOLS=N_ELEMENTS(pdf_levs)+1,white=[2]
   LEVS,MANUAL=pdf_levs
   FOR j=0,n_bins DO $
      downdraught_singlelvl_joint_pdf(i,*,j)=downdraught_singlelvl_joint_pdf(i,*,j)/TOTAL(downdraught_singlelvl_joint_pdf(i,*,j))   
   CON,X=indgen(n_downdraught_massflux_bins)+0.5,Y=indgen(n_bins+1)+0.5,FIELD=REFORM(downdraught_singlelvl_joint_pdf(i,*,*)),/BLOCK,/NOLINES,$
       TITLE='Joint PDF of downdraught massflux and precipitation rate at '+STRTRIM(STRING(single_level),1)+' hPa - 5S-5N, 70E-90E - first 5 days only'
   AXES,XVALS=indgen(n_downdraught_massflux_bins+1),XLABELS=[STRMID(STRTRIM(STRING(downdraught_massflux_bins),1),0,6),$
                                                           '>'+STRMID(STRTRIM(STRING(downdraught_massflux_bins(n_downdraught_massflux_bins-1)),1),0,6)],$
        YVALS=indgen(n_bins+2),YLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],$
        XTITLE='Downdraught massflux from convection (Pa s!U-1!N)',YTITLE='Precipitation rate (mm hr!U-1!N)',NDECS=3,$
        ORIENTATION=40
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.updraught_singlelvl_'+$
       STRTRIM(STRING(single_level),1)+'hPa_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=80
GSET,XMIN=0.,XMAX=1,YMIN=0.0001,YMAX=0.01,/YLOG
colors=['blue','red']
IF TOTAL(where(updraught_singlelvl_pdf le 0.0001)) gt 0 THEN $
   updraught_singlelvl_pdf[where(updraught_singlelvl_pdf le 0.0001)]=!Values.F_NaN
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(41)*(1.0/40.),Y=REFORM(updraught_singlelvl_pdf(i,*)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=0.1,YVALS=['0.0001','0.00015','0.0002','0.0003','0.0005','0.0007','0.001','0.0015','0.002','0.003','0.005','0.007',$
                      '0.01'],NDECS=4,XTITLE='Updraught massflux (Pa s!U-1!N)',YTITLE='Probability density'
LEGEND,labels=REVERSE(model_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux_twodhist.downdraught_singlelvl_'+$
       STRTRIM(STRING(single_level),1)+'hPa_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1500,YOFFSET=1500,TFONT=2,TCHARSIZE=80
GSET,XMIN=0.,XMAX=0.2,YMIN=0.000001,YMAX=0.002,/YLOG
colors=['blue','red']
IF TOTAL(where(downdraught_singlelvl_pdf le 0.000001)) gt 0 THEN $
   downdraught_singlelvl_pdf[where(downdraught_singlelvl_pdf le 0.000001)]=!Values.F_NaN
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(41)*(0.20/40.),Y=REFORM(downdraught_singlelvl_pdf(i,*)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=0.01,YVALS=['0.000001','0.0000015','0.000002','0.000003','0.000005','0.000007',$
                       '0.00001','0.000015','0.00002','0.00003','0.00005','0.00007','0.0001','0.00015','0.0002','0.0003',$
                       '0.0005','0.0007','0.001','0.0015'],$
     NDECS=4,XTITLE='Downdraught massflux (Pa s!U-1!N)',YTITLE='Probability density'
LEGEND,labels=REVERSE(model_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1
PSCLOSE

STOP

END

