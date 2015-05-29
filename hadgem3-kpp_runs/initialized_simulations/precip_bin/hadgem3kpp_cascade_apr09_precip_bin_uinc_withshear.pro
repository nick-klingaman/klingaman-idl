PRO hadgem3kpp_cascade_apr09_precip_bin_uinc_withshear

; Box to read (consider all gridpoints within)
box=[-5,60,5,80]

total_levels=['-0.35','-0.30','-0.25','-0.20','-0.16','-0.12','-0.09','-0.06','-0.04','-0.02','-0.01',$
              '0.01','0.02','0.04','0.06','0.09','0.12','0.16','0.20','0.25','0.30','0.35']
;hist_bins=[0,0.05,0.1,0.2,0.4,0.7,1.0,2.0,4.0,7.0,10.0]
;hist_bins=[0,0.02,0.10,0.30,0.6,1.2]
hist_bins=[0,0.02,0.05,0.10,0.30,0.60]
n_bins=N_ELEMENTS(hist_bins)

n_models=2
um3_dir='/home/ss901165/um_output3'
u_file_suffix='a.06apr09-05may09.u_rholvl.nc'
uinc_total_file_suffix='a.06apr09-05may09.uinc_total.nc'
uinc_conv_file_suffix='a.06apr09-05may09.uinc_conv.nc'
uinc_advect_file_suffix='a.06apr09-05may09.uinc_advect.nc'
uinc_bdylr_file_suffix='a.06apr09-05may09.uinc_bdylr.nc'
rholvl_pres_file_suffix='a.06apr09-05may09.p_rholvl.nc'
precip_file_suffix='a.06apr09-05may09.precip.nc'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      1 : BEGIN
         runid='xfadh'
         dirname='xfadh'
         description='Time-varying OSTIA SSTs (xfadh)'
      END
      0 : BEGIN
         runid='xfadk'
         dirname='xfadk'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk)'
      END
   ENDCASE
   print,runid
   model_count=24

   rholvl_pres_file=um3_dir+'/'+dirname+'/'+runid+rholvl_pres_file_suffix   
   model_lon=OPEN_AND_EXTRACT(rholvl_pres_file,'longitude')
   model_lat=OPEN_AND_EXTRACT(rholvl_pres_file,'latitude_1')
   model_nz=N_ELEMENTS(OPEN_AND_EXTRACT(rholvl_pres_file,'hybrid_ht_1'))
   DEFINE_BOUNDARIES,box,model_lat,model_lon,model_box_tx,/LIMIT
   model_nlat=N_ELEMENTS(model_lat)
   model_nlon=N_ELEMENTS(model_lon)
   
   print,'Reading pressure of model rho levels ...'
   rholvl_pres=REFORM(OPEN_AND_EXTRACT(rholvl_pres_file,'p_rholvl',$
                                       offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                       count=[model_nlon,model_nlat,model_nz,model_count]))/100.
   rholvl_pres_mean=fltarr(model_nz)
   FOR j=0,model_nz-1 DO $
      rholvl_pres_mean(j)=MEAN(rholvl_pres(*,*,j,*))

   u_file=um3_dir+'/'+dirname+'/'+runid+u_file_suffix
   uinc_total_file=um3_dir+'/'+dirname+'/'+runid+uinc_total_file_suffix
   uinc_conv_file=um3_dir+'/'+dirname+'/'+runid+uinc_conv_file_suffix
   uinc_advect_file=um3_dir+'/'+dirname+'/'+runid+uinc_advect_file_suffix
   uinc_bdylr_file=um3_dir+'/'+dirname+'/'+runid+uinc_bdylr_file_suffix
   precip_file=um3_dir+'/'+dirname+'/'+runid+precip_file_suffix
   model_ulat=OPEN_AND_EXTRACT(uinc_total_file,'latitude')
   model_ulon=OPEN_AND_EXTRACT(uinc_total_file,'longitude_1')
   model_z=OPEN_AND_EXTRACT(uinc_total_file,'hybrid_ht_1')
   DEFINE_BOUNDARIES,box,model_ulat,model_ulon,model_ubox_tx,/LIMIT
   model_nulat=N_ELEMENTS(model_ulat)
   model_nulon=N_ELEMENTS(model_ulon)
   model_nz=NEAREST(rholvl_pres_mean,150)+1
   model_z925=NEAREST(rholvl_pres_mean,850)
   model_z500=NEAREST(rholvl_pres_mean,500)
   IF i eq 0 THEN BEGIN
      binned_model_uinc_total=fltarr(n_models,n_bins,model_nz)
      binned_model_uinc_conv=fltarr(n_models,n_bins,model_nz)
;      binned_model_uinc_conv_eshear=fltarr(n_models,n_bins,model_nz)
;      binned_model_uinc_conv_wshear=fltarr(n_models,n_bins,model_nz)
      binned_model_uinc_advect=fltarr(n_models,n_bins,model_nz)
      binned_model_uinc_bdylr=fltarr(n_models,n_bins,model_nz)
      bins_npoints=fltarr(n_models,n_bins)
      model_legend_labels=strarr(n_models)      
   ENDIF
   
   print,'Reading total zonal wind increments ...'
   model_uinc_total=REFORM(OPEN_AND_EXTRACT(uinc_total_file,'uinc_total',$
                                            offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                            count=[model_nulon,model_nulat,model_nz,model_count]))*6.
   print,'Reading convective zonal wind increments ...'
   model_uinc_conv=REFORM(OPEN_AND_EXTRACT(uinc_conv_file,'uinc_conv',$
                                           offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                           count=[model_nulon,model_nulat,model_nz,model_count]))
   print,'Reading advective zonal wind increments ...'
   model_uinc_advect=REFORM(OPEN_AND_EXTRACT(uinc_advect_file,'uinc_advect',$
                                             offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                             count=[model_nulon,model_nulat,model_nz,model_count]))*6.
   print,'Reading boundary-layer zonal wind increments ...'
   model_uinc_bdylr=REFORM(OPEN_AND_EXTRACT(uinc_bdylr_file,'uinc_bdylr',$
                                            offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                            count=[model_nulon,model_nulat,model_nz,model_count]))
   print,'Reading precipitation ...'
   model_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'precip',$
                                        offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                        count=[model_nlon,model_nlat,1,model_count]))*(3600.)
   print,'Reading zonal wind ...'
   model_u=REFORM(OPEN_AND_EXTRACT(u_file,'u_rholvl',$
                                   offset=[model_ubox_tx(1),model_box_tx(0),0,0],$
                                   count=[model_nlon,model_nlat,model_nz,model_count]))

   model_u925=REFORM(model_u(*,*,model_z925,*))
   model_u500=REFORM(model_u(*,*,model_z500,*))
   model_u925_u500_shear=model_u925-model_u500
   shear_bins=[-50,MEDIAN(model_u925_u500_shear)]
   ;shear_bins=[-50,0]
   shear_indices=VALUE_LOCATE(shear_bins,model_u925_u500_shear)
   model_wshear_mean=fltarr(model_nz)
   model_eshear_mean=fltarr(model_nz)
   model_wshear_stddev=fltarr(model_nz)
   model_eshear_stddev=fltarr(model_nz)
   FOR j=0,model_nz-1 DO BEGIN
      thislev_model_u=REFORM(model_u(*,*,j,*))
      model_wshear_mean(j)=MEAN(thislev_model_u[where(shear_indices eq 0)])
      model_eshear_mean(j)=MEAN(thislev_model_u[where(shear_indices eq 1)])
      model_wshear_stddev(j)=STDDEV(thislev_model_u[where(shear_indices eq 0)])
      model_eshear_stddev(j)=STDDEV(thislev_model_u[where(shear_indices eq 1)])
   ENDFOR
   
   indices=VALUE_LOCATE(hist_bins,model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
      print,'Calculating for level '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(model_nz),1)
      thislev_uinc_total=REFORM(model_uinc_total(*,*,j,*))
      thislev_uinc_conv=REFORM(model_uinc_conv(*,*,j,*))
      thislev_uinc_bdylr=REFORM(model_uinc_bdylr(*,*,j,*))
      thislev_uinc_advect=REFORM(model_uinc_advect(*,*,j,*))
      FOR k=0,n_bins-1 DO BEGIN
         IF TOTAL(where(indices eq k)) gt 0 THEN BEGIN
            binned_model_uinc_total(i,k,j)=MEAN(thislev_uinc_total[where(indices eq k)],/NaN)
            binned_model_uinc_conv(i,k,j)=MEAN(thislev_uinc_conv[where(indices eq k)],/NaN)
                                ;binned_model_uinc_conv_wshear(i,k,j)=MEAN(thislev_uinc_conv[where(indices eq k and shear_indices eq 0)],/NaN)
                                ;binned_model_uinc_conv_eshear(i,k,j)=MEAN(thislev_uinc_conv[where(indices eq k and shear_indices eq 1)],/NaN)
            binned_model_uinc_bdylr(i,k,j)=MEAN(thislev_uinc_bdylr[where(indices eq k)],/NaN)
            binned_model_uinc_advect(i,k,j)=MEAN(thislev_uinc_advect[where(indices eq k)],/NaN)
            bins_npoints(i,k)=N_ELEMENTS(where(indices eq k))
         ENDIF
      ENDFOR
   ENDFOR
   model_legend_labels(i)=description
ENDFOR

precip_legend_labels=strarr(n_bins)
FOR i=0,n_bins-2 DO $
   precip_legend_labels(i)=STRMID(STRTRIM(STRING(hist_bins(i)),1),0,4)+' mm hr!U-1!N <= precip rate < '+$
   STRMID(STRTRIM(STRING(hist_bins(i+1)),1),0,4)+' mm hr!U-1!N'
precip_legend_labels(n_bins-1)=STRMID(STRTRIM(STRING(hist_bins(n_bins-1)),1),0,4)+' mm hr!U-1!N <= precip rate'

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.total.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=-0.32,XMAX=0.64,YMIN=1005,YMAX=140
colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(rholvl_pres_mean,500)+1
model_top=model_nz
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_uinc_total(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
GPLOT,X=-0.1,Y=125,TEXT='Zonal-wind increment binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
AXES,XSTEP=0.08,XMINOR=0.04,YVALS=indgen(18)*(-50)+1000,NDECS=2,$
     XTITLE='Total zonal wind increment (m s!U-1!N 6hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.conv.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=-0.06,XMAX=0.06,YMIN=1010,YMAX=140
colors=['brown','red','orange','green','blue','cyan']
model_top=model_nz
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_uinc_conv(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
GPLOT,X=-0.02,Y=125,TEXT='Zonal-wind increment from convection binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
AXES,XSTEP=0.02,XMINOR=0.01,YVALS=indgen(18)*(-50)+1000,NDECS=3,$
     XTITLE='Zonal wind increment from convection (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE

;psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.conv_eshear.ps'
;PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
;GSET,XMIN=-0.10,XMAX=0.10,YMIN=1005,YMAX=140
;colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(rholvl_pres_mean,500)+1
;model_top=model_nz
;FOR i=0,n_bins-1 DO BEGIN
;   FOR j=0,n_models-1 DO $
;      GPLOT,X=REFORM(binned_model_uinc_conv_eshear(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
;ENDFOR
;GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
;GPLOT,X=-0.02,Y=125,TEXT='Zonal-wind increment from convection binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
;AXES,XSTEP=0.02,XMINOR=0.01,YVALS=indgen(18)*(-50)+1000,NDECS=3,$
;     XTITLE='Zonal wind increment from convection (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
;LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
;LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
;PSCLOSE

;psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.conv_wshear.ps'
;PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
;GSET,XMIN=-0.10,XMAX=0.10,YMIN=1005,YMAX=140
;colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(rholvl_pres_mean,500)+1
;model_top=model_nz
;FOR i=0,n_bins-1 DO BEGIN
;   FOR j=0,n_models-1 DO $
;      GPLOT,X=REFORM(binned_model_uinc_conv_wshear(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
;ENDFOR
;GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
;GPLOT,X=-0.02,Y=125,TEXT='Zonal-wind increment from convection binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
;AXES,XSTEP=0.02,XMINOR=0.01,YVALS=indgen(18)*(-50)+1000,NDECS=3,$
;     XTITLE='Zonal wind increment from convection (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
;LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
;LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
;PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.advect.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=-0.32,XMAX=0.64,YMIN=1010,YMAX=140
colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(rholvl_pres_mean,500)+1
model_top=model_nz
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_uinc_advect(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1010,140],STYLE=0,THICK=70
GPLOT,X=-0.15,Y=125,TEXT='Zonal-wind increment from advection binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
AXES,XSTEP=0.08,XMINOR=0.04,YVALS=indgen(18)*(-50)+1000,NDECS=2,$
     XTITLE='Zonal wind increment from advection (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc.bdylr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=-0.10,XMAX=0.01,YMIN=1010,YMAX=800
colors=['brown','red','orange','green','blue','cyan']
model_top=NEAREST(rholvl_pres_mean,800)
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_uinc_bdylr(j,i,0:model_top-1)),Y=rholvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1010,800],STYLE=0,THICK=70
GPLOT,X=-0.01,Y=795,TEXT='Zonal-wind increment from boundary-layer binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration only',ALIGN=0.0
AXES,XSTEP=0.02,XMINOR=0.01,YVALS=indgen(11)*(-20)+1000,NDECS=3,$
     XTITLE='Zonal wind increment from boundary-layer (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE,/NOVIEW

STOP

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_q.diff_lower.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=80
GSET,XMIN=-2.0,XMAX=1.0,YMIN=1005,YMAX=500
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_q(j,i,0:model_top-1))-REFORM(binned_eraint_q(i,0:eraint_top-1)),Y=model_z(0:model_top-1),STYLE=j+1,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XSTEP=0.2,XMINOR=0.10,YMINOR=-25,YSTEP=-50,NDECS=2,XTITLE='Difference in specific humidity from ERA-Interim (g kg!U-1!N)',YTITLE='Pressure (hPa)'
GPLOT,X=[0,0],Y=[1000,500],STYLE=0,THICK=70
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)+1),LEGPOS=1
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_q.upper.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=80
GSET,XMIN=0.008,XMAX=5,YMIN=500,YMAX=150,/XLOG
colors=['brown','red','orange','green','blue','cyan']
eraint_top=NEAREST(eraint_z,500)+1
model_top=NEAREST(model_z,500)+1
FOR i=0,n_bins-1 DO BEGIN
   GPLOT,X=REFORM(binned_eraint_q(i,eraint_top-1:eraint_nz-1)),Y=eraint_z(eraint_top-1:eraint_nz-1),STYLE=0,COL=FSC_COLOR(colors(i))
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_q(j,i,model_top-1:model_nz-1)),Y=model_z(model_top-1:model_nz-1),STYLE=j+1,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XVALS=['0.01','0.015','0.02','0.03','0.04','0.07','0.10','0.15','0.20','0.30','0.40','0.70','1.00','1.50','2.00','3.00','4.00'],YSTEP=-50,YMINOR=-25,NDECS=2,$
     XTITLE='Specific humidity (g kg!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
LEGEND,labels=REVERSE(['ERA-Interim at N96',model_legend_labels]),COL=REPLICATE(FSC_COLOR("black"),n_models+1),STYLE=REVERSE(indgen(n_models+1)),LEGPOS=9
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_q.diff_upper.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=80
GSET,XMIN=-0.8,XMAX=0.5,YMIN=500,YMAX=150
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_q(j,i,model_top-1:model_nz-1))-REFORM(binned_eraint_q(i,eraint_top-1:eraint_nz-1)),$
            Y=model_z(model_top-1:model_nz-1),STYLE=j+1,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XSTEP=0.1,XMINOR=0.05,YMINOR=-25,YSTEP=-50,NDECS=2,XTITLE='Difference in specific humidity from ERA-Interim (g kg!U-1!N)',YTITLE='Pressure (hPa)'
GPLOT,X=[0,0],Y=[500,150],STYLE=0,THICK=70
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)+1),LEGPOS=1
PSCLOSE

STOP
END

