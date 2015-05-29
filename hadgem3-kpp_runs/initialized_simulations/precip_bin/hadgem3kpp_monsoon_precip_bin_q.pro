PRO hadgem3kpp_monsoon_precip_bin_q

; Box to read (longitude to plot; latitude for averaging)
box=[-5,40,5,200]

mylevs=['0.01','0.02','0.04','0.07','0.1','0.2','0.4','0.7','1','2','4','7','10','12','15','20']
mylevs_anom=['-4','-2','-1','-0.5','-0.25','-0.13','-0.06','-0.03','-0.01','0.01','0.03','0.06','0.13','0.25','0.5','1','2','4']

; Get ERA-Interim winds
eraint_file='/home/ss901165/datasets_mango/ERA-INTERIM/Q/Q.jan-dec_dmeans.2009.mjo_domain.n96.nc'
trmm_file='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.2009.n96.nc'
eraint_time_offset=95
eraint_count=30
eraint_lat=OPEN_AND_EXTRACT(eraint_file,'latitude')
eraint_lon=OPEN_AND_EXTRACT(eraint_file,'longitude')
eraint_z=OPEN_AND_EXTRACT(eraint_file,'p')
DEFINE_BOUNDARIES,box,eraint_lat,eraint_lon,eraint_box_tx,/LIMIT
eraint_nlat=N_ELEMENTS(eraint_lat)
eraint_nlon=N_ELEMENTS(eraint_lon)
eraint_nz=NEAREST(eraint_z,150)+1
;eraint_zcount=NEAREST(eraint_z,200)

trmm_lat=OPEN_AND_EXTRACT(trmm_file,'latitude')
trmm_lon=OPEN_AND_EXTRACT(trmm_file,'longitude')
DEFINE_BOUNDARIES,box,trmm_lat,trmm_lon,trmm_box_tx,/LIMIT
trmm_nlat=N_ELEMENTS(trmm_lat)
trmm_nlon=N_ELEMENTS(trmm_lon)
trmm_time_offset=95
trmm_count=30

eraint_q=OPEN_AND_EXTRACT(eraint_file,'Q',$
                          offset=[eraint_box_tx(1),eraint_box_tx(0),0,eraint_time_offset],$
                          count=[eraint_nlon,eraint_nlat,eraint_nz,eraint_count])*1000.
trmm_precip=OPEN_AND_EXTRACT(trmm_file,'precip',$
                             offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_time_offset],$
                             count=[trmm_nlon,trmm_nlat,trmm_count])

hist_bins=[0,1,2,5,10,20]
n_bins=N_ELEMENTS(hist_bins)
indices=VALUE_LOCATE(hist_bins,trmm_precip)
binned_eraint_q=fltarr(n_bins,eraint_nz)
bins_npoints=fltarr(n_bins)
FOR i=0,eraint_nz-1 DO BEGIN
   thislev_q=REFORM(eraint_q(*,*,i,*))
   FOR j=0,n_bins-1 DO BEGIN
      IF TOTAL(where(indices eq j)) gt 0 THEN BEGIN
         binned_eraint_q(j,i)=MEAN(thislev_q[where(indices eq j)])
         bins_npoints(j)=N_ELEMENTS(where(indices eq j))
      ENDIF
   ENDFOR
ENDFOR

n_models=2
um3_dir='/home/ss901165/um_output3'
q_file_suffix='a.pdk9560.nc'
precip_file_suffix='a.pak9560.nc'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runid='xfadh'
         dirname='xfadh.old_stash'
         description='Time-varying OSTIA SSTs (xfadh)'
      END
      1 : BEGIN
         runid='xfadw'
         dirname='xfadw'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk)'
      END
   ENDCASE
   
   print,runid
   q_input_file=um3_dir+'/'+dirname+'/'+runid+q_file_suffix
   precip_input_file=um3_dir+'/'+dirname+'/'+runid+precip_file_suffix
   model_lat=OPEN_AND_EXTRACT(q_input_file,'latitude')
   model_lon=OPEN_AND_EXTRACT(q_input_file,'longitude')
   model_z=OPEN_AND_EXTRACT(q_input_file,'p')
   DEFINE_BOUNDARIES,box,model_lat,model_lon,model_box_tx,/LIMIT
   model_nlat=N_ELEMENTS(model_lat)
   model_nlon=N_ELEMENTS(model_lon)
   model_nz=NEAREST(model_z,150)+1
   IF i eq 0 THEN BEGIN
      binned_model_q=fltarr(n_models,n_bins,model_nz)
      bins_npoints=fltarr(n_models,n_bins)
      model_legend_labels=strarr(n_models)      
   ENDIF
   model_count=30
   
   model_q=OPEN_AND_EXTRACT(q_input_file,'q',$
                            offset=[model_box_tx(1),model_box_tx(0),0,0],$
                            count=[model_nlon,model_nlat,model_nz,model_count])*1000.   
   model_q[where(model_q eq 0)]=!Values.F_NaN

   model_precip=REFORM(OPEN_AND_EXTRACT(precip_input_file,'precip',$
                                 offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                        count=[model_nlon,model_nlat,1,model_count]))*86400.

   indices=VALUE_LOCATE(hist_bins,model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
      thislev_q=REFORM(model_q(*,*,j,*))
      FOR k=0,n_bins-1 DO BEGIN
         IF TOTAL(where(indices eq k)) gt 0 THEN BEGIN
            binned_model_q(i,k,j)=MEAN(thislev_q[where(indices eq k)],/NaN)
            bins_npoints(i,k)=N_ELEMENTS(where(indices eq k))
         ENDIF
      ENDFOR
   ENDFOR
   model_legend_labels(i)=description
ENDFOR

precip_legend_labels=strarr(n_bins)
FOR i=0,n_bins-2 DO $
   precip_legend_labels(i)=STRTRIM(STRING(hist_bins(i)),1)+' mm day!U-1!N <= precip rate < '+STRTRIM(STRING(hist_bins(i+1)),1)+' mm day!U-1!N'
precip_legend_labels(n_bins-1)=STRTRIM(STRING(hist_bins(n_bins-1)),1)+' mm day!U-1!N <= precip rate'

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_q.lower.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=80
GSET,XMIN=1.0,XMAX=18,YMIN=1000,YMAX=500
colors=['brown','red','orange','green','blue','cyan']
eraint_top=NEAREST(eraint_z,500)+1
model_top=NEAREST(model_z,500)+1
FOR i=0,n_bins-1 DO BEGIN
   GPLOT,X=REFORM(binned_eraint_q(i,0:eraint_top-1)),Y=eraint_z(0:eraint_top-1),STYLE=0,COL=FSC_COLOR(colors(i))
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_q(j,i,0:model_top-1)),Y=model_z(0:model_top-1),STYLE=j+1,COL=FSC_COLOR(colors(i))
ENDFOR
AXES,XSTEP=2,YSTEP=-50,XMINOR=1,YMINOR=-25,NDECS=2,XTITLE='Specific humidity (g kg!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
LEGEND,labels=REVERSE(['ERA-Interim at N96',model_legend_labels]),COL=REPLICATE(FSC_COLOR("black"),n_models+1),STYLE=REVERSE(indgen(n_models+1)),LEGPOS=9
PSCLOSE,/NOVIEW

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_q.diff_lower.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=500,TFONT=2,TCHARSIZE=80
GSET,XMIN=-2.0,XMAX=1.0,YMIN=1000,YMAX=500
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

