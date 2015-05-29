PRO hadgem3kpp_cascade_apr09_precip_bin_massflux

; Box to read (longitude to plot; latitude for averaging)
box=[-5,40,5,200]

total_levels=['-0.35','-0.30','-0.25','-0.20','-0.16','-0.12','-0.09','-0.06','-0.04','-0.02','-0.01',$
              '0.01','0.02','0.04','0.06','0.09','0.12','0.16','0.20','0.25','0.30','0.35']
;hist_bins=[0,0.05,0.1,0.2,0.4,0.7,1.0,2.0,4.0,7.0,10.0]
hist_bins=[0,0.02,0.10,0.30,0.6,1.2]
n_bins=N_ELEMENTS(hist_bins)

n_models=1
um3_dir='/home/ss901165/um_output3'
updraught_massflux_file_suffix='a.06apr09-10apr09.updraught_massflux_thlvl.nc'
downdraught_massflux_file_suffix='a.06apr09-10apr09.downdraught_massflux_thlvl.nc'
thlvl_pres_file_suffix='a.06apr09-10apr09.p_thlvl.nc'
precip_file_suffix='a.06apr09-10apr09.cvrain.nc'
FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         runid='xfadu'
         dirname='xfadu'
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

   updraught_massflux_file=um3_dir+'/'+dirname+'/'+runid+updraught_massflux_file_suffix
   downdraught_massflux_file=um3_dir+'/'+dirname+'/'+runid+downdraught_massflux_file_suffix
   precip_file=um3_dir+'/'+dirname+'/'+runid+precip_file_suffix   
   model_nz=NEAREST(thlvl_pres_mean,150)+1
   IF i eq 0 THEN BEGIN
      binned_model_updraught_massflux=fltarr(n_models,n_bins,model_nz)
      binned_model_downdraught_massflux=fltarr(n_models,n_bins,model_nz)
      bins_npoints=fltarr(n_models,n_bins)
      model_legend_labels=strarr(n_models)      
   ENDIF
   model_preciplat=OPEN_AND_EXTRACT(precip_file,precip_latname)
   model_preciplon=OPEN_AND_EXTRACT(precip_file,'longitude')
   DEFINE_BOUNDARIES,box,model_preciplat,model_preciplon,model_precipbox_tx,/LIMIT
   model_npreciplat=N_ELEMENTS(model_preciplat)
   model_npreciplon=N_ELEMENTS(model_preciplon)
   
   print,'Reading updraught mass flux ...'
   model_updraught_massflux=REFORM(OPEN_AND_EXTRACT(updraught_massflux_file,'updraught_massflux_thlvl',$
                                            offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                            count=[model_nlon,model_nlat,model_nz,model_count]))

   print,'Reading downdraught mass flux ...'
   model_downdraught_massflux=REFORM(OPEN_AND_EXTRACT(downdraught_massflux_file,'downdraught_massflux_thlvl',$
                                            offset=[model_box_tx(1),model_box_tx(0),0,0],$
                                            count=[model_nlon,model_nlat,model_nz,model_count]))

   print,'Reading precipitation ...'
   model_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'cvrain',$
                                        offset=[model_precipbox_tx(1),model_precipbox_tx(0),0,0],$
                                        count=[model_npreciplon,model_npreciplat,1,model_count]))*(3.)

   indices=VALUE_LOCATE(hist_bins,model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
      print,'Calculating for level '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(model_nz),1)
      thislev_updraught_massflux=REFORM(model_updraught_massflux(*,*,j,*))
      thislev_downdraught_massflux=REFORM(model_downdraught_massflux(*,*,j,*))      
      FOR k=0,n_bins-1 DO BEGIN
         IF TOTAL(where(indices eq k)) gt 0 THEN BEGIN
            binned_model_updraught_massflux(i,k,j)=MEAN(thislev_updraught_massflux[where(indices eq k)],/NaN)
            binned_model_downdraught_massflux(i,k,j)=MEAN(thislev_downdraught_massflux[where(indices eq k)],/NaN)
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

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux.updraught.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=0,XMAX=1.60,YMIN=1005,YMAX=140
colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(thlvl_pres_mean,500)+1
model_top=model_nz
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_updraught_massflux(j,i,0:model_top-1)),Y=thlvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
GPLOT,X=-0.1,Y=125,TEXT='Updraught massflux binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration - timestep output',ALIGN=0.0
AXES,XSTEP=0.20,XMINOR=0.10,YVALS=indgen(18)*(-50)+1000,NDECS=2,$
     XTITLE='Updraught massflux (m s!U-1!N 6hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE

STOP

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_massflux.downdraught.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
GSET,XMIN=-0.02,XMAX=0.02,YMIN=1005,YMAX=140
colors=['brown','red','orange','green','blue','cyan']
;model_top=NEAREST(thlvl_pres_mean,500)+1
model_top=model_nz
FOR i=0,n_bins-1 DO BEGIN
   FOR j=0,n_models-1 DO $
      GPLOT,X=REFORM(binned_model_downdraught_massflux(j,i,0:model_top-1)),Y=thlvl_pres_mean(0:model_top-1),STYLE=REFORM(j(0)),COL=FSC_COLOR(colors(i))
ENDFOR
GPLOT,X=[0,0],Y=[1005,140],STYLE=0,THICK=70
GPLOT,X=-0.02,Y=125,TEXT='Downdraught massflux binned by precipitation - 5S-5N, 40E-200E - first 5 days of integration - timestep output',ALIGN=0.0
AXES,XSTEP=0.005,XMINOR=0.0025,YVALS=indgen(18)*(-50)+1000,NDECS=3,$
     XTITLE='Downdraught massflux (m s!U-1!N hr!U-1!N)',YTITLE='Pressure (hPa)'
LEGEND,labels=REVERSE(precip_legend_labels),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=2
LEGEND,labels=REVERSE(model_legend_labels),COL=REPLICATE(FSC_COLOR("black"),n_models),STYLE=REVERSE(indgen(n_models)),LEGPOS=9
PSCLOSE

STOP
END

