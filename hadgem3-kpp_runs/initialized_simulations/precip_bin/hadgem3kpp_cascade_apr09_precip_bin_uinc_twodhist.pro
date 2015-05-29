PRO hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist

; Box to read (consider all gridpoints within)
box=[-5,70,5,90]

total_levels=['-0.21','-0.19','-0.17','-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
              '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15','0.17','0.19','0.21']
conv_levels=['-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13']
bdylr_levels=['-0.12','-0.10','-0.08','-0.06','-0.05','-0.04','-0.03','-0.02','-0.01','-0.005','0.005','0.01']
;hist_bins=[0,0.05,0.1,0.2,0.4,0.7,1.0,2.0,4.0,7.0,10.0]
;hist_bins=[0,0.02,0.10,0.30,0.6,1.2]
;hist_bins=indgen(31)*0.02
;n_bins=N_ELEMENTS(hist_bins)
n_bins=20
;uinc_conv_bins=[-0.04,-0.03,-0.025,-0.020,-0.017,-0.014,-0.011,-0.008,-0.005,-0.003,-0.001,$
;                0.001,0.003,0.005,0.008,0.011,0.014,0.017,0.02,0.025,0.03,0.04]
uinc_conv_bins=[-0.34,-0.30,-0.26,-0.22,-0.18,-0.14,-0.10,-0.06,-0.02,0.02,0.06,0.10,0.14,0.18,0.22,0.26,0.30,0.34]
n_uincconv_bins=N_ELEMENTS(uinc_conv_bins)
single_level=200

n_models=4
hist_bins=fltarr(n_models,n_bins+1)
hist_bins(*,0)=0
um3_dir='/home/ss901165/um_output3'

FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      1 : BEGIN
         runid='xfadh'
         dirname='xfadh'
         description='Time-varying OSTIA SSTs (xfadh) - hourly means'
         model_count=120
         model_incmult=1
         end_date='05may09'
         rholvl_latname='latitude_1'
         rholvl_zname='hybrid_ht_1'
         precip_latname='latitude'
      END
      0 : BEGIN
         runid='xfadk'
         dirname='xfadk'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk) - hourly means'
         model_count=120
         model_incmult=1
         end_date='05may09'
         rholvl_latname='latitude_1'
         rholvl_zname='hybrid_ht_1'
         precip_latname='latitude'
      END
      2 : BEGIN
         runid='xfadt'
         dirname='xfadt'
         description='T-vary OSTIA and 1.5x mix entrain (xfadk) - timestep output'
         model_count=360
         model_incmult=1
         end_date='10apr09'
         rholvl_latname='latitude'
         rholvl_zname='hybrid_ht'
         precip_latname='latitude_1'
      END
      3 : BEGIN
         runid='xfioz'
         dirname='xfioz'
         description='Time-varying OSTIA SSTs (xfadh) - timestep output'
         model_count=360
         model_incmult=1
         end_date='10apr09'
         rholvl_latname='latitude'
         rholvl_zname='hybrid_ht'
         precip_latname='latitude_1'
      END
   ENDCASE
   print,runid
   
   u_file_suffix='a.06apr09-'+end_date+'.u_rholvl.nc'
   uinc_total_file_suffix='a.06apr09-'+end_date+'.uinc_total.nc'
   uinc_conv_file_suffix='a.06apr09-'+end_date+'.uinc_conv.nc'
   uinc_advect_file_suffix='a.06apr09-'+end_date+'.uinc_advect.nc'
   uinc_bdylr_file_suffix='a.06apr09-'+end_date+'.uinc_bdylr.nc'
   rholvl_pres_file_suffix='a.06apr09-'+end_date+'.p_rholvl.nc'
   precip_file_suffix='a.06apr09-'+end_date+'.cvrain.nc'

   rholvl_pres_file=um3_dir+'/'+dirname+'/'+runid+rholvl_pres_file_suffix   
   model_lon=OPEN_AND_EXTRACT(rholvl_pres_file,'longitude')
   model_lat=OPEN_AND_EXTRACT(rholvl_pres_file,rholvl_latname)
   model_nz=N_ELEMENTS(OPEN_AND_EXTRACT(rholvl_pres_file,rholvl_zname))
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
   model_z925=NEAREST(rholvl_pres_mean,925)
   model_zsing=NEAREST(rholvl_pres_mean,single_level)
   model_z500=NEAREST(rholvl_pres_mean,500)
   IF i eq 0 THEN BEGIN
      binned_model_uinc_total=fltarr(n_models,n_bins+1,model_nz)
      binned_model_uinc_conv=fltarr(n_models,n_bins+1,model_nz)
      singlelvl_joint_pdf=fltarr(n_models,n_uincconv_bins,n_bins+1)
      singlelvl_pdf=fltarr(n_models,41)
;      binned_model_uinc_conv_eshear=fltarr(n_models,n_bins,model_nz)
;      binned_model_uinc_conv_wshear=fltarr(n_models,n_bins,model_nz)
      binned_model_uinc_advect=fltarr(n_models,n_bins+1,model_nz)
      binned_model_uinc_bdylr=fltarr(n_models,n_bins+1,model_nz)
      bins_npoints=fltarr(n_models,n_bins+1)
      model_runids=strarr(n_models)
      model_legend_labels=strarr(n_models)      
   ENDIF

   model_preciplat=OPEN_AND_EXTRACT(precip_file,precip_latname)
   model_preciplon=OPEN_AND_EXTRACT(precip_file,'longitude')
   DEFINE_BOUNDARIES,box,model_preciplat,model_preciplon,model_precipbox_tx,/LIMIT
   model_npreciplat=N_ELEMENTS(model_preciplat)
   model_npreciplon=N_ELEMENTS(model_preciplon)
   
   print,'Reading total zonal wind increments ...'
   model_uinc_total=REFORM(OPEN_AND_EXTRACT(uinc_total_file,'uinc_total',$
                                            offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                            count=[model_nulon,model_nulat,model_nz,model_count]))*model_incmult
   print,'Reading convective zonal wind increments ...'
   model_uinc_conv=REFORM(OPEN_AND_EXTRACT(uinc_conv_file,'uinc_conv',$
                                           offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                           count=[model_nulon,model_nulat,model_nz,model_count]))*model_incmult
   print,'Reading advective zonal wind increments ...'
   model_uinc_advect=REFORM(OPEN_AND_EXTRACT(uinc_advect_file,'uinc_advect',$
                                             offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                             count=[model_nulon,model_nulat,model_nz,model_count]))*model_incmult
   print,'Reading boundary-layer zonal wind increments ...'
   model_uinc_bdylr=REFORM(OPEN_AND_EXTRACT(uinc_bdylr_file,'uinc_bdylr',$
                                            offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
                                            count=[model_nulon,model_nulat,model_nz,model_count]))*model_incmult
   print,'Reading precipitation ...'
   model_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'cvrain',$
                                        offset=[model_precipbox_tx(1),model_precipbox_tx(0),0,0],$
                                        count=[model_npreciplon,model_npreciplat,1,model_count]))*(3.)
;   print,'Reading zonal wind ...'
;   model_u=REFORM(OPEN_AND_EXTRACT(u_file,'u_rholvl',$
;                                   offset=[model_ubox_tx(1),model_ubox_tx(0),0,0],$
;                                   count=[model_nulon,model_nulat,model_nz,model_count]))

;   hist_input=fltarr(2,model_nlon*model_nlat*model_count)
;   hist_input(0,*)=REFORM(model_uinc_conv(*,*,model_zsing,*),[model_nlon*model_nlat*model_count])
;   hist_input(1,*)=REFORM(model_precip,[model_nlon*model_nlat*model_count])

;   singlelvl_joint_pdf(i,*,*)=HIST_ND(hist_input,min=[-0.020,0],max=[0.020,0.25],nbins=[41,26])

   model_zsing_uinc_conv=REFORM(model_uinc_conv(*,*,model_zsing,*))
   model_nonzero_precip=model_precip[where(model_precip gt 0.001)]
   model_zsing_uinc_conv_nonzero_precip=model_zsing_uinc_conv[where(model_precip gt 0.001)]
   model_nonzero_precip_sorted=SORT(model_nonzero_precip)
   FOR j=0,n_bins-1 DO $
      hist_bins(i,j+1)=model_nonzero_precip(model_nonzero_precip_sorted(FLOOR(N_ELEMENTS(model_nonzero_precip)*j/FLOAT(n_bins))))
;   print,hist_bins
 
   model_precip_indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)
   model_zsing_uinc_conv_indices=VALUE_LOCATE(uinc_conv_bins,model_zsing_uinc_conv)
   FOR j=0,n_bins DO BEGIN
      FOR k=0,n_uincconv_bins-1 DO BEGIN
         singlelvl_joint_pdf(i,k,j)=N_ELEMENTS(where(model_zsing_uinc_conv_indices eq k and $
                                               model_precip_indices eq j))
      ENDFOR         
   ENDFOR
;   print,singlelvl_joint_pdf(0,21,*)

   PDF,REFORM(model_zsing_uinc_conv,[model_nulon*model_nulat*model_count]),xrange=[-0.2,0.2],npdf=41,pdf=temp,/noplot
   singlelvl_pdf(i,*)=temp
  
   indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
      ;print,'Calculating for level '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(model_nz),1)
      thislev_uinc_total=REFORM(model_uinc_total(*,*,j,*))
      thislev_uinc_conv=REFORM(model_uinc_conv(*,*,j,*))
      thislev_uinc_bdylr=REFORM(model_uinc_bdylr(*,*,j,*))
      thislev_uinc_advect=REFORM(model_uinc_advect(*,*,j,*))
      FOR k=0,n_bins DO BEGIN
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
   model_runids(i)=runid
ENDFOR

FOR i=0,n_models-1 DO BEGIN
;   precip_legend_labels=strarr(n_bins)
;   FOR j=0,n_bins-2 DO $
;      precip_legend_labels(k)=STRMID(STRTRIM(STRING(hist_bins(i)),1),0,4)+' mm hr!U-1!N <= precip rate < '+$
;      STRMID(STRTRIM(STRING(hist_bins(i+1)),1),0,4)+' mm hr!U-1!N'
;   precip_legend_labels(n_bins-1)=STRMID(STRTRIM(STRING(hist_bins(n_bins-1)),1),0,4)+' mm hr!U-1!N <= precip rate'

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.'+$
          model_runids(i)+'_total.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(total_levels)+1
   LEVS,MANUAL=total_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_uinc_total(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Total zonal-wind increment binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],$
        YVALS=indgen(18)*(-50)+1000,XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.'+$
          model_runids(i)+'_conv.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(conv_levels)+1
   LEVS,MANUAL=conv_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_uinc_conv(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Zonal-wind increment from convection binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],$
        YVALS=indgen(18)*(-50)+1000,XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.'+$
          model_runids(i)+'_advect.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(total_levels)+1
   LEVS,MANUAL=total_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_uinc_advect(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Zonal-wind increment from advection binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],$
        YVALS=indgen(18)*(-50)+1000,XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.'+$
          model_runids(i)+'_bdylr.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=800
   model_top=NEAREST(rholvl_pres_mean,800)
   CS,SCALE=24,NCOLS=N_ELEMENTS(bdylr_levels)+1,/REV
   LEVS,MANUAL=bdylr_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_uinc_bdylr(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Zonal-wind increment from boundary-layer binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],$
        YVALS=indgen(18)*(-50)+1000,XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.'+$
          model_runids(i)+'_conv_singlelvl_'+STRTRIM(STRING(single_level),1)+'hPa.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_uincconv_bins,YMIN=0,YMAX=n_bins+1
   pdf_levs=['0.02','0.04','0.06','0.08','0.10','0.12','0.16','0.20','0.25','0.30','0.70']
   CS,SCALE=30,NCOLS=N_ELEMENTS(pdf_levs)+1,white=[2]
   LEVS,MANUAL=pdf_levs
   FOR j=0,n_bins DO $
      singlelvl_joint_pdf(i,*,j)=singlelvl_joint_pdf(i,*,j)/TOTAL(singlelvl_joint_pdf(i,*,j))
   ;print,singlelvl_joint_pdf(0,21,*)
   CON,X=indgen(n_uincconv_bins)+0.5,Y=indgen(n_bins+1)+0.5,FIELD=REFORM(singlelvl_joint_pdf(i,*,*)),/BLOCK,/NOLINES,$
       TITLE='Joint PDF of convective increment and precipitation rate at '+STRTRIM(STRING(single_level),1)+' hPa - 5S-5N, 70E-90E - first 5 days only'
   AXES,XVALS=indgen(n_uincconv_bins+1),XLABELS=[STRMID(STRTRIM(STRING(uinc_conv_bins),1),0,6),'>'+$
                                                 STRMID(STRTRIM(STRING(uinc_conv_bins(n_uincconv_bins-1)),1),0,6)],$
        YVALS=indgen(n_bins+2),YLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+$
                                        STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],$
        XTITLE='Zonal-wind increment from convection',YTITLE='Precipitation rate',NDECS=3,$
        ORIENTATION=40
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_uinc_twodhist.conv_singlelvl_'+$
       STRTRIM(STRING(single_level),1)+'hPa_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80
GSET,XMIN=-0.2,XMAX=0.2,YMIN=0,YMAX=MAX(singlelvl_pdf)*1.3
colors=['blue','red','blue','red']
styles=[0,0,1,1]
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(41)*(0.4/40.)-0.2,Y=REFORM(singlelvl_pdf(i,*)),COL=FSC_COLOR(colors(i)),STYLE=styles(i)
AXES,XSTEP=0.04,YSTEP=MAX(singlelvl_pdf)/20.,NDECS=4,XTITLE='Zonal wind increment from convection (m s!U-1!N hr!U-1!N)',YTITLE='Probability density'
LEGEND,labels=REVERSE(model_runids),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1,STYLE=REVERSE(styles)
PSCLOSE,/NOVIEW

STOP

END

