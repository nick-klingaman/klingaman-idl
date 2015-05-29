PRO hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist

; Box to read (consider all gridpoints within)
box=[-5,70,5,90]

total_levels_hrly=['-0.090','-0.078','-0.066','-0.054','-0.042','-0.030','-0.018','-0.006',$
                   '0.006','0.018','0.030','0.042','0.054','0.066','0.078','0.090']
total_levels_ts=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']
conv_levels_hrly=['-0.175','-0.125','-0.075','-0.025','0.025','0.075','0.125','0.175','0.225','0.275','0.325','0.375']
conv_levels_ts=['-0.28','-0.20','-0.12','-0.04','0.04','0.12','0.20','0.28','0.36','0.44','0.52','0.60']
advect_levels_hrly=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']
bdylr_levels_hrly=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
                   '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
bdylr_levels_ts=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01',$
                 '0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
;hist_bins=[0,0.05,0.1,0.2,0.4,0.7,1.0,2.0,4.0,7.0,10.0]
;hist_bins=[0,0.02,0.10,0.30,0.6,1.2]
;hist_bins=indgen(31)*0.02
;n_bins=N_ELEMENTS(hist_bins)
n_bins=20

;tinc_conv_bins=[-0.04,-0.03,-0.025,-0.020,-0.017,-0.014,-0.011,-0.008,-0.005,-0.003,-0.001,$
;                0.001,0.003,0.005,0.008,0.011,0.014,0.017,0.02,0.025,0.03,0.04]
;tinc_conv_bins=[-0.02,0.02,0.06,0.10,0.14,0.18,0.22,0.26,0.30,0.34,0.38,0.42,0.46,0.50,0.54,0.58,0.62]
tinc_conv_bins=[-0.075,-0.045,-0.015,0.015,0.045,0.075,0.105,0.135,0.165,0.195,0.225,0.255,0.285,0.315,0.345,0.375,0.405,0.435]
;tinc_conv_bins=[-0.015,-0.005,0.005,0.015,0.025,0.035,0.045,0.055,0.065,0.075,0.085,0.095,0.105,0.115,0.125,0.135]
n_tincconv_bins=N_ELEMENTS(tinc_conv_bins)
single_level=700

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
         model_incmult=3
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
         model_incmult=3
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
         model_incmult=3
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
         model_incmult=3
         end_date='10apr09'
         rholvl_latname='latitude'
         rholvl_zname='hybrid_ht'
         precip_latname='latitude_1'
      END
   ENDCASE
   print,runid
   model_offset=0

   ;u_file_suffix='a.06apr09-'+end_date+'.t_rholvl.nc'
   tinc_total_file_suffix='a.06apr09-'+end_date+'.tinc_total.nc'
   tinc_conv_file_suffix='a.06apr09-'+end_date+'.tinc_conv.nc'
   tinc_advect_file_suffix='a.06apr09-'+end_date+'.tinc_advect.nc'
   tinc_bdylr_file_suffix='a.06apr09-'+end_date+'.tinc_bdylr_lscld.nc'
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
                                       offset=[model_box_tx(1),model_box_tx(0),0,model_offset],$
                                       count=[model_nlon,model_nlat,model_nz,model_count]))/100.
   rholvl_pres_mean=fltarr(model_nz)
   FOR j=0,model_nz-1 DO $
      rholvl_pres_mean(j)=MEAN(rholvl_pres(*,*,j,*))   

   ;u_file=um3_dir+'/'+dirname+'/'+runid+u_file_suffix
   tinc_total_file=um3_dir+'/'+dirname+'/'+runid+tinc_total_file_suffix
   tinc_conv_file=um3_dir+'/'+dirname+'/'+runid+tinc_conv_file_suffix
   tinc_advect_file=um3_dir+'/'+dirname+'/'+runid+tinc_advect_file_suffix
   tinc_bdylr_file=um3_dir+'/'+dirname+'/'+runid+tinc_bdylr_file_suffix
   precip_file=um3_dir+'/'+dirname+'/'+runid+precip_file_suffix
   model_tlat=OPEN_AND_EXTRACT(tinc_total_file,'latitude')
   model_tlon=OPEN_AND_EXTRACT(tinc_total_file,'longitude')
   model_z=OPEN_AND_EXTRACT(tinc_total_file,'hybrid_ht')
   DEFINE_BOUNDARIES,box,model_tlat,model_tlon,model_tbox_tx,/LIMIT
   model_ntlat=N_ELEMENTS(model_tlat)
   model_ntlon=N_ELEMENTS(model_tlon)
   model_nz=NEAREST(rholvl_pres_mean,150)+1
   model_z925=NEAREST(rholvl_pres_mean,925)
   model_zsing=NEAREST(rholvl_pres_mean,single_level)
   model_z500=NEAREST(rholvl_pres_mean,500)
   IF i eq 0 THEN BEGIN
      binned_model_tinc_total=fltarr(n_models,n_bins+1,model_nz)
      binned_model_tinc_conv=fltarr(n_models,n_bins+1,model_nz)
      singlelvl_joint_pdf=fltarr(n_models,n_tincconv_bins,n_bins+1)
      singlelvl_pdf=fltarr(n_models,41)
;      binned_model_tinc_conv_eshear=fltarr(n_models,n_bins,model_nz)
;      binned_model_tinc_conv_wshear=fltarr(n_models,n_bins,model_nz)
      binned_model_tinc_advect=fltarr(n_models,n_bins+1,model_nz)
      binned_model_tinc_bdylr=fltarr(n_models,n_bins+1,model_nz)
      bins_npoints=fltarr(n_models,n_bins+1)
      model_runids=strarr(n_models)
      model_legend_labels=strarr(n_models)      
   ENDIF

   model_preciplat=OPEN_AND_EXTRACT(precip_file,precip_latname)
   model_preciplon=OPEN_AND_EXTRACT(precip_file,'longitude')
   DEFINE_BOUNDARIES,box,model_preciplat,model_preciplon,model_precipbox_tx,/LIMIT
   model_npreciplat=N_ELEMENTS(model_preciplat)
   model_npreciplon=N_ELEMENTS(model_preciplon)
      
   print,'Reading total temperature increments ...'
   model_tinc_total=REFORM(OPEN_AND_EXTRACT(tinc_total_file,'tinc_total',$
                                            offset=[model_tbox_tx(1),model_tbox_tx(0),0,model_offset],$
                                            count=[model_ntlon,model_ntlat,model_nz,model_count]))
   print,'Reading convective temperature increments ...'
   model_tinc_conv=REFORM(OPEN_AND_EXTRACT(tinc_conv_file,'tinc_conv',$
                                           offset=[model_tbox_tx(1),model_tbox_tx(0),0,model_offset],$
                                           count=[model_ntlon,model_ntlat,model_nz,model_count]))
   print,'Reading advective temperature increments ...'
   model_tinc_advect=REFORM(OPEN_AND_EXTRACT(tinc_advect_file,'tinc_advect',$
                                             offset=[model_tbox_tx(1),model_tbox_tx(0),0,model_offset],$
                                             count=[model_ntlon,model_ntlat,model_nz,model_count]))
   print,'Reading boundary-layer temperature increments ...'
   model_tinc_bdylr=REFORM(OPEN_AND_EXTRACT(tinc_bdylr_file,'tinc_bdylr_lscld',$
                                            offset=[model_tbox_tx(1),model_tbox_tx(0),0,model_offset],$
                                            count=[model_ntlon,model_ntlat,model_nz,model_count]))
   print,'Reading precipitation ...'
   model_precip=REFORM(OPEN_AND_EXTRACT(precip_file,'cvrain',$
                                        offset=[model_precipbox_tx(1),model_precipbox_tx(0),0,model_offset],$
                                        count=[model_npreciplon,model_npreciplat,1,model_count]))*(3.)
   ;print,'Reading temperature ...'
   ;model_u=REFORM(OPEN_AND_EXTRACT(u_file,'u_rholvl',$
   ;                                offset=[model_tbox_tx(1),model_box_tx(0),0,model_offset],$
   ;                                count=[model_nlon,model_nlat,model_nz,model_count]))

                                
;   hist_input=fltarr(2,model_nlon*model_nlat*model_count)
;   hist_input(0,*)=REFORM(model_tinc_conv(*,*,model_zsing,*),[model_nlon*model_nlat*model_count])
;   hist_input(1,*)=REFORM(model_precip,[model_nlon*model_nlat*model_count])

;   singlelvl_joint_pdf(i,*,*)=HIST_ND(hist_input,min=[-0.020,0],max=[0.020,0.25],nbins=[41,26])

   model_zsing_tinc_conv=REFORM(model_tinc_conv(*,*,model_zsing,*))
   model_nonzero_precip=model_precip[where(model_precip gt 0.001)]
   model_zsing_tinc_conv_nonzero_precip=model_zsing_tinc_conv[where(model_precip gt 0.001)]
   model_nonzero_precip_sorted=SORT(model_nonzero_precip)
   FOR j=0,n_bins-1 DO $
      hist_bins(i,j+1)=model_nonzero_precip(model_nonzero_precip_sorted(FLOOR(N_ELEMENTS(model_nonzero_precip)*j/FLOAT(n_bins))))
   print,hist_bins
 
   model_precip_indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)
   model_zsing_tinc_conv_indices=VALUE_LOCATE(tinc_conv_bins,model_zsing_tinc_conv)
   FOR j=0,n_bins DO BEGIN
      FOR k=0,n_tincconv_bins-1 DO BEGIN
         singlelvl_joint_pdf(i,k,j)=N_ELEMENTS(where(model_zsing_tinc_conv_indices eq k and $
                                               model_precip_indices eq j))
      ENDFOR         
   ENDFOR
;   print,singlelvl_joint_pdf(0,21,*)

   PDF,REFORM(model_zsing_tinc_conv,[model_ntlon*model_ntlat*model_count]),xrange=[-0.05,0.35],npdf=41,pdf=temp,/noplot
   singlelvl_pdf(i,*)=temp
  
   indices=VALUE_LOCATE(REFORM(hist_bins(i,*)),model_precip)  
   FOR j=0,model_nz-1 DO BEGIN
;      print,'Calculating for level '+STRTRIM(STRING(j+1),1)+' of '+STRTRIM(STRING(model_nz),1)
      thislev_tinc_total=REFORM(model_tinc_total(*,*,j,*))
      thislev_tinc_conv=REFORM(model_tinc_conv(*,*,j,*))
      thislev_tinc_bdylr=REFORM(model_tinc_bdylr(*,*,j,*))
      thislev_tinc_advect=REFORM(model_tinc_advect(*,*,j,*))
      FOR k=0,n_bins DO BEGIN
         IF TOTAL(where(indices eq k)) gt 0 THEN BEGIN
            binned_model_tinc_total(i,k,j)=MEAN(thislev_tinc_total[where(indices eq k)],/NaN)
            binned_model_tinc_conv(i,k,j)=MEAN(thislev_tinc_conv[where(indices eq k)],/NaN)
                                ;binned_model_tinc_conv_wshear(i,k,j)=MEAN(thislev_tinc_conv[where(indices eq k and shear_indices eq 0)],/NaN)
                                ;binned_model_tinc_conv_eshear(i,k,j)=MEAN(thislev_tinc_conv[where(indices eq k and shear_indices eq 1)],/NaN)
            binned_model_tinc_bdylr(i,k,j)=MEAN(thislev_tinc_bdylr[where(indices eq k)],/NaN)
            binned_model_tinc_advect(i,k,j)=MEAN(thislev_tinc_advect[where(indices eq k)],/NaN)
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
   CASE i OF 
      0 : BEGIN
         total_levels=total_levels_hrly
         conv_levels=conv_levels_hrly
         advect_levels=advect_levels_hrly
         bdylr_levels=bdylr_levels_hrly
      END
      1 : BEGIN
         total_levels=total_levels_hrly
         conv_levels=conv_levels_hrly
         advect_levels=advect_levels_hrly
         bdylr_levels=bdylr_levels_hrly
      END
      2 : BEGIN
         total_levels=total_levels_ts
         conv_levels=conv_levels_ts
         advect_levels=advect_levels_hrly
         bdylr_levels=bdylr_levels_ts
      END
      3 : BEGIN
         total_levels=total_levels_ts
         conv_levels=conv_levels_ts
         advect_levels=advect_levels_hrly
         bdylr_levels=bdylr_levels_ts
      END
   ENDCASE

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.'+model_runids(i)+'_total.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(total_levels)+1
   LEVS,MANUAL=total_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_tinc_total(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Total temperature increment binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'> '+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],YVALS=indgen(18)*(-50)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.'+model_runids(i)+'_conv.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(conv_levels)+1,MIN=60
   LEVS,MANUAL=conv_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_tinc_conv(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Temperature increment from convection binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
    AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'> '+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],YVALS=indgen(18)*(-50)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.'+model_runids(i)+'_advect.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=140
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(advect_levels)+1,MIN=2,MAX=150
   LEVS,MANUAL=advect_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_tinc_advect(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Temperature increment from advection binned by precip - 5S-5N, 70E-90E - first 5 days of run only',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'> '+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],YVALS=indgen(18)*(-50)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.'+model_runids(i)+'_bdylr.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=1005,YMAX=800
   model_top=model_nz
   CS,SCALE=1,NCOLS=N_ELEMENTS(bdylr_levels)+1
   LEVS,MANUAL=bdylr_levels
   CON,X=indgen(n_bins+1)+0.5,Y=rholvl_pres_mean(0:model_top-1),FIELD=REFORM(binned_model_tinc_bdylr(i,*,0:model_top-1)),/BLOCK,$
       TITLE='Temp incr from bndry-lyr and lge-scl cloud binned by precip - 5S-5N, 70E-90E - first 5 days of run',/NOLINES
   AXES,XVALS=indgen(n_bins+2),XLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,5),'> '+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,5)],YVALS=indgen(21)*(-10)+1000,$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Pressure (hPa)',NDECS=3,ORIENTATION=40
   PSCLOSE
   
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.'+model_runids(i)+'_conv_singlelvl_'+$
          STRTRIM(STRING(single_level),1)+'hPa.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80,/PORTRAIT
   GSET,XMIN=0,XMAX=n_tincconv_bins,YMIN=0,YMAX=n_bins+1
   pdf_levs=['0.02','0.04','0.06','0.08','0.10','0.12','0.16','0.20','0.25','0.30','0.70']
   CS,SCALE=30,NCOLS=N_ELEMENTS(pdf_levs)+1,white=[2]
   LEVS,MANUAL=pdf_levs
   FOR j=0,n_bins DO $
      singlelvl_joint_pdf(i,*,j)=singlelvl_joint_pdf(i,*,j)/TOTAL(singlelvl_joint_pdf(i,*,j))
   ;print,singlelvl_joint_pdf(0,21,*)
   CON,X=indgen(n_tincconv_bins)+0.5,Y=indgen(n_bins+1)+0.5,FIELD=REFORM(singlelvl_joint_pdf(i,*,*)),/BLOCK,/NOLINES,$
       TITLE='Joint PDF of convective increment and precipitation rate at '+STRTRIM(STRING(single_level),1)+' hPa - 5S-5N, 70E-90E - first 5 days only'
   AXES,XVALS=indgen(n_tincconv_bins+1),XLABELS=[STRMID(STRTRIM(STRING(tinc_conv_bins),1),0,6),'>'+STRMID(STRTRIM(STRING(tinc_conv_bins(n_tincconv_bins-1)),1),0,6)],$
        YVALS=indgen(n_bins+2),YLABELS=[STRMID(STRTRIM(STRING(REFORM(hist_bins(i,*))),1),0,6),'>'+STRMID(STRTRIM(STRING(hist_bins(i,n_bins)),1),0,6)],$
        XTITLE='Temperature increment from convection',YTITLE='Precipitation rate',NDECS=3,$
        ORIENTATION=40
   PSCLOSE,/NOVIEW
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_bin/hadgem3kpp_cascade_apr09_precip_bin_tinc_twodhist.conv_singlelvl_'+$
       STRTRIM(STRING(single_level),1)+'hPa_pdf.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=90,MARGIN=1400,SPACE2=700,XOFFSET=1000,YOFFSET=1500,TFONT=2,TCHARSIZE=80
GSET,XMIN=-0.05,XMAX=0.35,YMIN=0.00001,YMAX=0.01,/YLOG
colors=['blue','red','blue','red']
styles=[0,0,1,1]
singlelvl_pdf[where(singlelvl_pdf lt 0.00001)]=!Values.F_NaN
FOR i=0,n_models-1 DO $
   GPLOT,X=indgen(41)*(0.4/40.)-0.05,Y=REFORM(singlelvl_pdf(i,*)),COL=FSC_COLOR(colors(i)),STYLE=styles(i)
AXES,XSTEP=0.02,NDECS=4,$
     YVALS=['0.00001','0.000015','0.00002','0.00003','0.00004','0.00007','0.0001','0.00015','0.0002','0.0003','0.0004','0.0007','0.001','0.0015','0.002','0.003','0.004','0.007','0.01'],$
     XTITLE='Temperature increment from convection (K hr!U-1!N)',YTITLE='Probability density'
LEGEND,labels=REVERSE(model_runids),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=1,STYLE=REVERSE(styles)
PSCLOSE,/NOVIEW

STOP

END

