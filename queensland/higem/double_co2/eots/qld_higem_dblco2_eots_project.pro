PRO qld_higem_dblco2_eots_project

n_seasons=4
precip_nyears=31
n_eots=4
higem_stbl_basedir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
higem_ctl_basedir='/home/ss901165/higem_qccce/es_control_eafeb'

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

FOR i=0,n_seasons-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         stbl_precip_infile=higem_stbl_basedir+'/higem_eadwu.dec-feb_smeans.o2-r3.precip.global_domain.nc'
         ctl_precip_clim_infile=higem_ctl_basedir+'/higem_eafeb.dec-feb_smean_clim.h9-w8.precip.global_domain.nc'
         eots_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.dec-feb_smeans.h9-w8.eots.nc'
         season_name='dec-feb'
         hist_max=400
         hist_min=-300
      END
      1 : BEGIN
         stbl_precip_infile=higem_stbl_basedir+'/higem_eadwu.mar-may_smeans.o2-r3.precip.global_domain.nc'
         ctl_precip_clim_infile=higem_ctl_basedir+'/higem_eafeb.mar-may_smean_clim.h9-w8.precip.global_domain.nc'
         eots_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.mar-may_smeans.h9-w8.eots.nc'
         season_name='mar-may'
         hist_max=200
         hist_min=-200
      END
      2 : BEGIN
         stbl_precip_infile=higem_stbl_basedir+'/higem_eadwu.jun-aug_smeans.o2-r3.precip.global_domain.nc'
         ctl_precip_clim_infile=higem_ctl_basedir+'/higem_eafeb.jun-aug_smean_clim.h9-w8.precip.global_domain.nc'
         eots_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jun-aug_smeans.h9-w8.eots.nc'
         season_name='jun-aug'
         hist_max=100
         hist_min=-100
      END
      3 : BEGIN
         stbl_precip_infile=higem_stbl_basedir+'/higem_eadwu.sep-nov_smeans.o2-r3.precip.global_domain.nc'
         ctl_precip_clim_infile=higem_ctl_basedir+'/higem_eafeb.sep-nov_smean_clim.h9-w8.precip.global_domain.nc'
         eots_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sep-nov_smeans.h9-w8.eots.nc'
         season_name='sep-nov'
         hist_min=-100
         hist_max=100
      END
   ENDCASE
   
   ; Read latitude and longitude from EOT file
   eot_longitude=OPEN_AND_EXTRACT(eots_infile,'longitude')
   eot_latitude=OPEN_AND_EXTRACT(eots_infile,'latitude')
   eot_nlat=N_ELEMENTS(eot_latitude)
   eot_nlon=N_ELEMENTS(eot_longitude)

   ; Read latitude and longitude from precip file
   precip_longitude=OPEN_AND_EXTRACT(stbl_precip_infile,'longitude')
   precip_latitude=OPEN_AND_EXTRACT(stbl_precip_infile,'latitude')
   DEFINE_BOUNDARIES,[eot_latitude(0),eot_longitude(0),eot_latitude(eot_nlat-1),eot_longitude(eot_nlon-1)],$
                     precip_latitude,precip_longitude,precip_box_tx,/LIMIT
   precip_nlon=N_ELEMENTS(precip_longitude)
   precip_nlat=N_ELEMENTS(precip_latitude)

   ; Read land-sea mask
   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,[eot_latitude(0),eot_longitude(0),eot_latitude(eot_nlat-1),eot_longitude(eot_nlon-1)],$
                     mask_latitude,mask_longitude,mask_box_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)

   stbl_precip=OPEN_AND_EXTRACT(stbl_precip_infile,'precip',$
                                offset=[precip_box_tx(1),precip_box_tx(0),0],$
                                count=[precip_nlon,precip_nlat,precip_nyears])*86400.

   ctl_precip_clim=OPEN_AND_EXTRACT(ctl_precip_clim_infile,'precip',$
                                    offset=[precip_box_tx(1),precip_box_tx(0)],$
                                    count=[precip_nlon,precip_nlat])*86400.

   eots=OPEN_AND_EXTRACT(eots_infile,'spatial_pattern',$
                         offset=[0,0,0],$
                         count=[eot_nlon,eot_nlat,n_eots])

   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))

   projection=fltarr(n_eots,precip_nyears)
   overall_variance_explained=fltarr(n_eots)
   variance_explained=fltarr(n_eots,precip_nlon,precip_nlat,precip_nyears)
   variance_explained_grid=fltarr(n_eots,precip_nlon,precip_nlat)
   FOR j=0,n_eots-1 DO BEGIN
      this_eot_pattern=REFORM(eots(*,*,j))
      FOR k=0,precip_nyears-1 DO BEGIN
         this_year_precip=REFORM(stbl_precip(*,*,k))-ctl_precip_clim
         this_year_precip[where(mask eq 0)]=!Values.F_NaN
         projection(j,k)=TOTAL(this_eot_pattern*this_year_precip,/NaN)
      ENDFOR

      this_projection_stddev=SQRT(1./FLOAT(precip_nyears)*TOTAL(projection(j,*)^2))
      FOR k=0,precip_nlon-1 DO BEGIN
         FOR m=0,precip_nlat-1 DO BEGIN
            IF mask(k,m) ne 0 THEN BEGIN 
               thispt_stbl_precip_ts=REFORM(stbl_precip(k,m,*))-ctl_precip_clim(k,m)
               thispt_stddev=SQRT(1./FLOAT(precip_nyears)*TOTAL(thispt_stbl_precip_ts^2))
               thispt_corr=(1./FLOAT(precip_nyears)*TOTAL(REFORM(projection(j,*))*thispt_stbl_precip_ts))/$
                           (this_projection_stddev*thispt_stddev)
               thispt_eot=thispt_corr*thispt_stddev/this_projection_stddev
               FOR n=0,precip_nyears-1 DO $
                  variance_explained(j,k,m,n)=projection(j,n)*thispt_eot            
               ;variance_explained_grid(j,k,m)=TOTAL(variance_explained(j,k,m,*)^2,/NaN)
               variance_explained_grid(j,k,m)=CORRELATE(thispt_stbl_precip_ts,REFORM(projection(j,*)))^2*100.
            ENDIF ELSE BEGIN
               variance_explained(j,k,m,*)=!Values.F_NaN
               variance_explained_grid(j,k,m)=!Values.F_NaN
            ENDELSE
         ENDFOR       
      ENDFOR

      FOR n=0,precip_nyears-1 DO $
         overall_variance_explained(j)=MEAN(variance_explained(j,*,*,n)^2,/NaN)+overall_variance_explained(j)

      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_dblco2_eots_project.'+season_name+'.ctl_eot'+$
             STRTRIM(STRING(j+1),1)+'_ts.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=200,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=200
      GSET,XMIN=0,XMAX=precip_nyears,YMIN=hist_min,YMAX=hist_max,$
           TITLE='Projection of HiGEM 2xCO2 rainfall onto spatial pattern of '+season_name+' EOT '+STRTRIM(STRING(j+1),1)+' from HiGEM CTL'
      HIST,X=indgen(precip_nyears)+0.5,Y=REFORM(projection(j,*)),FILLCOL=FSC_COLOR('black')
      AXES,XSTEP=4,XMINOR=2,YSTEP=50,YTITLE='Timeseries of EOT projection',XTITLE='Year at beginning of '+season_name+' period'
      smoothed=SMOOTH(REFORM(projection(j,*)),7)
      GPLOT,X=indgen(precip_nyears-6)+3.5,Y=smoothed(3:precip_nyears-4),STYLE=0,THICK=150,COL=FSC_COLOR('red')
      PSCLOSE,/NOVIEW

      var_levs=['5','15','25','35','45','55','65','75','85','95']
      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_dblco2_eots_project.'+season_name+'.ctl_eot'+$
             STRTRIM(STRING(j+1),1)+'_varexpl_stbl.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=200,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=200,/PORTRAIT
      MAP,LONMIN=eot_longitude(0),LONMAX=eot_longitude(eot_nlon-1),LATMIN=eot_latitude(0),LATMAX=eot_latitude(eot_nlat-1)
      CS,SCALE=26,NCOLS=N_ELEMENTS(var_levs)+1,/REV
      LEVS,MANUAL=var_levs
      CON,X=eot_longitude,Y=eot_latitude,FIELD=REFORM(variance_explained_grid(j,*,*)),/NOLINES,/BLOCK,CB_TITLE='Percentage of variance explained'
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/eots/qld_higem_dblco2_eots_project.'+season_name+'.ctl_eot'+$
             STRTRIM(STRING(j+1),1)+'_varexpl_ctl.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=200,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=112,SPACE3=200,/PORTRAIT
      MAP,LONMIN=eot_longitude(0),LONMAX=eot_longitude(eot_nlon-1),LATMIN=eot_latitude(0),LATMAX=eot_latitude(eot_nlat-1)
      CS,SCALE=26,NCOLS=N_ELEMENTS(var_levs)+1,/REV
      LEVS,MANUAL=var_levs
      CON,X=eot_longitude,Y=eot_latitude,FIELD=this_eot_pattern^2*100.,/NOLINES,/BLOCK,CB_TITLE='Percentage of variance explained'
      AXES
      PSCLOSE,/NOVIEW
      
   ENDFOR
ENDFOR

STOP
END

