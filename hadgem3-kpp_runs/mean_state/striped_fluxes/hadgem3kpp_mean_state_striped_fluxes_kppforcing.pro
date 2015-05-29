PRO hadgem3kpp_mean_state_striped_fluxes_kppforcing

n_models=3
time='si1son'
box=[-30,0,30,360]

mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.3.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

FOR i=0,n_models-1 DO BEGIN
   CASE i OF 
      0 : BEGIN
         infile='/home/ss901165/um_output5/xfctu_withfixes/xfctua.p'+time+'.nc'
         runid='xfctu'
         description='GA3.0 science, AMIP2 SSTs/ice, vn7.8 (xfctu)'
         variables=['evap','sh_1','solar_2','longwave_2']
         n_variables=N_ELEMENTS(variables)
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xfctu_monotonefix/xfctua.p'+time+'.nc'
         runid='xfctu_monotone'
         description='GA3.0 science with monotone limiter fix, AMIP2 SSTs/ice, vn7.8 (xfctu)'
         variables=['evap','sh_1','solar_2','longwave_2']
         n_variables=N_ELEMENTS(variables)
      END
      2 : BEGIN
         infile='/home/ss901165/um_output5/xfzbj/i1/xfzbja.p'+time+'.nc'
         runid='xfzbj'
         description='Old NPK science, clim SSTs/ice, vn7.4 (xfzbj)'
         variables=['evap','sh_1','solar_1','longwave_1']
         n_variables=N_ELEMENTS(variables)
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   net_flux=fltarr(n_lon,n_lat)
   FOR j=0,n_variables-1 DO BEGIN
      CASE j OF 
         0 : BEGIN
            mylevs=['60','70','80','90','100','110','120','130','140','150','160','170','180','190','200','210','220','230','240','250']
            multiplier=2.5*10.^6
         END
         1 : BEGIN
            mylevs=['0','3','6','9','12','15','18','21','24','27','30','33','36','39','42']
            multiplier=1
         END
         2 : BEGIN
            mylevs=['150','160','170','180','190','200','210','220','230','240','250','260','270','280','290','300']
         END
         3 : BEGIN
            mylevs=['-100','-97','-94','-91','-88','-85','-82','-79','-76','-73','-70','-67','-64','-61','-58','-55','-52','-49','-46','-43','-40']
         END
      ENDCASE

      this_variable=REFORM(OPEN_AND_EXTRACT(infile,variables(j),offset=[box_tx(1),box_tx(0),0,0],$
                                       count=[n_lon,n_lat,1,1]))*multiplier

      this_variable[where(mask eq 1)]=!Values.F_NaN
      
;      STOP

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_kppforcing.'$
             +runid+'.'+variables(j)+'_'+time+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=90,SPACE3=800,$
             YSIZE=10000
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
      LEVS,MANUAL=mylevs
      MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
      CON,X=longitude,Y=latitude,FIELD=this_variable,/NOLINES,TITLE='Mean '+time+' '+variables(j)+' from run '+description,$
          CB_TITLE='W m!U-2!N'
      AXES
      PSCLOSE,/NOVIEW
 
      IF j eq 2 or j eq 3 THEN BEGIN
         net_flux=net_flux+this_variable
      ENDIF ELSE $
         net_flux=net_flux-this_variable      
   ENDFOR

   mylevs=['-105','-95','-85','-75','-65','-55','-45','-35','-25','-15','-5','5','15','25','35','45','55','65','75','85','95','105']

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_kppforcing.'$
          +runid+'.kpp_surface_forcing_'+time+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=90,SPACE3=500,$
          YSIZE=10000
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   MAP,/hires,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
   CON,X=longitude,Y=latitude,FIELD=net_flux,/NOLINES,TITLE='Mean '+time+' surface forcing for KPP from run '+description,$
       CB_TITLE='W m!U-2!N'
   AXES
   PSCLOSE   
ENDFOR

STOP
END
