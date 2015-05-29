PRO mjo_diabatic_mogreps_sst_precip_errors

  dir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Coupled_model'
  atmdir='/home/ss901165/um_output6/mjodiab_20day/metum_coupled_ann/Atmos_control'
  n_sets=4

  box=[-47,0,47,360]
  
  precip_levs=['-5','-4','-3','-2','-1','0','1','2','3','4','5']
;sst_levs=['-1.5','-1.2','-0.9','-0.6','-0.3','0.3','0.6','0.9','1.2','1.5']           
  sst_levs=['-1.2','-0.9','-0.6','-0.3','0.3','0.6','0.9','1.2']

  FOR i=0,n_sets-1 DO BEGIN
     CASE i OF
        2 : BEGIN
           sst_diff_file=dir+'/diff_day5_sst.nc'
           sst_name='ts'
           precip_diff_file=dir+'/diff_day5_precip.nc'
           mask_file=dir+'/20091015/MetUM.landsea.20091015.00Z.nc'
           psfile_desc='mogreps_day5'
           title_desc='MOGREPS day+5 (Oct 2009 - Jan 2010)'
           a=1.2
           b=0.1
        END
        3 : BEGIN
           sst_diff_file=dir+'/diff_day15_sst.nc'
           precip_diff_file=dir+'/diff_day15_precip.nc'
           mask_file=dir+'/20091015/MetUM.landsea.20091015.00Z.nc'
           psfile_desc='mogreps_day15'
           title_desc='MOGREPS day+15 (Oct 2009 - Jan 2010)'               
        END
        4 : BEGIN
           sst_diff_file=0
           precip_diff_file=atmdir+'/diff_day5_precip.nc'
           psfile_desc='mogreps_atm_day5'
           title_desc='MOGREPS ATM day+5 (Oct 2009 - Jan 2010)'
        END
        5 : BEGIN
           sst_diff_file=0
           precip_diff_file=atmdir+'/diff_day15_precip.nc'
           psfile_desc='mogreps_atm_day15'
           title_desc='MOGREPS ATM day+15 (Oct 2009 - Jan 2010)'
        END
        0 : BEGIN
           sst_diff_file=dir+'/diff_day5_sst.cpl-atm.nc'
           sst_name='sst'
           precip_diff_file=dir+'/diff_day5_precip.cpl-atm.nc'
           mask_file=dir+'/20091015/MetUM.landsea.20091015.00Z.nc'
           psfile_desc='mogreps_cpl-atm_day5'
           title_desc='MOGREPS CPL-ATM day+5 (Oct 2009 - Jan 2010)'
           a=1.8
           b=-0.1
        END
        1 : BEGIN
           sst_diff_file=dir+'/diff_day15_sst.cpl-atm.nc'
           precip_diff_file=dir+'/diff_day15_precip.cpl-atm.nc'
           mask_file=dir+'/20091015/MetUM.landsea.20091015.00Z.nc'
           psfile_desc='mogreps_cpl-atm_day15'
           title_desc='MOGREPS CPL-ATM day+15 (Oct 2009 - Jan 2010)'
           a=1.4
           b=-0.1
        END
     ENDCASE
     
     lon=OPEN_AND_EXTRACT(precip_diff_file,'longitude')
     lat=OPEN_AND_EXTRACT(precip_diff_file,'latitude')
     DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
     nlon=N_ELEMENTS(lon)
     nlat=N_ELEMENTS(lat)

     ;IF sst_diff_file ne 0 THEN BEGIN
        sst_diff=REFORM(OPEN_AND_EXTRACT(sst_diff_file,sst_name,$
                                         offset=[box_tx(1),box_tx(0),0],$
                                         count=[nlon,nlat,1]))    
        mask=REFORM(OPEN_AND_EXTRACT(mask_file,'landsea',$
                                     offset=[box_tx(1),box_tx(0),0],$
                                     count=[nlon,nlat,1]))
        IF TOTAL(where(ABS(sst_diff) ge 100)) ge 0 THEN $
           sst_diff[where(ABS(sst_diff) ge 100)]=!Values.F_NaN
     ;ENDIF
        
     precip_diff=REFORM(OPEN_AND_EXTRACT(precip_diff_file,'pr',$
                                         offset=[box_tx(1),box_tx(0),0],$
                                         count=[nlon,nlat,1]))

     sst_diff[where(mask ne 0)]=!Values.F_NaN

     FOR j=0,nlon-1 DO BEGIN
        ;IF sst_diff_file ne 0 THEN $
           sst_diff(j,*)=SMOOTH(sst_diff(j,*),3,/NaN)
        precip_diff(j,*)=SMOOTH(precip_diff(j,*),3)
     ENDFOR
     FOR j=0,nlat-1 DO BEGIN
        ;IF sst_diff_file ne 0 THEN $
           sst_diff(*,j)=SMOOTH(sst_diff(*,j),3,/NaN)
        precip_diff(*,j)=SMOOTH(precip_diff(*,j),3)
     ENDFOR
     ;IF sst_diff_file ne 0 THEN $
        sst_diff[where(mask ne 0)]=!Values.F_NaN

     psfile='/home/ss901165/idl/mjo_diabatic/mean_state/mjo_diabatic_mogreps_sst_precip_errors.'+psfile_desc+'.ps'
     PSOPEN,file=psfile,FONT=6,CHARSIZE=170,TFONT=6,TCHARSIZE=120,YSIZE=11000,MARGIN=2500,YOFFSET=2000,SPACE2=1500
     MAP,LONMIN=box(1),LONMAx=box(3),LATMIN=-45,LATMAX=45
     CS,SCALE=8,NCOLS=N_ELEMENTS(precip_levs)+1,white=[7,8]
     LEVS,MANUAL=precip_levs
     CON,X=lon,Y=lat,FIELD=precip_diff,/NOLINES,CB_TITLE='Difference in daily precipitation (mm day!U-1!N)',$
         CB_WIDTH=110
     ;IF sst_diff_file ne 0 THEN BEGIN
        LEVS,MANUAL=sst_levs
        CS,SCALE=6,NCOLS=N_ELEMENTS(sst_levs)+3
        CON,X=lon,Y=lat,FIELD=a*sst_diff-(b*precip_diff),/NOFILL,POSITIVE_STYLE=2,NEGATIVE_STYLE=2,COL=[1,2,3,4,5,10,11,12,13,14],ZERO_STYLE=0,ZERO_THICK=0,POSITIVE_THICK=250,NEGATIVE_THICK=250
     ;ENDIF
     AXES,XSTEP=30,YSTEP=10
     PSCLOSE

  ENDFOR  

STOP
END

