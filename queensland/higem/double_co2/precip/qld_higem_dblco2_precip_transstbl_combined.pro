PRO qld_higem_dblco2_precip_transstbl_combined

; Directories
higem_control_indir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_transstbl_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'

higem_control_nyears=149
higem_transstbl_nyears=51
transstbl_offset=50

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'

box=[-50,90,-1,180]
qldbox=[-32,137,-11,154]

mylevs_lintrend_annual=['-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13']
mylevs_lintrend_season=['-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5']

mylevs_diff_season=['-156','-132','-108','-84','-60','-36','-12','12','36','60','84','108','132','156']
mylevs_diff_annual=['-325','-275','-225','-175','-125','-75','-25','25','75','125','175','225','275','325']

mylevs_ratio_season=['0.35','0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.18','1.33','1.54','1.82','2.22','2.86']
mylevs_ratio_annual=['0.61','0.67','0.73','0.79','0.85','0.91','0.97','1.03','1.10','1.18','1.27','1.37','1.49','1.64']

n_seasons=5
FOR i=4,n_seasons-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='dec-feb'
         season_type='smean'
         season_ndays=90.
         ts_min=100
         ts_max=600
         ts_step=25
      END
      1 : BEGIN
         season_name='mar-may'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=400
         ts_step=25
      END
      2 : BEGIN
         season_name='jun-aug'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=150
         ts_step=15
      END
      3 : BEGIN
         season_name='sep-nov'
         season_type='smean'
         season_ndays=90.
         ts_min=0
         ts_max=220
         ts_step=20         
      END
      4 : BEGIN
         season_name='may-apr'
         season_type='amean'
         season_ndays=360.
         ts_min=200
         ts_max=1000
         ts_step=100
      END
   ENDCASE
   
   higem_control_infile=higem_control_indir+'/higem_eafeb.'+season_name+'_'+season_type+'s.h9-w8.precip.aus_domain.nc'
   higem_transstbl_infile=higem_transstbl_indir+'/higem_eafee_eadwu.'+season_name+'_'+season_type+'s.m9-s0.precip.global_domain.nc'
   
   control_longitude=OPEN_AND_EXTRACT(higem_control_infile,'longitude')
   control_latitude=OPEN_AND_EXTRACT(higem_control_infile,'latitude')
   DEFINE_BOUNDARIES,box,control_latitude,control_longitude,control_box_tx,/LIMIT
   control_nlon=N_ELEMENTS(control_longitude)
   control_nlat=N_ELEMENTS(control_latitude)
   
   transstbl_longitude=OPEN_AND_EXTRACT(higem_transstbl_infile,'longitude')
   transstbl_latitude=OPEN_AND_EXTRACT(higem_transstbl_infile,'latitude')
   DEFINE_BOUNDARIES,box,transstbl_latitude,transstbl_longitude,transstbl_box_tx,/LIMIT
   transstbl_nlon=N_ELEMENTS(transstbl_longitude)
   transstbl_nlat=N_ELEMENTS(transstbl_latitude)

   control_precip=OPEN_AND_EXTRACT(higem_control_infile,'precip',$
                                   offset=[control_box_tx(1),control_box_tx(0),0],$
                                   count=[control_nlon,control_nlat,higem_control_nyears])*86400.
   
   transstbl_precip=OPEN_AND_EXTRACT(higem_transstbl_infile,'precip',$
                                 offset=[transstbl_box_tx(1),transstbl_box_tx(0),0],$
                                 count=[transstbl_nlon,transstbl_nlat,higem_transstbl_nyears])*86400.
   
   transstbl_precip_clim=fltarr(transstbl_nlon,transstbl_nlat)
   FOR j=0,transstbl_nlon-1 DO $
      FOR k=0,transstbl_nlat-1 DO $
         transstbl_precip_clim(j,k)=MEAN(transstbl_precip(j,k,*))
   

   ; Number of 50 year chunks in the HiGEM control integration
   n_chunks=3
   n_years_per_chunk=49
   diff_transstbl_control_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   ratio_transstbl_control_chunks=fltarr(transstbl_nlon,transstbl_nlat)
   FOR j=0,n_chunks-1 DO BEGIN
      FOR k=0,transstbl_nlon-1 DO BEGIN
         FOR m=0,transstbl_nlat-1 DO BEGIN
            diff_transstbl_control_chunks(k,m)=transstbl_precip_clim(k,m)-MEAN(control_precip(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
            ratio_transstbl_control_chunks(k,m)=transstbl_precip_clim(k,m)/MEAN(control_precip(k,m,j*n_years_per_chunk:(j+1)*n_years_per_chunk-1))
         ENDFOR
      ENDFOR
         
      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_combined.diff_transstbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_diff_annual
         cb_label='mm year!U-1!N'
      ENDIF ELSE BEGIN
         LEVS,MANUAL=mylevs_diff_season
         cb_label='mm season!U-1!N'
      ENDELSE
      CON,FIELD=diff_transstbl_control_chunks*season_ndays,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Diff in clim rainfall between 2% year!U-1!N CO2 transient (30 years) and control (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_combined.ratio_transstbl-minus-control_chunk'+$
             STRTRIM(STRING(j+1),1)+'.'+season_name+'_'+season_type+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
      IF season_type eq 'amean' THEN BEGIN
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
      ENDIF ELSE $
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
      MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
      IF season_type eq 'amean' THEN BEGIN
         LEVS,MANUAL=mylevs_ratio_annual
      ENDIF ELSE $
         LEVS,MANUAL=mylevs_ratio_season
      CON,FIELD=ratio_transstbl_control_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
          TITLE='Ratio in clim rainfall between 2% year!U-1!N CO2 transient (30 years) and control (30 years, chunk '+$
          STRTRIM(STRING(j+1),1)+') - '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
      AXES
      PSCLOSE,/NOVIEW
   ENDFOR

   FOR k=0,transstbl_nlon-1 DO BEGIN
      FOR m=0,transstbl_nlat-1 DO BEGIN
         diff_transstbl_control_chunks(k,m)=transstbl_precip_clim(k,m)-MEAN(control_precip(k,m,*))
         ratio_transstbl_control_chunks(k,m)=transstbl_precip_clim(k,m)/MEAN(control_precip(k,m,*))
      ENDFOR
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_combined.diff_transstbl-minus-control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_diff_annual
      cb_label='mm year!U-1!N'
   ENDIF ELSE BEGIN
      LEVS,MANUAL=mylevs_diff_season
      cb_label='mm season!U-1!N'
   ENDELSE
   CON,FIELD=diff_transstbl_control_chunks*season_ndays,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Diff in clim rainfall between 2%+2x CO2 (50 years) and control (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE=cb_label                    
   AXES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_combined.ratio_transstbl-minus-control.'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=1200,XOFFSET=500,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,CB_WIDTH=110,XSIZE=20000
   IF season_type eq 'amean' THEN BEGIN
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_annual)+1,/REV,white=[9]
   ENDIF ELSE $
      CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio_season)+1,/REV,white=[9] 
   MAP,LONMIN=MIN(transstbl_longitude),LONMAX=MAX(transstbl_longitude),LATMIN=MIN(transstbl_latitude),LATMAX=MAX(transstbl_latitude),/hires ;,/SET
   IF season_type eq 'amean' THEN BEGIN
      LEVS,MANUAL=mylevs_ratio_annual
   ENDIF ELSE $
      LEVS,MANUAL=mylevs_ratio_season
   CON,FIELD=ratio_transstbl_control_chunks,X=transstbl_longitude,Y=transstbl_latitude,$
       TITLE='Ratio in clim rainfall between 2% year!U-1!N CO2 transient (30 years) and control (150 years) '+$
       '- '+season_name,/NOLINES,/BLOCK,CB_TITLE='Ratio of rainfall (unitless)'
   AXES
   PSCLOSE,/NOVIEW
  
   mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
   mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
   DEFINE_BOUNDARIES,qldbox,mask_latitude,mask_longitude,mask_qldbox_tx,/LIMIT
   mask_nlon=N_ELEMENTS(mask_longitude)
   mask_nlat=N_ELEMENTS(mask_latitude)

   DEFINE_BOUNDARIES,qldbox,control_latitude,control_longitude,control_qldbox_tx
   DEFINE_BOUNDARIES,qldbox,transstbl_latitude,transstbl_longitude,transstbl_qldbox_tx
   
   mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                offset=[mask_qldbox_tx(1),mask_qldbox_tx(0),0,0],$
                                count=[mask_nlon,mask_nlat,1,1]))
   
   control_qld_aavg=fltarr(higem_control_nyears)
   transstbl_qld_aavg=fltarr(higem_transstbl_nyears)
   FOR j=0,higem_control_nyears-1 DO BEGIN
      thisyear_precip=REFORM(control_precip(control_qldbox_tx(1):control_qldbox_tx(3),control_qldbox_tx(0):control_qldbox_tx(2),j))
      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
      control_qld_aavg(j)=MEAN(thisyear_precip,/NaN)      
   ENDFOR
   FOR j=0,higem_transstbl_nyears-1 DO BEGIN
      thisyear_precip=REFORM(transstbl_precip(transstbl_qldbox_tx(1):transstbl_qldbox_tx(3),transstbl_qldbox_tx(0):transstbl_qldbox_tx(2),j))
      thisyear_precip[where(mask eq 0)]=!Values.F_NaN
      transstbl_qld_aavg(j)=MEAN(thisyear_precip,/NaN)     
   ENDFOR

   psfile='/home/ss901165/idl/queensland/higem/double_co2/precip/qld_higem_dblco2_precip_transstbl_combined.qld_aavg_ts_'+$
          season_name+'_'+season_type+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1500,TFONT=2,$
          TCHARSIZE=80,SPACE3=200
   GSET,XMIN=0,XMAX=higem_control_nyears,YMIN=ts_min,YMAX=ts_max,$
        TITLE='Queensland area-average precipitation in HiGEM control, transient and stabilized simulations - '+season_name
   GPLOT,X=indgen(higem_control_nyears),Y=control_qld_aavg*season_ndays,COL=FSC_COLOR('black'),STYLE=0,THICK=40
   GPLOT,X=indgen(higem_transstbl_nyears)+transstbl_offset,Y=transstbl_qld_aavg*season_ndays,COL=FSC_COLOR('red'),STYLE=0,THICK=40
   
   temp=SMOOTH(control_qld_aavg*season_ndays,5)
   GPLOT,X=indgen(higem_control_nyears-4)+3,Y=temp(2:higem_control_nyears-3),COL=FSC_COLOR('black'),STYLE=0,THICK=150
   temp=SMOOTH(transstbl_qld_aavg*season_ndays,5)
   GPLOT,X=indgen(higem_transstbl_nyears-4)+3+transstbl_offset,Y=temp(2:higem_transstbl_nyears-3),COL=FSC_COLOR('red'),STYLE=0,THICK=150  

   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg),MEAN(control_qld_aavg)]*season_ndays,STYLE=2
   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg)+STDDEV(control_qld_aavg),$
                                       MEAN(control_qld_aavg)+STDDEV(control_qld_aavg)]*season_ndays,STYLE=1
   GPLOT,X=[0,higem_control_nyears],Y=[MEAN(control_qld_aavg)-STDDEV(control_qld_aavg),$
                                       MEAN(control_qld_aavg)-STDDEV(control_qld_aavg)]*season_ndays,STYLE=1

   AXES,XSTEP=10,YSTEP=ts_step,YMINOR=ts_step/2.,XMINOR=2,XTITLE='Year of HiGEM integration',YTITLE='Total precipitation (mm) averaged over Queensland land points'
   
   GLEGEND,labels=['Five-year running mean','Yearly timeseries','HiGEM 2xCO2','HiGEM control'],$
           COL=[FSC_COLOR('black'),FSC_COLOR('black'),FSC_COLOR('red'),FSC_COLOR('black')],$
           THICK=[150,40,100,100,100],LEGPOS=1
                

   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

