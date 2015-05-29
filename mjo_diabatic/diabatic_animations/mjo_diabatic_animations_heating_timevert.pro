PRO mjo_diabatic_animations_heating_timevert,model_names,start_date,stop_date,box,field,lead_times=lead_times,plot_yotc=plot_yotc,plot_precip=plot_precip

;  levels=['-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6']
  levels=['-17','-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15','17']

  IF N_ELEMENTS(box) ne 4 THEN BEGIN
     print,'Specify box for area-averaging as a four-element vector: [start_lat,start_lon,stop_lat,stop_lon]'
     STOP
  ENDIF

  IF KEYWORD_SET(lead_times) THEN BEGIN
     our_lead_times=lead_times
  ENDIF ELSE $
     our_lead_times=indgen(20)
  n_lead_times=N_ELEMENTS(our_lead_times)  

  CASE field OF
     'q1_qr' : BEGIN
        field_desc='q1_minus_qr'
        cb_title='Non-radiative diabatic heating (Q1 minus Qr; K day!U-1!N)'
     END
  ENDCASE

  n_times_per_day=8
  standard_valid_dates=STRTRIM(STRING([indgen(22)+20091010,indgen(25)+20091101,indgen(22)+20091210,indgen(25)+20100101]),1)
  dates=STRTRIM(STRING([indgen(30)+20090901,indgen(31)+20091001,indgen(30)+20091101,indgen(31)+20091201,indgen(31)+20100101,$
                        indgen(28)+20100201]),1)
  
                                ; Parse start and stop dates
  start_year=STRMID(start_date,0,4)
  stop_year=STRMID(stop_date,0,4)
  start_month=STRMID(start_date,4,2)
  stop_month=STRMID(stop_date,4,2)
  start_day=STRMID(start_date,6,2)
  stop_day=STRMID(stop_date,6,2)

                                ; Get Julian dates
  start_julian=GREGORIAN_TO_JULIAN(FLOAT(start_day),FLOAT(start_month),FLOAT(start_year))
  stop_julian=GREGORIAN_TO_JULIAN(FLOAT(stop_day),FLOAT(stop_month),FLOAT(stop_year))
  WHILE stop_year gt start_year DO BEGIN
     stop_julian=365+stop_julian
     stop_year=stop_year-1
  ENDWHILE
  n_days=(stop_julian-start_julian+1)
  print,n_days

  IF KEYWORD_SET(plot_yotc) THEN BEGIN
     yotc_infile='/home/ss901165/datasets/ECMWF_YOTC/ECMWF_YOTC.oct2009-feb2010_3hrmeans.tnt.nc'
     yotc_longitude=OPEN_AND_EXTRACT(yotc_infile,'longitude')
     yotc_latitude=OPEN_AND_EXTRACT(yotc_infile,'latitude')
     DEFINE_BOUNDARIES,box,yotc_latitude,yotc_longitude,yotc_box_tx,/LIMIT
     yotc_nlon=N_ELEMENTS(yotc_longitude)
     yotc_nlat=N_ELEMENTS(yotc_latitude)
     yotc_z=OPEN_AND_EXTRACT(yotc_infile,'p')
     yotc_nz=N_ELEMENTS(yotc_z)

     yotc_start_date=274
     yotc_offset=(start_julian-yotc_start_date)*FLOAT(n_times_per_day)
     yotc_count=(stop_julian-start_julian+1)*FLOAT(n_times_per_day)

     yotc_field=OPEN_AND_EXTRACT(yotc_infile,'tnt_physics',$
                                 offset=[yotc_box_tx(1),yotc_box_tx(0),0,yotc_offset],$
                                 count=[yotc_nlon,yotc_nlat,yotc_nz,yotc_count])*8.

     ; Reverse YoTC vertical dimension to be high pressure -> low pressure
     ; Do area-averaging
     yotc_aavg_field=fltarr(n_days*n_times_per_day,yotc_nz)
     FOR j=0,yotc_nz-1 DO $
        FOR k=0,(n_days*n_times_per_day)-1 DO $
           yotc_aavg_field(k,yotc_nz-1-j)=MEAN(yotc_field(*,*,j,k))
     FOR j=0,yotc_nz-1 DO $
        yotc_aavg_field(*,j)=SMOOTH(yotc_aavg_field(*,j),9)
     yotc_z=REVERSE(yotc_z)

     IF KEYWORD_SET(plot_precip) THEN BEGIN
        trmm_infile='/home/ss901165/datasets/ECMWF_YOTC/TRMM_3B42v6A.jun2009-feb2010_dmeans.precip.2.5x2.5.nc'
        trmm_longitude=OPEN_AND_EXTRACT(trmm_infile,'longitude')
        trmm_latitude=OPEN_AND_EXTRACT(trmm_infile,'latitude')
        DEFINE_BOUNDARIES,box,trmm_latitude,trmm_longitude,trmm_box_tx,/LIMIT
        trmm_nlon=N_ELEMENTS(trmm_longitude)
        trmm_nlat=N_ELEMENTS(trmm_latitude)

        trmm_start_date=152
        trmm_offset=(start_julian-trmm_start_date)
        trmm_count=(stop_julian-start_julian+1)

        trmm_precip = OPEN_AND_EXTRACT(trmm_infile,'precip',$
                                       offset=[trmm_box_tx(1),trmm_box_tx(0),trmm_offset],$
                                       count=[trmm_nlon,trmm_nlat,trmm_count])
        trmm_precip_aavg=fltarr(trmm_count)
        FOR j=0,trmm_count-1 DO $
           trmm_precip_aavg(j)=MEAN(trmm_precip(*,*,j))        
     ENDIF
     
     psfile='/home/ss901165/idl/mjo_diabatic/diabatic_animations/mjo_diabatic_animations_heating_timevert.ecmwf_yotc.'+field_desc+'.'+$
            'latavg'+STRTRIM(STRING(box(0)),1)+'to'+STRTRIM(STRING(box(2)),1)+$
            '_lonavg'+STRTRIM(STRING(box(1)),1)+'to'+STRTRIM(STRING(box(3)),1)+'.'+start_date+'-'+stop_date+'.ps'     
     PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2300,SPACE2=1000,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
     GSET,XMIN=where(dates eq start_date),XMAX=where(dates eq stop_date)+1,YMIN=MAX(yotc_z)+25,YMAX=MIN(yotc_z)
     CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,white=[11]
     LEVS,MANUAL=levels
     test=where(dates eq start_date)
     CON,X=findgen(n_days*n_times_per_day)/FLOAT(n_times_per_day)+test(0),Y=yotc_z,$
         FIELD=yotc_aavg_field,POSITIVE_STYLE=0,NEGATIVE_STYLE=1,/NOLINELABELS,/NOLINES,$
         CB_TITLE=cb_title,TITLE='Heating from ECMWF YoTC analyses (0-24 hr forecast)'+$
            ' area-averaged lat '+STRTRIM(STRING(box(0)),1)+' to '+STRTRIM(STRING(box(2)),1)+' , lon '+STRTRIM(STRING(box(1)),1)+$
         ' to '+STRTRIM(STRING(box(3)),1)
     IF KEYWORD_SET(plot_precip) THEN BEGIN
        AXES,YVALS=[1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50],$
             YTITLE='Pressure (hPa)',XTITLE='Date',XLABELS=dates[test(0)+indgen(n_days/5+1)*5],$
             XVALS=indgen(n_days/5+1)*5+test(0),ORIENTATION=20,/NORIGHT
        GSET,XMIN=where(dates eq start_date),XMAX=where(dates eq stop_date)+1,YMIN=0,YMAX=40
        GPLOT,X=findgen(n_days)+test(0)+0.5,Y=trmm_precip_aavg
        AXES,YSTEP=2,YMINOR=1,YTITLE='Area-averaged TRMM precipitation (mm day!U-1!N)',/ONLYRIGHT
     ENDIF ELSE $
        AXES,YVALS=[1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50],$
             YTITLE='Pressure (hPa)',XTITLE='Date',XLABELS=dates[test(0)+indgen(n_days/5+1)*5],$
             XVALS=indgen(n_days/5+1)*5+test(0),ORIENTATION=20 
     PSCLOSE,/NOVIEW
  ENDIF
  
  n_models=N_ELEMENTS(model_names)
  
  FOR i=0,n_models-1 DO BEGIN
     print,model_names(i)
     CASE model_names(i) OF 
        'ecmwf' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/ecmwf'
           model_desc='ECMWF_IFS'
           plot_desc='ECMWF_IFS'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           conv_name='tntc'
           multiplier=86400.           
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=1.
           pbl_multiplier=1.           
        END
        'miroc' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/miroc'
           plot_desc='MIROC5'
           model_desc='miroc5' 
           valid_dates=standard_valid_dates
           has_tntd=0
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           multiplier=86400.           
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=86400.
        END
        'mri' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/mri'
           model_desc='MRI-AGCM'
           plot_desc='MRI-AGCM'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           multiplier=86400.
           longitude_name='lon'
           latitude_name='lat'
           level_name='plev'
           level_multiplier=0.01
           reverse_level=0
           precip_multiplier=86400.
        END        
        'nasa' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/nasa'
           model_desc='GEOS5_AGCM'
           plot_desc='GEOS5_AGCM'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           multiplier=86400.
           longitude_name='lon'
           latitude_name='lat'
           level_name='lev'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=86400.
        END
        'nrl' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/nrl'
           model_desc='NGEM01'
           plot_desc='NRL_NavGEM01'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1.
           reverse_level=1
           precip_multiplier=86400.
        END
        'giss' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/giss'
           model_desc='ModelE'
           plot_desc='GISS_ModelE2'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=1
           pbl_name='tntpbl'
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=1.
        END
        'cancm4' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/cancm4'
           model_desc='CanCM4'
           plot_desc='CCCma_CanCM4'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=-999
           pbl_name='tntscpbl'
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=0.01
           reverse_level=0
           precip_multiplier=86400.
        END
        'metum' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/metum'
           model_desc='MetUM'
           plot_desc='MetUM_GA3'
           valid_dates=standard_valid_dates
           has_tntd=1
           combine_lsc_lsp=0
           pbl_name='tntpbl'
           multiplier=72.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1
           reverse_level=0
           precip_multiplier=86400.
           conv_name='tntc'
           pbl_multiplier=1.
        END
        'spcam' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/spcam'
           model_desc='SPCAM3.0'
           plot_desc='SPCAM3'
           valid_dates=standard_valid_dates
           has_tntd=0
           combine_lsc_lsp=-999
           conv_name='tntsp'
           pbl_name='tntpbl'
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='lev_p'
           level_multiplier=1
           reverse_level=0
           precip_multiplier=86400.
           pbl_multiplier=-1./86400.
        END
        'cam5' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/cam5'
           model_desc='NCAR.CAM5'
           plot_desc='NCAR_CAM5'
           color_scale=26
           valid_dates=standard_valid_dates
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='levels'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=86400.*1000.           
        END
        'cam5zm' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/cam5zm'
           model_desc='CAM5ZMMicroCAPT'           
           plot_desc='LLNL_CAM5ZM'
           valid_dates=standard_valid_dates
           multiplier=86400.
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='level'
           level_multiplier=1.
           reverse_level=0
           precip_multiplier=86400. 
        END
        'ecearth' : BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/ecearth'
           model_desc='ecearth3'
           plot_desc='SHMI_ECEarth3'
           multiplier=86400.
           valid_dates=standard_valid_dates
           longitude_name='lon'
           latitude_name='lat'
           level_name='lev'
           reverse_level=0.
           precip_multiplier=86400.
           level_multiplier=1.
        END
        'cnrm_atmos': BEGIN
           indir='/home/ss901165/um_output6/mjodiab_20day/cnrm_atmos'
           model_desc='CNRM'
           plot_desc='CNRM_AM'
           multiplier=86400.
           valid_dates=standard_valid_dates
           longitude_name='longitude'
           latitude_name='latitude'
           level_name='lev'
           reverse_level=0
           precip_multiplier=86400.
           level_multiplier=1.
        END
     ENDCASE

     model_offset=where(valid_dates eq start_date)
     date_offset=where(dates eq start_date)

     FOR j=0,n_days-1 DO BEGIN        
        
      today_year=STRMID(dates(j+date_offset),0,4)
      today_month=STRMID(dates(j+date_offset),4,2)
      today_date=STRMID(dates(j+date_offset),6,2)
      
      FOR k=0,n_lead_times-1 DO BEGIN
          
         init_year=STRMID(dates(j+date_offset-our_lead_times(k)),0,4)
         init_month=STRMID(dates(j+date_offset-our_lead_times(k)),4,2)
         init_date=STRMID(dates(j+date_offset-our_lead_times(k)),6,2)

         offset=REFORM(j+date_offset-our_lead_times(k))
         IF where(valid_dates eq dates(offset(0))) eq -1 THEN BEGIN            
                                ;print,'No hindcast for '+dates(offset(0))+' at lead time '+STRTRIM(STRING(our_lead_times(k)),1)
            this_model_field_aavg(k,j*n_times_per_day:(j+1)*n_times_per_day-1,*)=!Values.F_NaN
         ENDIF ELSE BEGIN
            IF our_lead_times(k) eq 19 THEN BEGIN
               count=n_times_per_day-1
            ENDIF ELSE $
               count=n_times_per_day
            initial_date=dates(j+date_offset-our_lead_times(k))
                                ;print,initial_date
            IF field eq 'q1_qr' THEN BEGIN
               tntc_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.tnt_physics.'+REFORM(initial_date)+'.00Z.nc'
               longitude=OPEN_AND_EXTRACT(tntc_infile(0),longitude_name)
               latitude=OPEN_AND_EXTRACT(tntc_infile(0),latitude_name)
               DEFINE_BOUNDARIES,box,latitude,longitude,model_box_tx,/LIMIT
               model_nlon=N_ELEMENTS(longitude)
               model_nlat=N_ELEMENTS(latitude)
               z=OPEN_AND_EXTRACT(tntc_infile(0),level_name)*level_multiplier
               model_nz=N_ELEMENTS(z)
               
               IF j eq 0 and k eq 0 THEN BEGIN
                  this_model_field_aavg=fltarr(n_lead_times,n_days*n_times_per_day,model_nz)
                  IF KEYWORD_SET(plot_precip) THEN $
                     this_model_precip_aavg=fltarr(n_lead_times,n_days*n_times_per_day)
               ENDIF

               tntc=OPEN_AND_EXTRACT(tntc_infile(0),'tnt_physics',$
                                     offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
                                     count=[model_nlon,model_nlat,model_nz,count])*multiplier

               ; IF has_tntd THEN BEGIN
               ;    tntd_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.tntd.'+REFORM(initial_date)+'.00Z.nc'
               ;    tntd=OPEN_AND_EXTRACT(tntd_infile(0),'tntd',$
               ;                          offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
               ;                          count=[model_nlon,model_nlat,model_nz,count])*multiplier
               ; ENDIF ELSE BEGIN ; No "other terms" (tntd) file; set to 0.
               ;    tntd=0.
               ; ENDELSE

               ; tntpbl_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.'+pbl_name+'.'+REFORM(initial_date)+'.00Z.nc'
               ; tntpbl=OPEN_AND_EXTRACT(tntpbl_infile(0),pbl_name,$
               ;                       offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
               ;                       count=[model_nlon,model_nlat,model_nz,count])*multiplier*pbl_multiplier

               ; IF combine_lsc_lsp eq 1 THEN BEGIN
               ;    tntlscp_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.tntlscp.'+REFORM(initial_date)+'.00Z.nc'
               ;    tntlscp=OPEN_AND_EXTRACT(tntlscp_infile(0),'tntlscp',$
               ;                             offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
               ;                             count=[model_nlon,model_nlat,model_nz,count])*multiplier
               ; ENDIF ELSE IF combine_lsc_lsp eq 0 THEN BEGIN
               ;                  ; Add code here to deal with separate cloud and ls-precip files.
               ;    tntlscld_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.tntlscld.'+REFORM(initial_date)+'.00Z.nc'
               ;    tntlscld=OPEN_AND_EXTRACT(tntlscld_infile(0),'tntlscld',$
               ;                              offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
               ;                              count=[model_nlon,model_nlat,model_nz,count])*multiplier
               ;    tntlsp_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.tntlsp.'+REFORM(initial_date)+'.00Z.nc'
               ;    tntlsp=OPEN_AND_EXTRACT(tntlsp_infile(0),'tntlsp',$
               ;                            offset=[model_box_tx(1),model_box_tx(0),0,our_lead_times(k)*n_times_per_day],$
               ;                            count=[model_nlon,model_nlat,model_nz,count])*multiplier                  
               ;    tntlscp=tntlscld+tntlsp
               ; ENDIF ELSE $     ; No lsc/lsp files; set to 0.
               ;    tntlscp=0.
               
               ;today_field=tntc+tntd+tntlscp+tntpbl
               today_field=tntc
               IF TOTAL(where(ABS(today_field) ge 9999)) ge 0 THEN $
                  today_field[where(ABS(today_field) ge 9999)]=!Values.F_NaN               
               
               IF reverse_level eq 1 THEN BEGIN
                  FOR m=0,model_nz-1 DO $
                     FOR n=0,count-1 DO $
                        this_model_field_aavg(k,j*n_times_per_day+n,model_nz-m-1)=$
                     MEAN(today_field(*,*,m,n))
                  z=REVERSE(z)
               ENDIF ELSE BEGIN
                  FOR m=0,model_nz-1 DO $
                     FOR n=0,count-1 DO $
                        this_model_field_aavg(k,j*n_times_per_day+n,m)=$
                     MEAN(today_field(*,*,m,n))
               ENDELSE

               IF KEYWORD_SET(plot_precip) THEN BEGIN
                  precip_infile=indir+'/'+REFORM(initial_date)+'/'+model_desc+'.pr.'+REFORM(initial_date)+'.00Z.nc'
                  precip_longitude=OPEN_AND_EXTRACT(precip_infile(0),longitude_name)
                  precip_latitude=OPEN_AND_EXTRACT(precip_infile(0),latitude_name)
                  DEFINE_BOUNDARIES,box,precip_latitude,precip_longitude,precip_box_tx,/LIMIT
                  precip_nlon=N_ELEMENTS(precip_longitude)
                  precip_nlat=N_ELEMENTS(precip_latitude)
                  
                  precip=OPEN_AND_EXTRACT(precip_infile(0),'pr',$
                                          offset=[precip_box_tx(1),precip_box_tx(0),our_lead_times(k)*n_times_per_day],$
                                          count=[precip_nlon,precip_nlat,count])*precip_multiplier
                  FOR n=0,count-1 DO $
                     this_model_precip_aavg(k,j*n_times_per_day+n)=MEAN(precip(*,*,n))
               ENDIF
            ENDIF ELSE BEGIN
                                ; Add code here to deal with other heating fields (e.g., q1, qr separately)
               
            ENDELSE
         ENDELSE
      ENDFOR      
   ENDFOR
     
     FOR k=0,n_lead_times-1 DO BEGIN
        FOR m=0,model_nz-1 DO $
           this_model_field_aavg(k,*,m)=SMOOTH(this_model_field_aavg(k,*,m),9,/NaN)
        IF KEYWORD_SET(plot_precip) THEN $
           this_model_precip_aavg(k,*)=SMOOTH(this_model_precip_aavg(k,*),9,/NaN)
     ENDFOR
     
     FOR k=0,n_lead_times-1 DO BEGIN
        psfile='/home/ss901165/idl/mjo_diabatic/diabatic_animations/mjo_diabatic_animations_heating_timevert.'+plot_desc+'.'+field_desc+'.'+$
               'latavg'+STRTRIM(STRING(box(0)),1)+'to'+STRTRIM(STRING(box(2)),1)+$
               '_lonavg'+STRTRIM(STRING(box(1)),1)+'to'+STRTRIM(STRING(box(3)),1)+$
               '.'+start_date+'-'+stop_date+'_lead'+STRTRIM(STRING(our_lead_times(k)),1)+'.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2300,SPACE2=1000,XOFFSET=700,YOFFSET=500,TFONT=2,TCHARSIZE=90,CB_WIDTH=110
        GSET,XMIN=where(dates eq start_date),XMAX=where(dates eq stop_date)+1,YMIN=MAX(z)+25,YMAX=MIN(z)
        CS,SCALE=1,NCOLS=N_ELEMENTS(levels)+1,white=[11]
        LEVS,MANUAL=levels
        test=where(dates eq start_date)
        CON,X=findgen(n_days*n_times_per_day)/FLOAT(n_times_per_day)+test(0),Y=z,$
            FIELD=REFORM(this_model_field_aavg(k,*,*)),POSITIVE_STYLE=0,NEGATIVE_STYLE=1,/NOLINELABELS,/NOLINES,$
            CB_TITLE=cb_title,TITLE='Heating from '+plot_desc+$
            ' area-averaged lat '+STRTRIM(STRING(box(0)),1)+' to '+STRTRIM(STRING(box(2)),1)+' , lon '+STRTRIM(STRING(box(1)),1)+$
            ' to '+STRTRIM(STRING(box(3)),1)+' lead time = '+STRTRIM(STRING(our_lead_times(k)),1)+' days'
        valid_start=0
        valid_stop=n_days*n_times_per_day-1
        j=0
        WHILE valid_start eq 0 and j lt n_days*n_times_per_day DO BEGIN         
           IF FINITE(this_model_field_aavg(k,j,0)) eq 1 THEN $
              valid_start=j/FLOAT(n_times_per_day)+test(0)
           j=j+1
        ENDWHILE
        j=n_days*n_times_per_day-1
        WHILE valid_stop eq n_days*n_times_per_day-1 and j ge 0 DO BEGIN
           IF FINITE(this_model_field_aavg(k,j,0)) eq 1 THEN $
              valid_stop=j/FLOAT(n_times_per_day)+test(0)
           j=j-1
        ENDWHILE
        print,valid_start,valid_stop
        GPLOT,X=[valid_start,valid_start],Y=[MAX(z)+25,MIN(z)],THICK=200,COL=FSC_COLOR('black')
        GPLOT,X=[valid_stop,valid_stop],Y=[MAX(z)+25,MIN(z)],THICK=200,COL=FSC_COLOR('black')

        IF KEYWORD_SET(plot_precip) THEN BEGIN
           AXES,YVALS=[1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50],$
                YTITLE='Pressure (hPa)',XTITLE='Date',XLABELS=dates[test(0)+indgen(n_days/5+1)*5],$
                XVALS=indgen(n_days/5+1)*5+test(0),ORIENTATION=20,/NORIGHT
           GSET,XMIN=where(dates eq start_date),XMAX=where(dates eq stop_date)+1,YMIN=0,YMAX=40
           precip_plot=REFORM(this_model_precip_aavg(k,*))
           precip_plot(0:(valid_start-test(0))*n_times_per_day)=!Values.F_NaN
           precip_plot((valid_stop-test(0))*n_times_per_day-n_times_per_day:n_days*n_times_per_day-1)=!Values.F_NaN
           GPLOT,X=findgen(n_days*n_times_per_day)/FLOAT(n_times_per_day)+test(0),Y=precip_plot
           AXES,YSTEP=2,YMINOR=1,YTITLE='Area-averaged precipitation (mm day!U-1!N)',/ONLYRIGHT
        ENDIF ELSE $
           AXES,YVALS=[1000,950,900,850,800,750,700,650,600,550,500,450,400,350,300,250,200,150,100,50],$
                YTITLE='Pressure (hPa)',XTITLE='Date',XLABELS=dates[test(0)+indgen(n_days/5+1)*5],$
                XVALS=indgen(n_days/5+1)*5+test(0),ORIENTATION=20        
        
                                ; Print black lines at beginning and end of valid period at this lead time        
        PSCLOSE,/NOVIEW

     ENDFOR
  ENDFOR
  
  STOP
END

  
