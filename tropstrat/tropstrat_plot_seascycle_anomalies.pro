PRO tropstrat_plot_seascycle_anomalies
  
; Plot mean seasonal cycle and distribution of anomalies therefrom for 10 hPa, 60N winds

  n_sets=4
  names=['KPP coupled','Atmos clim SST','Atmos 31-day SST','ERA-Interim']
  colors=['red','blue','purple','black']
  ndpy=150
  day_start=300
  year_limit=1
  clim=fltarr(n_sets,ndpy)
  raw_bins=findgen(21)*6-40
  anom_bins=findgen(21)*4-40
  n_bins=N_ELEMENTS(anom_bins)
  n_raw_bins=N_ELEMENTS(raw_bins)
  anom_binned=fltarr(n_sets,n_bins+1)
  ts_binned=fltarr(n_sets,n_raw_bins+1)

  FOR i=0,n_sets-1 DO BEGIN
     CASE i OF
        0 : BEGIN
           infiles=['/export/niagara/data-06/cy000010/um_output/xilai/process/hadgem3kpp_nrglobal1_n96.jan-dec_dmeans_ts.years1-44.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilaj/process/hadgem3kpp_nrglobal2_n96.jan-dec_dmeans_ts.years1-25.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilak/process/hadgem3kpp_nrglobal3_n96.jan-dec_dmeans_ts.years1-25.u10_60N.nc']
           n_years=75
           ndpy_model=360
           varname='u'
        END     
        1 : BEGIN         
           infiles=['/export/niagara/data-06/cy000010/um_output/xilap/process/hadgem3a_nrglobal1_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilaq/process/hadgem3a_nrglobal2_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilar/process/hadgem3a_nrglobal3_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc']
           n_years=75
           ndpy_model=360
           varname='u'
        END 
        2 : BEGIN         
           infiles=['/export/niagara/data-06/cy000010/um_output/xilas/process/hadgem3a_kppnrglobal1smooth31_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilat/process/hadgem3a_kppnrglobal2smooth31_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc',$
                    '/export/niagara/data-06/cy000010/um_output/xilau/process/hadgem3a_kppnrglobal3smooth31_n96_v2.jan-dec_dmeans_ts.years1-25.u10_60N.nc']
           n_years=75
           ndpy_model=360
           varname='u'
        END
        3 : BEGIN
           infiles=['/home/ss901165/datasets/ERA-INTERIM/U10/ERA_Interim.jan-dec_dmeans.1979-2013.u10hPa_60N.nc']
           n_years=35
           ndpy_model=365
           varname='U'
           day_start=305
        END
     ENDCASE
     
     n_files=N_ELEMENTS(infiles)
     n_years_file=n_years/n_files
     ts=fltarr(n_files,ndpy_model*n_years_file)
     ts_restrict=fltarr(ndpy*(n_years-year_limit*n_files))
     anom=fltarr(ndpy*(n_years-year_limit*n_files))     
     FOR j=0,n_files-1 DO $
        ts(j,*)=OPEN_AND_EXTRACT(infiles(j),varname,offset=[0],count=[n_years_file*ndpy_model])
     FOR j=0,ndpy-1 DO BEGIN
        print,j+day_start,ndpy_model*n_years_file-1,ndpy_model
        clim(i,j)=MEAN(ts(*,j+day_start:ndpy_model*n_years_file-1:ndpy_model))
     ENDFOR
     FOR j=0,n_files-1 DO BEGIN
        FOR k=0,n_years_file-year_limit-1 DO BEGIN
           print,k*ndpy_model+day_start,k*ndpy_model+day_start+ndpy-1
           anom(j*(n_years_file-year_limit)*ndpy+k*ndpy:j*(n_years_file-year_limit)*ndpy+(k+1)*ndpy-1)=$
              REFORM(ts(j,k*ndpy_model+day_start:k*ndpy_model+day_start+ndpy-1))-REFORM(clim(i,*))
           ts_restrict(j*(n_years_file-year_limit)*ndpy+k*ndpy:j*(n_years_file-year_limit)*ndpy+(k+1)*ndpy-1)=$
              REFORM(ts(j,k*ndpy_model+day_start:k*ndpy_model+day_start+ndpy-1))
        ENDFOR
     ENDFOR
     
     FOR j=0,n_bins-2 DO $
        IF TOTAL(where(anom ge anom_bins(j) and anom lt anom_bins(j+1))) ge 0 THEN $
           anom_binned(i,j+1)=anom_binned(i,j+1)+N_ELEMENTS(where(anom ge anom_bins(j) and anom lt anom_bins(j+1)))
     IF TOTAL(where(anom lt anom_bins(0))) ge 0 THEN $
        anom_binned(i,0)=N_ELEMENTS(where(anom lt anom_bins(0)))
     IF TOTAL(where(anom ge anom_bins(n_bins-1))) THEN $
        anom_binned(i,n_bins)=N_ELEMENTS(where(anom ge anom_bins(n_bins-1)))
     
     anom_binned(i,*)=anom_binned(i,*)/FLOAT(N_ELEMENTS(anom))
     
     FOR j=0,n_raw_bins-2 DO $
        IF TOTAL(where(ts_restrict ge raw_bins(j) and ts_restrict lt raw_bins(j+1))) ge 0 THEN $
           ts_binned(i,j+1)=ts_binned(i,j+1)+N_ELEMENTS(where(ts_restrict ge raw_bins(j) and ts_restrict lt raw_bins(j+1)))
     IF TOTAL(where(ts_restrict lt raw_bins(0))) ge 0 THEN $
        ts_binned(i,0)=N_ELEMENTS(where(ts_restrict lt raw_bins(0)))
     IF TOTAL(where(ts_restrict ge raw_bins(n_raw_bins-1))) THEN $
        ts_binned(i,n_raw_bins)=N_ELEMENTS(where(ts_restrict ge raw_bins(n_raw_bins-1)))
     ts_binned(i,*)=ts_binned(i,*)/FLOAT(N_ELEMENTS(ts_restrict))
     
  ENDFOR
  
  psfile='/home/ss901165/idl/tropstrat/tropstrat_plot_seascycle_anomalies.u10hPa_60N.nov-mar_anompdf.ps'
  PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,TCHARSIZE=100,XOFFSET=500
  ymax=0.2
  GSET,XMIN=0,XMAX=n_bins+1,YMIN=0,YMAX=ymax,TITLE='PDF of anomalies in zonal-mean zonal wind at 10 hPa and 60N - Nov-Mar, all years'
  FOR i=0,n_sets-1 DO $
     GPLOT,X=indgen(n_bins+2)+0.5,Y=REFORM(anom_binned(i,*)),COL=FSC_COLOR(colors(i))
  AXES,XVALS=indgen(n_bins+2),XLABELS=['<',STRMID(STRTRIM(STRING(anom_bins),1),0,3),'>'],$
       YSTEP=0.02,NDECS=2,YTITLE='Probability',XTITLE='Anomaly in zonal-mean zonal wind (m s!U-1!N)'
  GPLOT,X=[n_bins/2+ODD(n_bins),n_bins/2+ODD(n_bins)],Y=[0,ymax],STYLE=2
  GLEGEND,LEGPOS=1,labels=REVERSE(names),COL=REVERSE(FSC_COLOR(colors))
  PSCLOSE,/NOVIEW
  
  psfile='/home/ss901165/idl/tropstrat/tropstrat_plot_seascycle_anomalies.u10hPa_60N.nov-mar_rawpdf.ps'
  PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,TCHARSIZE=100,XOFFSET=500
  ymax=0.2
  GSET,XMIN=0,XMAX=n_raw_bins+1,YMIN=0,YMAX=ymax,TITLE='PDF of zonal-mean zonal wind at 10 hPa and 60N - Nov-Mar, all years'
  FOR i=0,n_sets-1 DO $
     GPLOT,X=indgen(n_raw_bins+2)+0.5,Y=REFORM(ts_binned(i,*)),COL=FSC_COLOR(colors(i))
  AXES,XVALS=indgen(n_raw_bins+2),XLABELS=['<',STRMID(STRTRIM(STRING(raw_bins),1),0,3),'>'],$
       YSTEP=0.02,NDECS=2,YTITLE='Probability',XTITLE='Zonal-mean zonal wind (m s!U-1!N)'
  GPLOT,X=[NEAREST(raw_bins,0)+0.66,NEAREST(raw_bins,0)+0.66],Y=[0,ymax],STYLE=2
  GLEGEND,LEGPOS=9,labels=REVERSE(names),COL=REVERSE(FSC_COLOR(colors))
  PSCLOSE
  
  STOP
END
