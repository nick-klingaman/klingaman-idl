PRO hadgem3_monwg_timestep_twodpdf_darwin

darwin='/home/ss901165/datasets/DARWIN_RADAR'
n_sets=3

heaviside=[1,2,4,6,9,12,16,20,25,30,40,60,90,130,180]
;heaviside=[1,2,4,7,10,20,40,70,100,200,400,700,1000]
n_heavi=N_ELEMENTS(heaviside)
on_levs_ratio=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00']
on_levs=['0.02','0.04','0.06','0.08','0.10','0.12']
off_levs_ratio=['0.01','0.02','0.03','0.04','0.05','0.06','0.07','0.08','0.09','0.10','0.11','0.12','0.13']
off_levs=['0.50','0.60','0.70','0.80','0.90','1.00']
off_levs_map=['0.10','0.20','0.30','0.40','0.50','0.60','0.70','0.80','0.90','1.00']

FOR i=0,n_sets-1 DO BEGIN
    print,i
    CASE i OF
       1 : BEGIN
          infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon_5km_aavg.nc'
          varname='rr'
          plot_title='Darwin radar on 4km grid and 10min - 2005-06'
          start_read=0
          n_time=LONG(24346)
          tsteps_per_day=6*24.
          multiplier=1.
          psfile_title='darwin_0506_4km_10min'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          use_map=1       
          ysize_map=12000          
          xname='longitude'
          yname='latitude'
       END
       0 : BEGIN
          infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon_n1024_aavg.nc'
          varname='rr'
          plot_title='Darwin radar on N1024 grid and 10min - 2005-06'
          start_read=0
          n_time=LONG(24346)
          tsteps_per_day=6*24.
          multiplier=1.
          psfile_title='darwin_0506_n1024_10min'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          use_map=1       
          ysize_map=12000          
          xname='longitude'
          yname='latitude'
       END
       2 : BEGIN
          infile=darwin+'/cpol_rainrate_cappi_2p5km_20051110-20060430_10min_latlon.nc'
          varname='rr'
          plot_title='Darwin radar on 2.5km grid and 10min - 2005-06'
          start_read=0
          n_time=LONG(24346)
          tsteps_per_day=6*24.
          multiplier=1.
          psfile_title='darwin_0506_2p5km_10min'
          mylevs=['0.01','0.02','0.03','0.05','0.08','0.11','0.15','0.21','0.28','0.36','0.47','0.58','0.70','0.84']
          mylevs_norm=['0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95',$
                       '1.05','1.18','1.33','1.53','1.82','2.22','2.86','4','6.67','20']
          row_map=[0]
          column_map=[11,12,13,14]          
          map_levs=['0.003','0.005','0.007','0.01','0.015','0.02','0.03','0.05','0.07','0.1','0.15','0.2']
          mean_levs=['3','6','9','12','15','18','21']
          use_map=1       
          ysize_map=12000          
          xname='x'
          yname='y'
       END
    ENDCASE
    
    twod_pdf_pwet=fltarr(n_heavi+1,n_heavi+1)
    twod_pdf_prct=fltarr(n_heavi+1,n_heavi+1)
    oned_pdf_prct=fltarr(n_heavi+1)
    heaviside_tstep=FLOAT(heaviside)/24.;FLOAT(tsteps_per_day)

    x=OPEN_AND_EXTRACT(infile,xname)
    y=OPEN_AND_EXTRACT(infile,yname)
    nx=N_ELEMENTS(x)
    ny=N_ELEMENTS(y)
    print,nx,ny

    precip=REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                   offset=[0,0,start_read],$    
                                   count=[nx,ny,n_time-start_read]))*multiplier
    IF TOTAL(where(precip lt 0)) ge 0 THEN $
       precip[where(precip lt 0)]=!Values.F_NaN
    IF TOTAL(where(precip gt 1e10)) ge 0 THEN $
       precip[where(precip gt 1e10)]=!Values.F_NaN
    precip_mean=fltarr(nx,ny)
    FOR j=0,nx-1 DO BEGIN
       xdist=300./FLOAT(nx-1)*(j-nx/2)
       FOR k=0,ny-1 DO BEGIN
          ydist=300./FLOAT(ny-1)*(k-ny/2)
          dist=SQRT(xdist^2+ydist^2)
          IF dist gt 150. THEN $
             precip(j,k,*)=!Values.F_NaN
       ENDFOR
    ENDFOR
;    FOR j=0,nx-1 DO $
;       FOR k=0,ny-1 DO $
;          precip_mean(j,k)=MEAN(precip(j,k,*),/NaN)*FLOAT(tsteps_per_day)

    spatial_freq=fltarr(nx,ny)
    spatial_freq_onoff=fltarr(nx,ny)
    spatial_freq_offon=fltarr(nx,ny)
    spatial_freq_on=fltarr(nx,ny)
    spatial_freq_off=fltarr(nx,ny)
    
    valid_pts=0
    FOR j=0,nx-1 DO BEGIN
       FOR k=0,ny-1 DO BEGIN
          ts=REFORM(precip(j,k,*))
          FOR m=0,n_heavi-2 DO BEGIN
             IF TOTAL(where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))) ge 0 THEN BEGIN
                valid=where(ts ge heaviside_tstep(m) and ts lt heaviside_tstep(m+1))
                oned_pdf_prct(m+1)=oned_pdf_prct(m+1)+N_ELEMENTS(valid)
                next=ts[valid+1]
                FOR n=0,n_heavi-2 DO BEGIN
                   IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                      twod_pdf_prct(m+1,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(m+1,n+1)
                ENDFOR
                IF TOTAL(where(next lt heaviside_tstep(0) and next ge 0)) ge 0 THEN BEGIN
                   twod_pdf_prct(m+1,0)=N_ELEMENTS(where(next lt heaviside_tstep(0) and next ge 0))+twod_pdf_prct(m+1,0)
                   IF where(row_map eq m) ge 0 THEN BEGIN
                      spatial_freq_onoff(j,k)=spatial_freq_onoff(j,k)+N_ELEMENTS(where(next lt heaviside_tstep(0) and next ge 0))
                      spatial_freq_on(j,k)=spatial_freq_on(j,k)+N_ELEMENTS(valid)
                   ENDIF
                ENDIF
                IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                   twod_pdf_prct(m+1,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(m+1,n_heavi)
             ENDIF 
          ENDFOR
          IF TOTAL(where(ts lt heaviside_tstep(0) and ts ge 0)) ge 0 THEN BEGIN
             valid=where(ts lt heaviside_tstep(0) and ts ge 0)
             oned_pdf_prct(0)=oned_pdf_prct(0)+N_ELEMENTS(valid)
             next=ts[valid+1]
             spatial_freq_off(j,k)=spatial_freq_off(j,k)+N_ELEMENTS(valid)
             FOR n=0,n_heavi-2 DO BEGIN
                IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN BEGIN
                   twod_pdf_prct(0,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(0,n+1)                
                   IF where(row_map eq n) ge 0 THEN $
                      spatial_freq_offon(j,k)=spatial_freq_offon(j,k)+N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))
                ENDIF
             ENDFOR
             IF TOTAL(where(next lt heaviside_tstep(0) and next ge 0)) ge 0 THEN $
                twod_pdf_prct(0,0)=N_ELEMENTS(where(next lt heaviside_tstep(0) and next ge 0))+twod_pdf_prct(0,0)
             IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                twod_pdf_prct(0,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(0,n_heavi)
          ENDIF 
          IF TOTAL(where(ts ge heaviside_tstep(n_heavi-1))) ge 0 THEN BEGIN
             valid=where(ts ge heaviside_tstep(n_heavi-1))
             oned_pdf_prct(n_heavi)=oned_pdf_prct(n_heavi)+N_ELEMENTS(valid)
             next=ts[valid+1]
             FOR n=0,n_heavi-2 DO BEGIN
                IF TOTAL(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1))) ge 0 THEN $
                   twod_pdf_prct(n_heavi,n+1)=N_ELEMENTS(where(next ge heaviside_tstep(n) and next lt heaviside_tstep(n+1)))+twod_pdf_prct(n_heavi,n+1)             
             ENDFOR
             IF TOTAL(where(next lt heaviside_tstep(0) and next ge 0)) ge 0 THEN $
                twod_pdf_prct(n_heavi,0)=N_ELEMENTS(where(next lt heaviside_tstep(0) and next ge 0))+twod_pdf_prct(n_heavi,0)
             IF TOTAL(where(next ge heaviside_tstep(n_heavi-1))) ge 0 THEN $
                twod_pdf_prct(n_heavi,n_heavi)=N_ELEMENTS(where(next ge heaviside_tstep(n_heavi-1)))+twod_pdf_prct(n_heavi,n_heavi)
          ENDIF       
       ENDFOR
    ENDFOR
    spatial_freq=spatial_freq/FLOAT(n_time)
    spatial_freq_on=spatial_freq_on/FLOAT(n_time)
    spatial_freq_off=spatial_freq_off/FLOAT(n_time)
    spatial_freq_onoff=spatial_freq_onoff/FLOAT(n_time)
    spatial_freq_offon=spatial_freq_offon/FLOAT(n_time)

;    print,valid_pts
    print,TOTAL(oned_pdf_prct)
    print,N_ELEMENTS(where(FINITE(precip) eq 1))
    print,oned_pdf_prct
    oned_pdf_prct=oned_pdf_prct/FLOAT(TOTAL(oned_pdf_prct))
    print,oned_pdf_prct
    
    twod_pdf_prct_norm_col=fltarr(n_heavi+1,n_heavi+1)
    twod_pdf_prct_norm_row=fltarr(n_heavi+1,n_heavi+1)
    FOR m=0,n_heavi DO BEGIN
       twod_pdf_prct_norm_col(m,*)=twod_pdf_prct(m,*)/TOTAL(twod_pdf_prct(m,*))
       twod_pdf_prct_norm_row(*,m)=twod_pdf_prct(*,m)/TOTAL(twod_pdf_prct(*,m))
    ENDFOR
    twod_pdf_prct=twod_pdf_prct/TOTAL(twod_pdf_prct)

    mylevs_prct=['1e-5','2e-5','4e-5','7e-5','1e-4','2e-4','4e-4','7e-4','1e-3','2e-3','4e-3','7e-3','1e-2','2e-2','4e-2','7e-2','1e-1']
    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=100,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_prct)+1,white=[2]
    LEVS,MANUAL=mylevs_prct
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Probability',TITLE=psfile_title
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT    
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    IF TOTAL(where(oned_pdf_prct lt 0.001)) ge 0 THEN $
       oned_pdf_prct[where(oned_pdf_prct lt 0.001)]=!Values.F_NaN
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2,SYM=3
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_col.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    LEVS,MANUAL=mylevs
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_row.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
    LEVS,MANUAL=mylevs
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW
    
    FOR m=0,n_heavi DO BEGIN
       twod_pdf_prct_norm_col(m,*)=twod_pdf_prct_norm_col(m,*)/oned_pdf_prct
       twod_pdf_prct_norm_row(*,m)=twod_pdf_prct_norm_row(*,m)/oned_pdf_prct
    ENDFOR

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_col2.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
    LEVS,MANUAL=mylevs_norm
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_col,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by column and PDF)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_prct_norm_row2.ps'
    PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=110,XSIZE=16500,YSIZE=16500,SPACE2=2000,YOFFSET=1000
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0,YMAX=n_heavi+1
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_norm)+1,white=[12]
    LEVS,MANUAL=mylevs_norm
    CON,X=indgen(n_heavi+1)+0.5,Y=indgen(n_heavi+1)+0.5,FIELD=twod_pdf_prct_norm_row,/BLOCK,/NOLINES,CB_WIDTH=135,CB_TITLE='Normalised probability (by row and PDF)'
    GPLOT,X=[0,n_heavi+1],Y=[0,n_heavi+1],STYLE=1
    AXES,XVALS=indgen(n_heavi+2),YVALS=indgen(n_heavi+2),$
         YLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XLABELS=['<'+STRTRIM(STRING(heaviside(0)),1),STRTRIM(STRING(heaviside),1),'>'+STRTRIM(STRING(heaviside(n_heavi-1)),1)],$
         XTITLE='Precipitation on timestep t (mm day!U-1!N)',YTITLE='Precipitation on timestep t+1 (mm day!U-1!N)',/NORIGHT         
    GSET,XMIN=0,XMAX=n_heavi+1,YMIN=0.001,YMAX=1,/YLOG
    GPLOT,X=indgen(n_heavi+1)+0.5,Y=oned_pdf_prct,STYLE=2
    AXES,YVALS=['0.001','0.0015','0.002','0.003','0.004','0.006','0.008','0.01','0.015','0.02','0.03','0.04','0.06','0.08','0.10','0.15','0.2','0.3','0.4','0.6','0.8','1.0'],YTITLE='Probability of precipitation in column',/ONLYRIGHT,NDECS=2
    PSCLOSE,/NOVIEW



    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_onoff_freq.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    ; IF use_map eq 1 THEN BEGIN
    ;    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ; ENDIF ELSE $
    ;    GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    ; CS,SCALE=26,NCOLS=N_ELEMENTS(map_levs)+1,white=[2]
    ; LEVS,MANUAL=map_levs
    ; IF i ne 5 THEN BEGIN
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are on (>= 40 mm/day) then off (< 1 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq)),1)+' )',0,5)+' - '+psfile_title
    ; ENDIF ELSE $
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are on (>= 60 mm/day) then off (< 1 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq)),1)+' )',0,5)+' - '+psfile_title
    ; LEVS,MANUAL=mean_levs
    ; CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    ; AXES
    ; PSCLOSE,/NOVIEW

    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_on_freq.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    ; IF use_map eq 1 THEN BEGIN
    ;    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ; ENDIF ELSE $
    ;    GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    ; CS,SCALE=26,NCOLS=N_ELEMENTS(map_levs)+1,white=[2]
    ; LEVS,MANUAL=map_levs
    ; IF i ne 5 THEN BEGIN
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are on (>= 40 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq_on)),1)+' )',0,5)+' - '+psfile_title
    ; ENDIF ELSE $
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are on (>= 60 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq_on)),1)+' )',0,5)+' - '+psfile_title
    ; LEVS,MANUAL=mean_levs
    ; CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    ; AXES
    ; PSCLOSE,/NOVIEW

    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_onoff_ratio.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    ; IF use_map eq 1 THEN BEGIN
    ;    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ; ENDIF ELSE $
    ;    GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    ; CS,SCALE=26,NCOLS=N_ELEMENTS(on_levs_ratio)+1,white=[2]
    ; LEVS,MANUAL=on_levs_ratio
    ; IF i ne 5 THEN BEGIN
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_onoff/spatial_freq_on,CB_TITLE='Probability',/NOLINES,/BLOCK,$
    ;        TITLE='Probability that "on" (> 40 mm/day) is followed by "off" (< 1 mm/day) - '+psfile_title
    ; ENDIF ELSE $
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_onoff/spatial_freq_on,CB_TITLE='Probability',/NOLINES,/BLOCK,$
    ;        TITLE='Probability that "on" (> 40 mm/day) is followed by "off" (< 1 mm/day) - '+psfile_title
    ; LEVS,MANUAL=on_levs
    ; CON,X=longitude,Y=latitude,FIELD=spatial_freq_on,/NOFILL,POSITIVE_STYLE=2
    ; AXES
    ; PSCLOSE

    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_off_freq.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    ; IF use_map eq 1 THEN BEGIN
    ;    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ; ENDIF ELSE $
    ;    GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    ; CS,SCALE=26,NCOLS=N_ELEMENTS(off_levs_map)+1,white=[2]
    ; LEVS,MANUAL=off_levs_map
    ; IF i ne 5 THEN BEGIN
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are off (< 1 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq_off)),1)+' )',0,5)+' - '+psfile_title
    ; ENDIF ELSE $
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,CB_TITLE='Fractional frequency of occurrence',/NOLINES,/BLOCK,$
    ;        TITLE='Frac of tsteps that are off (< 1 mm/day) - avg freq = '+$
    ;        STRMID(STRTRIM(STRING(MEAN(spatial_freq_off)),1)+' )',0,5)+' - '+psfile_title
    ; LEVS,MANUAL=mean_levs
    ; CON,X=longitude,Y=latitude,FIELD=precip_mean,/NOFILL,POSITIVE_STYLE=2
    ; AXES
    ; PSCLOSE,/NOVIEW

    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_map_offon_ratio.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=125,TCHARSIZE=90,YSIZE=ysize_map,SPACE2=2000,YOFFSET=1000
    ; IF use_map eq 1 THEN BEGIN
    ;    MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2)
    ; ENDIF ELSE $
    ;    GSET,XMIN=box(1),XMAX=box(3),YMIN=box(0),YMAX=box(2)
    ; CS,SCALE=26,NCOLS=N_ELEMENTS(off_levs_ratio)+1,white=[2]
    ; LEVS,MANUAL=off_levs_ratio
    ; IF i ne 5 THEN BEGIN
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_offon/spatial_freq_off,CB_TITLE='Probability',/NOLINES,/BLOCK,$
    ;        TITLE='Probability that "off" (< 1 mm/day) is followed by "on" (> 40 mm/day) - '+psfile_title
    ; ENDIF ELSE $
    ;    CON,X=longitude,Y=latitude,FIELD=spatial_freq_offon/spatial_freq_off,CB_TITLE='Probability',/NOLINES,/BLOCK,$
    ;        TITLE='Probability that "off" (< 1 mm/day) is followed by "on" (> 40 mm/day) - '+psfile_title
    ; LEVS,MANUAL=off_levs
    ; CON,X=longitude,Y=latitude,FIELD=spatial_freq_off,/NOFILL,POSITIVE_STYLE=2
    ; AXES
    ; PSCLOSE

    ; psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_twodpdf.'+psfile_title+'_scatter_onoff_freq.ps'
    ; PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=160,TCHARSIZE=100,SPACE2=2000,YOFFSET=1000,MARGIN=3000,XOFFSET=1000
    ; GSET,XMIN=0,XMAX=40,YMIN=0,YMAX=0.2,TITLE=plot_title
    ; FOR j=0,nx-1 DO $
    ;    FOR k=0,ny-1 DO $
    ;       GPLOT,X=precip_mean(j,k),Y=spatial_freq(j,k),SYM=5,SIZE=40
    ; GPLOT,X=1,Y=0.18,TEXT='Correlation = '+STRMID(STRTRIM(STRING(CORRELATE(precip_mean,spatial_freq)),1),0,5),ALIGN=0.0
    ; AXES,XTITLE='Mean precipitation (mm day!U-1!N)',YTITLE='Fractional frequency of on/off or off/on',$
    ;      XSTEP=4,XMINOR=2,YSTEP=0.02,YMINOR=0.01,NDECS=2
    ; PSCLOSE,/NOVIEW

 ENDFOR

STOP
END

       
