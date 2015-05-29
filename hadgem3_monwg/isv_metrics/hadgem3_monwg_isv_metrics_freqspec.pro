PRO hadgem3_monwg_isv_metrics_freqspec

gpcp_infile='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso.monsoon_domain.n144.nc'
hadgem3a_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.nc'
hadgem3ao_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.nc'

box=[15,75,25,85]
units=['N','E','N','E']
IF box(0) lt 0 THEN units(0)='S'
IF box(2) lt 0 THEN units(2)='S'
region_name='CentIndia'
n_sets=3

color_numbers=[30,31,32]
color_names=["black","blue","red"]

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
        0: BEGIN
            infile=gpcp_infile
            plot_title='GPCP (1997-2007)'
            psfile_title='gpcp'
            start_read=30
            n_days=152
            n_years=11
            fived=0
            multiplier=1
        END
        1: BEGIN
            infile=hadgem3a_infile
            plot_title='HadGEM3-A (ahrqc, 20 years)'
            psfile_title='hadgem3a'
            start_read=30
            n_days=152
            n_years=20
            fived=1
            multiplier=86400.
        END
        2: BEGIN
            infile=hadgem3ao_infile
            plot_title='HadGEM3-AO (ahsaf, 30 years)'
            psfile_title='hadgem3ao'
            start_read=30
            n_days=152
            n_years=30
            fived=1
            multiplier=86400.
        END
    ENDCASE

    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)

    allyears_precip_boxavg=fltarr(n_days*n_years)
    thisyear_precip_boxavg=fltarr(n_days)
    FOR j=0,n_years-1 DO BEGIN
        IF (fived eq 1) THEN BEGIN
            thisyear_precip=$
              REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                      offset=[box_tx(1),box_tx(0),0,start_read,j],$
                                      count=[n_lon,n_lat,1,n_days,1]))*multiplier
        ENDIF ELSE BEGIN
            thisyear_precip=$
              REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                      offset=[box_tx(1),box_tx(0),start_read,j],$
                                      count=[n_lon,n_lat,n_days,1]))*multiplier
        ENDELSE
        FOR k=0,n_days-1 DO BEGIN
            allyears_precip_boxavg(j*n_days+k)=MEAN(thisyear_precip(*,*,k))
            thisyear_precip_boxavg(k)=MEAN(thisyear_precip(*,*,k))
        ENDFOR

        IF j eq 1 THEN STOP
        
;        PSD,thisyear_precip_boxavg,wavenumber,spectral_density,beta,/density

        power=NICKS_POWER_SPECTRUM(thisyear_precip_boxavg,76,out_freq=freq,out_period=period,confidence_ratio=confidence_ratio,/TUKEY,significance=significance)

;        wave=WAVELET(thisyear_precip_boxavg,1,LAG1=A_CORRELATE(thisyear_precip_boxavg,1),fft_theor=fft_theor,period=wave_period,scale=wave_scale)
;        wave_mean=fltarr(N_ELEMENTS(wave_period))
;        FOR k=0,N_ELEMENTS(wave_period)-1 DO BEGIN
;            wave_mean(k)=MEAN(FLOAT(wave(*,k))^2)
;        ENDFOR

        IF j eq 0 THEN BEGIN
            power_ensmean=fltarr(N_ELEMENTS(period))
;            red_ensmean=fltarr(N_ELEMENTS(wave_period))
        ENDIF
        power_ensmean(*)=power_ensmean(*)+power(*)/FLOAT(n_years)
;        red_ensmean(*)=red_ensmean(*)+fft_theor/FLOAT(n_years)*0.6

    ENDFOR

    IF i eq 0 THEN BEGIN
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_freqspec.'+region_name+'.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=100,XOFFSET=900,YOFFSET=500,TFONT=2,TCHARSIZE=100,SPACE3=300
        items=[plot_title(0)]
        colors=[color_numbers(0)]
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers(i)]
    ENDELSE

    period_stop=NEAREST(period,75)

    GSET,XMIN=MIN([2,period]),XMAX=MAX(period(period_stop:N_ELEMENTS(period)-1)),$
      YMIN=0.01,YMAX=100,/YLOG,/XLOG,$
      TITLE='Periodogram for rainfall area-averaged in box ('+STRTRIM(STRING(ABS(box(0))),1)+units(0)+'-'+$
      STRTRIM(STRING(ABS(box(2))),1)+units(2)+', '+STRTRIM(STRING(box(1)),1)+units(1)+'-'+STRTRIM(STRING(box(3)),1)+units(3)+')'
;    GSET,XMIN=MIN(period),XMAX=75,YMIN=0.1,YMAX=300,/YLOG,/XLOG
    yvals=['0.01','0.02','0.05','0.1','0.2','0.5','1.0','2.0','5.0','10.0','20.0','50.0','100.0']
    xvals=[2,3,4,6,8,10,15,20,25,30,40,50,60,70]
    AXES,XVALS=xvals,XTITLE="Period (days)",YTITLE="Power",YVALS=yvals,NDECS=2
    
    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=power_ensmean(period_stop:N_ELEMENTS(period)-1),X=period(period_stop:N_ELEMENTS(period)-1),THICK=200,COL=color_numbers(i)
;    GPLOT,Y=red_ensmean(period_stop:N_ELEMENTS(period)-1),X=period(period_stop:N_ELEMENTS(period)-1),THICK=200,STYLE=1,COL=color_numbers(i)
;    GPLOT,Y=power_ensmean(0:period_stop),X=wave_period(0:period_stop),THICK=200,STYLE=0,COL=color_numbers(i)
;    GPLOT,Y=red_ensmean(0:period_stop),X=wave_period(0:period_stop),THICK=200,STYLE=1,COL=color_numbers(i)
;    GPLOT,Y=sxx(period_stop+1:N_ELEMENTS(sxx)-1),X=period(period_stop:N_ELEMENTS(period)-1),THICK=200,COL=color_numbers(i)
    
ENDFOR

legend,labels=items,col=color_numbers,LEGPOS=7
PSCLOSE

STOP

END

