PRO hadgem3_monwg_isv_metrics_autocorr

gpcp_infile='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso.anom_filter2120.monsoon_domain.n144.nc'
ahjra_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.anom_filter2120.nc'
ahrqc_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.anom_filter2120.nc'
ahsaf_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.anom_filter2120.nc'
ahtpl_infile='/home/ss901165/um_output/hadgem3_monwg/ahtpl/ahtpl.precip.apr-oct.daily_12years.anom_filter2120.nc'

airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans_anom_filter2120.1982-2008.precip.nc'

ageyb_infile='/home/ss901165/um_output3/hadgem3_monwg/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans_anom_filter2120.1979-1998.precip.nc'

agkfc_infile='/home/ss901165/um_output3/hadgem3_monwg/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans_anom_filter2120..years1-30.precip.nc'

xemjo_infile='/home/ss901165/um_output3/xemjo/hadgem3ha_morph3_final_n216_amip2_xemjo.jan-dec_dmeans_anom_filter2120..1982-2008.precip.nc'

aicvx_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans_anom_filter2120.years1-30.precip.nc'

aisas_infile='/home/ss901165/um_output3/hadgem3_monwg/aisas/hadgem3hao_morph3_final_n216_orca025_aisas.jan-dec_dmeans_anom_filter2120.years1-22.precip.nc'

imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.anom_filter2120.nc'
trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter2120.dmeans.monsoon_domain.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter2120.nc'

box=[15,75,25,85]
units=['N','E','N','E']
IF box(0) lt 0 THEN units(0)='S'
IF box(2) lt 0 THEN units(2)='S'
region_name='CentIndia'
n_sets=7

n_lags=51
lags=indgen(n_lags)-n_lags/2

color_numbers=[30,31,32,33,34,35,36]
color_names=["black","black","blue","blue","red","purple","orange"]
style_numbers=[0,2,2,0,0,0,0]

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
        0: BEGIN
            infile=trmm_n96_infile
            varname='precip'
            plot_title='TRMM (N96, 1999-2008, 10 years)'
            psfile_title='trmm_n96'
            start_read=61-120/2
            n_days=122
            n_years=10
            fived=0
            multiplier=1
         END
        1: BEGIN
            infile=trmm_n216_infile
            varname='precip'
            plot_title='TRMM (N216, 1999-2008, 10 years)'
            psfile_title='trmm_n216'
            start_read=61-120/2
            n_days=122
            n_years=10
            fived=0
            multiplier=1
         END
        2: BEGIN
           infile=airxv_infile
           varname='precip'
           plot_title='HadGEM3-A MORPH3 final assessment (N96, 1982-2008)'
           psfile_title='airxv'
           start_read=180-120/2
           n_days=120
           n_years=27
           fived=0
           multiplier=86400.
        END
        3: BEGIN
           infile=imd_infile
           varname='rf'
           plot_title='IMD (1x1 gridded, 1951-2004, 54 years)'
           psfile_title='imd_1x1'
           start_read=61-120/2
           n_days=92
           n_years=54
           fived=0
           multiplier=1
        END
     ENDCASE

    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)

    print,i
    allyears_precip_boxavg=fltarr(n_days*n_years)
    thisyear_precip_boxavg=fltarr(n_days)
    FOR j=0,n_years-1 DO BEGIN
        thisyear_precip=$
          REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                  offset=[box_tx(1),box_tx(0),j,start_read],$
                                  count=[n_lon,n_lat,1,n_days]))*multiplier
        IF TOTAL(where(thisyear_precip ge 1E20)) ge 0 THEN $
          thisyear_precip[where(thisyear_precip ge 1E20)] = !Values.F_NaN
        FOR k=0,n_days-1 DO BEGIN
            allyears_precip_boxavg(j*n_days+k)=MEAN(thisyear_precip(*,*,k),/NaN)
            IF FINITE(allyears_precip_boxavg(j*n_days+k)) eq 0 THEN $
              allyears_precip_boxavg(j*n_days+k)=allyears_precip_boxavg(j*n_days+k-1)
            thisyear_precip_boxavg(k)=MEAN(thisyear_precip(*,*,k),/NaN)
            IF FINITE(thisyear_precip_boxavg(k)) eq 0 THEN $
              thisyear_precip_boxavg(k)=thisyear_precip_boxavg(k-1)
        ENDFOR

        IF j eq 0 THEN $
          acorr_mean=fltarr(n_lags)

        acorr_mean(*)=acorr_mean(*)+A_CORRELATE(thisyear_precip_boxavg,lags)/FLOAT(n_years)
    ENDFOR

    IF i eq 0 THEN BEGIN
        psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_autocorr.filter2120.ps'
        PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=3000,SPACE2=300,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=100
        items=[plot_title(0)]
        colors=[color_numbers(0)]
        styles=[style_numbers(0)]
        GSET,XMIN=MIN(lags),XMAX=MAX(lags),YMIN=-0.8,YMAX=1,$
          TITLE='For rain aavg ('+STRTRIM(STRING(ABS(box(0))),1)+units(0)+'-'+$
          STRTRIM(STRING(ABS(box(2))),1)+units(2)+', '+STRTRIM(STRING(box(1)),1)+units(1)+'-'+STRTRIM(STRING(box(3)),1)+units(3)+')'$
          +' autocorrelation of 2-120 day filtered rainfall for JJA'
        AXES,XSTEP=5,XTITLE="Lag days (positive lagging)",YSTEP=0.2,YTITLE="Autocorrelation of filtered timeseries",NDECS=2
    ENDIF ELSE BEGIN
        items=[items,plot_title]
        colors=[colors,color_numbers(i)]
        styles=[styles,style_numbers(i)]
    ENDELSE

    color=FSC_COLOR(color_names(i),color_numbers(i))
    GPLOT,Y=acorr_mean,X=lags,THICK=200,COL=color_numbers(i),STYLE=style_numbers(i)

ENDFOR

GPLOT,Y=REPLICATE(0,n_lags),X=lags,STYLE=1
GPLOT,Y=REPLICATE(0.304,n_lags),X=lags,STYLE=1,COL=color_numbers(0)
GPLOT,Y=REPLICATE(-0.304,n_lags),X=lags,STYLE=1,COL=color_numbers(0)
GPLOT,Y=REPLICATE(0.282,n_lags),X=lags,STYLE=1,COL=color_numbers(5)
GPLOT,Y=REPLICATE(-0.282,n_lags),X=lags,STYLE=1,COL=color_numbers(5)
GPLOT,Y=REPLICATE(0.240,n_lags),X=lags,STYLE=1,COL=color_numbers(2)
GPLOT,Y=REPLICATE(-0.240,n_lags),X=lags,STYLE=1,COL=color_numbers(2)
GPLOT,Y=REPLICATE(0.201,n_lags),X=lags,STYLE=1,COL=color_numbers(4)
GPLOT,Y=REPLICATE(-0.201,n_lags),X=lags,STYLE=1,COL=color_numbers(4)
GPLOT,Y=REPLICATE(0.134,n_lags),X=lags,STYLE=1,COL=color_numbers(6)
GPLOT,Y=REPLICATE(-0.134,n_lags),X=lags,STYLE=1,COL=color_numbers(6)

LEGEND,labels=REVERSE(items),COL=REVERSE(color_numbers),LEGPOS=7,STYLE=REVERSE(style_numbers)

PSCLOSE

STOP
END
