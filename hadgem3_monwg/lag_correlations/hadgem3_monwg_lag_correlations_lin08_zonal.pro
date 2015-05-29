PRO hadgem3_monwg_lag_correlations_lin08_zonal

gpcp_file='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso_anom_filter3050.monsoon_domain.n96.nc'
trmm_file='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter3050.dmeans.monsoon_domain.n96.nc'
ahjra_file='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.anom_filter3050.nc'
ahrqc_file='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.anom_filter3050.nc'
ahsaf_file='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.anom_filter3050.nc'

lon_range=[60,180]
latavg_range=[-10,10]
box=[latavg_range(0),lon_range(0),latavg_range(1),lon_range(1)]
lags=indgen(41)-20
n_lags=N_ELEMENTS(lags)

corr_pt=[0,95]
sig_level_gpcp=0.246
sig_level_model=0.185

n_sets=5
mylevs=['-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7']

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
        0: BEGIN
            infile=gpcp_file
            plot_title='GPCP (N96, 1997-2007)'
            psfile_title='gpcp'
            filter_start_read=91-50/2
            n_days=62
            n_years=11
            multiplier=[1,1]
            sig_level=sig_level_gpcp
        END
        1: BEGIN
            infile=trmm_file
            plot_title='TRMM (N96, 1999-2008)'
            psfile_title='trmm'
            filter_start_read=91-50/2
            n_days=62
            n_years=10
            multiplier=[1.,1]
            sig_level=sig_level_gpcp
        END
        2: BEGIN
            infile=ahjra_file
            plot_title='HadGEM3-A (ahjra, rh-based, 20 years)'
            psfile_title='ahjra'
            filter_start_read=90-50/2
            n_days=60
            n_years=20
            multiplier=[86400.,1.00]
            sig_level=sig_level_model
        END
        3: BEGIN
            infile=ahrqc_file
            plot_title='HadGEM3-A (ahrqc, w-based, 20 years)'
            psfile_title='ahrqc'
            filter_start_read=90-50/2
            n_days=60
            n_years=20
            multiplier=[86400.,1.00]
            sig_level=sig_level_model
        END
        4: BEGIN
            infile=ahsaf_file
            plot_title='HadGEM3-AO (ahsaf, w-based, 30 years)'
            psfile_title='ahsaf'
            filter_start_read=90-50/2
            n_days=60
            n_years=30
            multiplier=[86400,1.00]
            sig_level=sig_level_model
        END
    ENDCASE

    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lon=N_ELEMENTS(longitude)
    n_lat=N_ELEMENTS(latitude)
    corr_pt_lon=NEAREST(longitude,corr_pt(1))
    corr_pt_lat=NEAREST(latitude,corr_pt(0))
                                ; Read filtered rainfall for each year
                                ; and take the longitude average
    thisyear_precip_filtered_latavg=fltarr(n_lon,n_days)
    thisyear_precip_filtered_pt=fltarr(n_days)
    correlation=fltarr(n_lon,n_lags)
    FOR j=0,n_years-1 DO BEGIN
        thisyear_precip_filtered=REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                                         offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                                         count=[n_lon,n_lat,1,n_days]))*multiplier(0)
        
        IF TOTAL(where(thisyear_precip_filtered ge 1E20)) gt 0 THEN $
          thisyear_precip_filtered(where(thisyear_precip_filtered ge 1E20)) = !Values.F_NaN
        
        FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_days-1 DO BEGIN
                thisyear_precip_filtered_latavg(k,m)=MEAN(thisyear_precip_filtered(k,*,m),/NaN)
                IF FINITE(thisyear_precip_filtered_latavg(k,m)) eq 0 THEN $
                  thisyear_precip_filtered_latavg(k,m)=thisyear_precip_filtered_latavg(k,m-1)
            ENDFOR
        ENDFOR
        thisyear_precip_filtered_pt=REFORM(thisyear_precip_filtered(corr_pt_lon,corr_pt_lat,*))
        FOR m=0,n_days-1 DO BEGIN
            IF FINITE(thisyear_precip_filtered_pt(m)) eq 0 THEN $
              thisyear_precip_filtered_pt(m)=thisyear_precip_filtered_pt(m-1)
        ENDFOR

        FOR k=0,n_lon-1 DO $
          correlation(k,*)=correlation(k,*)+C_CORRELATE(thisyear_precip_filtered_pt,REFORM(thisyear_precip_filtered_latavg(k,*)),lags)/float(n_years)
        IF FINITE(correlation(0,0)) eq 0 THEN BEGIN
            print,'NaN correlation detected at i=',i,'j=',j
            STOP
        ENDIF
    ENDFOR

    psfile='/home/ss901165/idl/hadgem3_monwg/lag_correlations/hadgem3_monwg_lag_correlations_lin08_zonal.'+psfile_title+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=1000,XOFFSET=4000,YOFFSET=1500,TFONT=2,TCHARSIZE=100
    GSET,XMIN=MIN(longitude),XMAX=MAX(longitude),YMIN=MAX(lags),YMAX=MIN(lags)
    CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
    LEVS,MANUAL=mylevs
    CON,FIELD=correlation*multiplier(1),X=longitude,Y=lags,$
      TITLE='Lag correlation of 30-50 day rainfall as in Lin et al. (2008) - '+plot_title,$
      /NOLINES
    black=FSC_COLOR("black",30)
    FOR j=0,n_lags-1 DO BEGIN
        IF TOTAL(where(ABS(correlation(*,j)*multiplier(1)) gt sig_level)) ne -1 THEN $
          GPLOT,Y=REPLICATE(lags(j),N_ELEMENTS(where(ABS(correlation(*,j)*multiplier(1)) gt sig_level))),$
          X=longitude[where(ABS(correlation(*,j)*multiplier(1)) gt sig_level)],$
          SYM=3,COL=30,/NOLINES,SIZE=30
    ENDFOR
    AXES,xvals=[60,80,100,120,140,160,180],$
      YSTEP=-5,XTITLE='Longitude (positive east)',YTITLE='Lag (days, positive indicates (0N, 95E) is leading)'
    PSCLOSE,/NOVIEW
ENDFOR

STOP

END



