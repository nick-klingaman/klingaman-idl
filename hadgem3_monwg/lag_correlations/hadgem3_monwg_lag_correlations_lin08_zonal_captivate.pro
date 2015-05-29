PRO hadgem3_monwg_lag_correlations_lin08_zonal_captivate

trmm_n96_file='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2010.anom_filter_3050.n96.nc'
;trmm_n216_file='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter3050.nc'
akkvg_file='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmeans.i2-j7.anom_filter_3050.precip.nc'
ajtzr_file='/home/ss901165/um_output3/hadgem3_monwg/ajtzr/hadgem3ao_captivate_n96_orca1_ajtzr.jan-dec_dmeans.1979-2008.anom_filter_3050.precip.nc'
nick_1xentrain_file='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-30.anom_filter_3050.precip.nc'
nick_15xentrain_file='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-30.anom_filter_3050.precip.nc'
nick_15xentrain_iav_file='/home/ss901165/um_output4/hadgem3a_amip2_iav_1.5xentrain_vn74/hadgem3a_amip2_iav_1.5xentrain_vn74.mar-feb_dmeans.anom_filter_3050.precip.nc'

glosea4_0425_file='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0425_000.prcp_dmeans_anom_filter_3050.nc'
glosea4_0501_file='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0501_000.prcp_dmeans_anom_filter_3050.nc'
glosea4_0509_file='/home/ss901165/um_output3/hadgem3_monwg/glosea4_hindcasts/glosea4_hindcasts.0509_000.prcp_dmeans_anom_filter_3050.nc'

lon_range=[60,180]
latavg_range=[-10,10]
box=[latavg_range(0),lon_range(0),latavg_range(1),lon_range(1)]
lags=indgen(41)-20
n_lags=N_ELEMENTS(lags)

corr_pt=[0,95]
sig_level_gpcp=0.246
sig_level_model=0.185

n_sets=9
mylevs=['-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7']

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      3: BEGIN
         infile=trmm_n96_file
         plot_title='TRMM 3B42v6 (N96, 1999-2010, 12 years)'
         psfile_title='trmm_n96'
         filter_start_read=151
         n_days=120
         n_years=12
         multiplier=[1,1]
         sig_level=0.321
         varname='precip'
      END
      4 : BEGIN
         infile=akkvg_file
         plot_title='GA3.0 AMIP2 (akkvg, 16 years)'
         psfile_title='akkvg_ga30'
         filter_start_read=151
         n_days=120
         n_years=16
         multiplier=[86400,1]
         sig_level=0.312
      END
      5 : BEGIN
         infile=ajtzr_file
         plot_title='GA3.0 N96ORCA1 (ajtzr, 30 years)'
         psfile_title='ajtzr_n96orca1'
         filter_start_read=151
         n_days=120
         n_years=29
         multiplier=[86400,1]
         sig_level=0.274
      END
      6 : BEGIN
         infile=nick_1xentrain_file
         plot_title='Nick control (1x entrain, 30 years)'
         psfile_title='amip2_ctl_vn74'
         filter_start_read=151
         n_days=120
         n_years=30
         multiplier=[86400,1]
         sig_level=0.274
      END
      7 : BEGIN
         infile=nick_15xentrain_file
         plot_title='Nick 1.5x entrain (clim SST, 30 years)'
         psfile_title='amip2_15xentrain_vn74'
         filter_start_read=151
         n_days=120
         n_years=30
         multiplier=[86400,1.25]
         sig_level=0.274
      END
      8 : BEGIN
         infile=nick_15xentrain_iav_file
         plot_title='Nick 1.5x entrain (IAV SST, 30 years)'
         psfile_title='amip2_iav_15xentrain_vn74'
         filter_start_read=91
         n_days=120
         n_years=21
         multiplier=[86400,1.25]
         sig_level=0.274
      END
      0 : BEGIN
         infile=glosea4_0425_file
         plot_title='Glosea4 25/04 start (14 years)'
         psfile_title='glosea4_0425'
         filter_start_read=37
         n_days=115
         n_years=13
         multiplier=[86400,0.9]
         sig_level=0.375
         varname='precipitation_flux'
      END
      1 : BEGIN
         infile=glosea4_0501_file
         plot_title='Glosea4 01/05 start (14 years)'
         psfile_title='glosea4_0501'
         filter_start_read=37
         n_days=115
         n_years=13
         multiplier=[86400,0.9]
         sig_level=0.375
         varname='precipitation_flux'
      END
      2 : BEGIN
         infile=glosea4_0509_file
         plot_title='Glosea4 09/05 start (14 years)'
         psfile_title='glosea4_0509'
         filter_start_read=37
         n_days=115
         n_years=12
         multiplier=[86400,0.9]
         sig_level=0.375
         varname='precipitation_flux'
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
        thisyear_precip_filtered=REFORM(OPEN_AND_EXTRACT(infile,varname,$
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

    psfile='/home/ss901165/idl/hadgem3_monwg/lag_correlations/hadgem3_monwg_lag_correlations_lin08_zonal_captivate.'+psfile_title+'.ps'
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



