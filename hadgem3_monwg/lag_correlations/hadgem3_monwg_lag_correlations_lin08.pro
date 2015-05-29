PRO hadgem3_monwg_lag_correlations_lin08

gpcp_file='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso_anom_filter3050.monsoon_domain.n96.nc'
trmm_n96_file='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter3050.dmeans.monsoon_domain.n96.nc'
trmm_n216_file='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter3050.nc'
ahjra_file='/home/ss901165/um_output3/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.anom_filter3050.nc'
ahrqc_file='/home/ss901165/um_output3/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.anom_filter3050.nc'
ahsaf_file='/home/ss901165/um_output3/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.anom_filter3050.nc'
ahtpl_file='/home/ss901165/um_output3/hadgem3_monwg/ahtpl/ahtpl.precip.apr-oct.daily_12years.anom_filter3050.nc'
ailso_file='/home/ss901165/um_output3/hadgem3_monwg/ailso/ailso.precip.apr-oct.daily_18years.anom_filter3050.nc'
aiaux_file='/home/ss901165/um_output3/hadgem3_monwg/aiaux/aiaux.apr-oct_dmeans.h9-j8.precip_filter_3050.nc'
aitcb_file='/home/ss901165/um_output3/hadgem3_monwg/aitcb/aitcb.jan-dec_dmeans.h9-j8.precip_filter_3050.nc'
aitke_file='/home/ss901165/um_output3/hadgem3_monwg/aitke/aitke.jan-dec_dmeans.i0-j2.precip_filter_3050.nc'
aitkh_file='/home/ss901165/um_output3/hadgem3_monwg/aitkh/aitkh.jan-dec_dmeans.h9-i8.precip_filter_3050.nc'
airxv_file='/home/ss901165/um_output3/hadgem3_monwg/airxv/airxv.jan-dec_dmeans.h9-i8.precip_filter_3050.nc'
dge_file='/home/ss901165/um_output3/hadam3_dge/precip3050.nc'
mge_file='/home/ss901165/um_output3/hadam3_mge/precip3050.nc'

lonavg_range=[70,100]
lat_range=[-21,26]
box=[lat_range(0),lonavg_range(0),lat_range(1),lonavg_range(1)]
lags=indgen(41)-20
n_lags=N_ELEMENTS(lags)

corr_pt=[12.5,85]
sig_level_gpcp=0.246
sig_level_model=0.185
high_limit=50

n_sets=13
mylevs=['-0.7','-0.6','-0.5','-0.4','-0.3','-0.2','-0.1','0.0','0.1','0.2','0.3','0.4','0.5','0.6','0.7']

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
        0: BEGIN
           infile=trmm_n96_file
           plot_title='TRMM 3B42v6 (N96, 1999-2008, 10 years)'
           psfile_title='trmm_n96'
           filter_start_read=91-high_limit/2
           n_days=62
           n_years=10
           multiplier=[1,1]
           sig_level=sig_level_gpcp
        END        
        1: BEGIN
           infile=trmm_n216_file
           plot_title='TRMM (N216, 1999-2008, 10 years)'
           psfile_title='trmm_n216'
           filter_start_read=91-high_limit/2
           n_days=62
           n_years=10
           multiplier=[1,1]
           sig_level=sig_level_gpcp
        END
        2: BEGIN
            infile=ahjra_file
            plot_title='HadGEM3-A (ahjra, N96, rh-based, 20 years)'
            psfile_title='ahjra'
            filter_start_read=90-high_limit/2
            n_days=60
            n_years=20
            multiplier=[86400.,1.00]
            sig_level=sig_level_model
        END
        3: BEGIN
            infile=ahrqc_file
            plot_title='HadGEM3-A (ahrqc, N96, w-based, 20 years)'
            psfile_title='ahrqc'
            filter_start_read=90-high_limit/2
            n_days=60
            n_years=20
            multiplier=[86400.,1.00]
            sig_level=sig_level_model
        END
        4: BEGIN
           infile=ahsaf_file
           plot_title='HadGEM3-AO (ahsaf, N96, w-based, 30 years)'
           psfile_title='ahsaf'
           filter_start_read=90-high_limit/2
            n_days=60
            n_years=30
            multiplier=[86400.,1.00]
            sig_level=sig_level_model
         END
        5: BEGIN
           infile=ahtpl_file
           plot_title='HadGEM3-A (ahtpl, N216, w-based, 12 years)'
           psfile_title='ahtpl'
           filter_start_read=90-high_limit/2
           n_days=60
           n_years=12
           multiplier=[86400.,1.00]
           sig_level=sig_level_gpcp
        END
        6: BEGIN
           infile=ailso_file
           plot_title='HadGEM3-A (ailso, N216, w, 18 years)'
           psfile_title='ailso'
           filter_start_read=90-high_limit/2
           n_days=60
           n_years=18
           multiplier=[86400.,1.00]
           sig_level=sig_level_model
        END
        7: BEGIN
           infile=aiaux_file
           plot_title='HadGEM3-A (aiaux, N96, w, 20 years)'
           psfile_title='aiaux'
           filter_start_read=90-high_limit/2
           n_days=60
           n_years=20
           multiplier=[86400.,1.00]
           sig_level=sig_level_model
        END
        8: BEGIN
           infile=aitcb_file
           plot_title='HadGEM3-AO (aitcb, N96, w, 20 years)'
           psfile_title='aitcb'
           filter_start_read=180-high_limit/2
           n_days=60
           n_years=20
           multiplier=[86400.,1.00]
           sig_level=sig_level_model
        END
        9: BEGIN
           infile=aitke_file
           plot_title='HadGEM3-AO (aitke, N96, w, 13 years)'
           psfile_title='aitke'
           filter_start_read=180-high_limit/2
           n_days=60
           n_years=13
           multiplier=[86400.,1.00]
           sig_level=sig_level_gpcp
        END
        10: BEGIN
           infile=aitkh_file
           plot_title='HadGEM3-AO (aitkh, N96, w, 10 years)'
           psfile_title='aitkh'
           filter_start_read=180-high_limit/2
           n_days=60
           n_years=10
           multiplier=[86400.,1.00]
           sig_level=sig_level_gpcp
        END
        11: BEGIN
           infile=mge_file
           plot_title='HadAM3 with daily OSTIA (30 members)'
           psfile_title='hadam3_dge'
           filter_start_read=90-high_limit/2
           n_days=60
           n_years=30
           multiplier=[86400.,1.45]
           sig_level=sig_level_model
        END
        12: BEGIN
           infile=dge_file
           plot_title='HadAM3 with monthly OSTIA (30 members)'
           psfile_title='hadam3_mge'
           filter_start_read=90-high_limit/2
           n_days=60
           n_years=30
           multiplier=[86400.,1.00]
           sig_level=sig_level_model
        END
    ENDCASE

    print,i
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lon=N_ELEMENTS(longitude)
    n_lat=N_ELEMENTS(latitude)
    corr_pt_lon=NEAREST(longitude,corr_pt(1))
    corr_pt_lat=NEAREST(latitude,corr_pt(0))
                                ; Read filtered rainfall for each year
                                ; and take the longitude average
    thisyear_precip_filtered_lonavg=fltarr(n_lat,n_days)
    thisyear_precip_filtered_pt=fltarr(n_days)
    correlation=fltarr(n_lat,n_lags)
    FOR j=0,n_years-1 DO BEGIN
        thisyear_precip_filtered=REFORM(OPEN_AND_EXTRACT(infile,'precip',$
                                                         offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                                         count=[n_lon,n_lat,1,n_days]))*multiplier(0)
        FOR k=0,n_lat-1 DO $
          FOR m=0,n_days-1 DO $
          thisyear_precip_filtered_lonavg(k,m)=MEAN(thisyear_precip_filtered(*,k,m))
        
        thisyear_precip_filtered_pt=REFORM(thisyear_precip_filtered(corr_pt_lon,corr_pt_lat,*))
        
        FOR k=0,n_lat-1 DO $
          correlation(k,*)=correlation(k,*)+C_CORRELATE(thisyear_precip_filtered_pt,REFORM(thisyear_precip_filtered_lonavg(k,*)),lags)/float(n_years)
    ENDFOR
    
    psfile='/home/ss901165/idl/hadgem3_monwg/lag_correlations/hadgem3_monwg_lag_correlations_lin08.'+psfile_title+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=2000,SPACE2=1000,XOFFSET=4000,YOFFSET=1500,TFONT=2,TCHARSIZE=100
    GSET,XMIN=MIN(latitude),XMAX=MAX(latitude),YMIN=MAX(lags),YMAX=MIN(lags)
    CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
    LEVS,MANUAL=mylevs
    CON,FIELD=correlation*multiplier(1),X=latitude,Y=lags,$
      TITLE='Lag correlation of 30-50 day rainfall as in Lin et al. (2008) - '+plot_title,$
      /NOLINES
    black=FSC_COLOR("black",30)
    FOR j=0,n_lags-1 DO BEGIN
        IF TOTAL(where(ABS(correlation(*,j)*multiplier(1)) gt sig_level)) ne -1 THEN $
          GPLOT,Y=REPLICATE(lags(j),N_ELEMENTS(where(ABS(correlation(*,j)*multiplier(1)) gt sig_level))),$
          X=latitude[where(ABS(correlation(*,j)*multiplier(1)) gt sig_level)],$
          SYM=3,COL=30,/NOLINES,SIZE=50
    ENDFOR
    AXES,xvals=[-20,-15,-10,-5,0,5,10,15,20,25],$
      YSTEP=-5,XTITLE='Latitude (positive north)',YTITLE='Lag (days, positive indicates (12N, 85E) is leading)'
    PSCLOSE,/NOVIEW
ENDFOR

STOP

END



