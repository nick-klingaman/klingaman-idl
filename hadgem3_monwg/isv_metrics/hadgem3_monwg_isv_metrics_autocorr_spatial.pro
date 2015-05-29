PRO hadgem3_monwg_isv_metrics_autocorr_spatial

ahjra_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.precip.apr-oct.daily_20years.nc'
ahrqc_infile='/home/ss901165/um_output/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.daily_20years.nc'
ahsaf_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.precip.apr-oct.daily_30years.nc'
ahtpl_infile='/home/ss901165/um_output/hadgem3_monwg/ahtpl/ahtpl.precip.apr-oct.daily_12years.nc'
airxv_infile='/home/ss901165/um_output3/hadgem3_monwg/airxv/hadgem3a_morph3_final_n96_amip2_airxv.jan-dec_dmeans.1982-2008.precip.nc'
ageyb_infile='/home/ss901165/um_output3/hadgem3_monwg/ageyb/hadgem2a_final_n96_amip2_ageyb.jan-dec_dmeans.1979-1998.precip.nc'
agkfc_infile='/home/ss901165/um_output3/hadgem3_monwg/agkfc/hadgem2ao_final_n96_agkfc.jan-dec_dmeans.years1-30.precip.nc'
xemjo_infile='/home/ss901165/um_output3/xemjo/hadgem3ha_morph3_final_n216_amip2_xemjo.jan-dec_dmeans.1982-2008.precip.nc'
aicvx_infile='/home/ss901165/um_output3/hadgem3_monwg/aicvx/hadgem3ao_morph3_final_n96_orca1_aicvx.jan-dec_dmeans.years1-30.precip.nc'
aisas_infile='/home/ss901165/um_output3/hadgem3_monwg/aisas/hadgem3hao_morph3_final_n216_orca025_aisas.jan-dec_dmeans.years1-22.precip.nc'
akkvg_infile='/home/ss901165/um_output3/hadgem3_monwg/akkvg/akkvg.jan-dec_dmeans.i2-j7.precip.nc'
hadgem3a_amip2_ctl_infile='/home/ss901165/um_output3/hadgem3a_amip2_control_vn74/hadgem3a_amip2_ctl_vn74.jan-dec_dmeans.years1-30.precip.nc'
hadgem3a_amip2_15xentrain_infile='/home/ss901165/um_output3/hadgem3a_amip2_1.5xentrain_vn74/hadgem3a_amip2_1.5xentrain_vn74.jan-dec_dmeans.years1-30.precip.nc'

trmm_n96_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n96/TRMM_3B42v6A.jan-dec_dmeans.1999-2009.n96.nc'
trmm_n216_infile='/home/ss901165/datasets_mango/TRMM_3B42V6/n216/TRMM_3B42v6A.jan-dec_dmeans.1999-2008.n216.nc'
imd_infile='/home/ss901165/datasets/IMD_GRIDDED/imd_1x1v2_1951-2004.amjjaso.nc'

box=[0,60,30,120]
box_read=[0,59,31,121]
n_sets=10

n_lags=91
lags=indgen(n_lags)-n_lags/2
mylevs=['1','3','5','7','9','11','13','15','17']

heaviside=2.0

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
       0: BEGIN
          infile=trmm_n216_infile
          varname='precip'
          plot_title='TRMM rainfall (N216, 1999-2008, 10 years)'
          psfile_title='trmm_n216'
          start_read=150
          n_days=120
          n_years=10
          fived=0
          multiplier=1.
          sig_level=0.201
          year_last=1
       END
       1: BEGIN
          infile=trmm_n96_infile
          varname='precip'
          plot_title='TRMM rainfall (N96, 1999-2009, 11 years)'
          psfile_title='trmm_n96'
          start_read=150
          n_days=120
          n_years=10
          fived=0
          multiplier=1.
          sig_level=0.201
          year_last=1
       END
       ;2: BEGIN
       ;   infile=imd_infile
       ;   varname='rf'
       ;   plot_title='IMD gridded rainfall (1951-2004, 54 years)'
       ;   psfile_title='imd'
       ;   start_read=61
       ;   n_days=90
       ;   n_years=54
       ;   fived=1
       ;   multiplier=1.
       ;   sig_level=0.189
       ;END
       2: BEGIN
          infile=airxv_infile
          varname='precip'
          plot_title='HadGEM3-A MORPH3 final assessment (1982-2008)'
          psfile_title='airxv'
          start_read=150
          n_days=120
          n_years=27
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
;       3: BEGIN
;          infile=xemjo_infile
;          varname='precip'
;          plot_title='HadGEM3-HA MORPH3 final assessment (1982-2008)'
;          psfile_title='xemjo'
;          start_read=150
;          n_days=120
;          n_years=27
;          fived=0
;          multiplier=86400.
;          sig_level=0.189
;       END
       3: BEGIN
          infile=aisas_infile
          varname='precip'
          plot_title='HadGEM3-HAO MORPH3 final assessment (1982-2008)'
          psfile_title='aisas'
          start_read=150
          n_days=120
          n_years=22
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
       4: BEGIN
          infile=aicvx_infile
          varname='precip'
          plot_title='HadGEM3-AO MORPH3 final assessment (1982-2008)'
          psfile_title='aicvx'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
       5: BEGIN
          infile=ageyb_infile
          varname='precip'
          plot_title='HadGEM2-A MORPH3 final assessment (1979-1998)'
          psfile_title='ageyb'
          start_read=150
          n_days=120
          n_years=20
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
       6: BEGIN
          infile=agkfc_infile
          varname='precip'
          plot_title='HadGEM2-AO MORPH3 final assessment (30 years)'
          psfile_title='agkfc'
          start_read=150
          n_days=120
          n_years=20
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
       7: BEGIN
          infile=akkvg_infile
          varname='precip'
          plot_title='HadGEM3 GA3.0 AMIP2 simulation (1982-1997)'
          psfile_title='akkvg'
          start_read=150
          n_days=120
          n_years=16
          fived=1
          multiplier=86400.
          sig_level=0.189
       END
       8: BEGIN
          infile=hadgem3a_amip2_ctl_infile
          varname='precip'
          plot_title='Nick HadGEM3-A (vn7.4) 1.0x entrain (30 years)'
          psfile_title='hadgem3a_amip2_ctl_vn74'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
       9: BEGIN
          infile=hadgem3a_amip2_15xentrain_infile
          varname='precip'
          plot_title='Nick HadGEM3-A (vn7.4) 1.5x entrain (30 years)'
          psfile_title='hadgem3a_amip2_1.5xentrain_vn74'
          start_read=150
          n_days=120
          n_years=30
          fived=0
          multiplier=86400.
          sig_level=0.189
       END
    ENDCASE
    
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box_read,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
;    allyears_precip=fltarr(n_lon,n_lat,n_days*n_years)
    total_heaviside=fltarr(n_lon,n_lat)
    FOR j=0,n_years-1 DO BEGIN
       print,j
        IF fived eq 1 THEN BEGIN
            thisyear_precip=$
              REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                      offset=[box_tx(1),box_tx(0),0,start_read,j],$
                                      count=[n_lon,n_lat,1,n_days,1]))*multiplier
        ENDIF ELSE BEGIN
           IF year_last eq 1 THEN BEGIN
              thisyear_precip=$
                 REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                         offset=[box_tx(1),box_tx(0),start_read,j],$
                                         count=[n_lon,n_lat,n_days,1]))*multiplier
           ENDIF ELSE BEGIN
              thisyear_precip=$
                 REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                         offset=[box_tx(1),box_tx(0),j,start_read],$
                                         count=[n_lon,n_lat,1,n_days]))*multiplier
           ENDELSE
        ENDELSE
           IF j eq 0 THEN $
          acorr_mean=fltarr(n_lon,n_lat,n_lags)        
        
        thisyear_precip[where(thisyear_precip le heaviside)]=0.
        thisyear_precip[where(thisyear_precip gt heaviside)]=1.

        FOR k=0,n_lon-1 DO BEGIN
            FOR m=0,n_lat-1 DO BEGIN
                FOR n=1,n_days-1 DO BEGIN
                   IF thisyear_precip(k,m,n) eq 2E20 THEN $
                      thisyear_precip(k,m,n)=thisyear_precip(k,m,n-1)                    
                ENDFOR
                acorr_mean(k,m,*)=acorr_mean(k,m,*)+$
                                  A_CORRELATE(REFORM(thisyear_precip(k,m,*)),lags)/FLOAT(n_years)
                total_heaviside(k,m)=total_heaviside(k,m)+TOTAL(thisyear_precip(k,m,*))
             ENDFOR
         ENDFOR
     ENDFOR
    
    always_rain=fltarr(n_lon,n_lat)
    always_dry=fltarr(n_lon,n_lat)
    FOR k=0,n_lon-1 DO BEGIN
       FOR m=0,n_lat-1 DO BEGIN
          IF total_heaviside(k,m) ge n_days*n_years*0.95 THEN $
             always_rain(k,m)=1
          IF total_heaviside(k,m) le n_days*n_years*0.05 THEN $
             always_dry(k,m)=1
       ENDFOR
    ENDFOR
    
    decorrelation_time=fltarr(n_lon,n_lat)    
    FOR k=0,n_lon-1 DO BEGIN
        FOR m=0,n_lat-1 DO BEGIN
            count=0
            j=n_lags/2+1
            WHILE count lt 2 DO BEGIN
                IF acorr_mean(k,m,j) lt sig_level THEN BEGIN
                    count=count+1
                    j=j-1
                ENDIF ELSE $
                  j=j-1
                IF j eq 0 THEN BEGIN
                    count=2         
                    j=-2
                ENDIF
            ENDWHILE
            decorrelation_time(k,m)=lags(j+2)
            IF decorrelation_time(k,m) eq lags(0) THEN $
               decorrelation_time(k,m)=!Values.F_NaN  
         ENDFOR        
    ENDFOR    

    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_autocorr_spatial.'+psfile_title+'.heaviside_'+$
      STRMID(STRTRIM(STRING(heaviside),1),0,3)+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=-decorrelation_time,X=longitude,Y=latitude,/BLOCK,/NOLINES,$
        TITLE='De-correlation time for JJAS rain, Heaviside at 2 mm/day for '+plot_title
    FOR k=0,n_lon-1 DO BEGIN
       FOR m=0,n_lat-1 DO BEGIN
          IF longitude(k) ge 60 and latitude(m) le 30 THEN BEGIN
             IF always_rain(k,m) eq 1 THEN $
                GPLOT,X=longitude(k),Y=latitude(m),SYM=8
             IF always_dry(k,m) eq 1 THEN $
                GPLOT,X=longitude(k),Y=latitude(m),SYM=6
          ENDIF
       ENDFOR
    ENDFOR
    PSCLOSE,/NOVIEW
                
ENDFOR

STOP
END


