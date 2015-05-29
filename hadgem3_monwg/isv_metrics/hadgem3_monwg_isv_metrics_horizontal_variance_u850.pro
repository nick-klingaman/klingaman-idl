PRO hadgem3_monwg_isv_metrics_horizontal_variance_u850

low_limit=30
high_limit=50
low_limit_str=STRTRIM(STRING(low_limit),1)
high_limit_str=STRTRIM(STRING(high_limit),1)

;gpcp_infile='/home/ss901165/datasets/GPCP/one_degree/gpcp1dd.1997-2007.amjjaso_anom_filter'+low_limit_str+high_limit_str+'.monsoon_domain.n96.nc'
;trmm_infile='/home/ss901165/datasets/TRMM_3B42V6/TRMM_3B42v6A.1999-2008.apr-oct_anom_filter'+low_limit_str+high_limit_str+'.dmeans.monsoon_domain.n96.nc'
eraint_infile='/home/ss901165/datasets/ERA-INT/U850/U850.amjjaso.1989-2008.monsoon_domain.n96.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahjra_infile='/home/ss901165/um_output/hadgem3_monwg/ahjra/ahjra.u850.apr-oct.daily_11years.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahsaf_infile='/home/ss901165/um_output/hadgem3_monwg/ahsaf/ahsaf.u850.daily_20years.anom_filter'+low_limit_str+high_limit_str+'.nc'
ahhbs_infile='/home/ss901165/um_output/hadgem3_monwg/ahhbs/ahhbs.u850.apr-oct.daily_9years.anom_filter'+low_limit_str+high_limit_str+'.nc'

box_plot=[-30,45,30,175]
box_read=[-30,40,30,180]
; For 30-50 day
;mylevs=['0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0','4.5','5.0','5.5','6.0','6.5','7.0']
mylevs=['0.20','0.40','0.60','0.80','1.00','1.20','1.40','1.60','1.80','2.00','2.20','2.40','2.60','2.80','3.00']
; For 10-20 day
;mylevs=['0.35','0.70','1.05','1.40','1.75','2.10','2.45','2.80','3.15','3.50','3.85','4.20','4.55']
; For 2-120 day
;mylevs=['3.0','4.5','6.0','7.5','9.0','10.5','12.0','13.5','15.0','16.5','18.0','19.5','21.0']
n_sets=4

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF 
        0: BEGIN
            infile=eraint_infile
            plot_title='ERA-INT (1989-2008, N96)'
            psfile_title='eraint_n96'
            filter_start_read=91-high_limit/2
            varname='U'
            n_days=62
            n_years=20
        END
        1: BEGIN
            infile=ahjra_infile
            plot_title='HadGEM3-A (ahjra, rh-based, 11 years)'
            psfile_title='ahjra'
            filter_start_read=90-high_limit/2
            varname='u'
            n_days=60
            n_years=11
        END
        2: BEGIN
            infile=ahhbs_infile
            plot_title='HadGEM3-AO (ahhbs, rh-based, 9 years)'
            psfile_title='ahhbs'
            filter_start_read=90-high_limit/2
            varname='u'
            n_days=60
            n_years=9
        END
        3: BEGIN
            infile=ahsaf_infile
            plot_title='HadGEM3-AO (ahsaf, w-based, 20 years)'
            psfile_title='ahsaf'
            filter_start_read=180-high_limit/2
            varname='u'
            n_days=60
            n_years=20            
        END
    ENDCASE
    
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box_read,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
                                ; Read filtered rainfall for each year
                                ; and take the longitude average
    allyears_precip_filtered=fltarr(n_lon,n_lat,n_days*n_years)    
    FOR j=0,n_years-1 DO BEGIN
        allyears_precip_filtered(*,*,j*n_days:(j+1)*n_days-1)=$
          REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                  offset=[box_tx(1),box_tx(0),j,filter_start_read],$
                                  count=[n_lon,n_lat,1,n_days]))        
    ENDFOR
    
    IF TOTAL(where(allyears_precip_filtered ge 1E20)) gt 0 THEN $
       allyears_precip_filtered[where(allyears_precip_filtered ge 1E20)]=!Values.F_NaN
    precip_filtered_variance=fltarr(n_lon,n_lat)
    FOR j=0,n_lon-1 DO $
      FOR k=0,n_lat-1 DO $
      precip_filtered_variance(j,k)=VARIANCE(allyears_precip_filtered(j,k,*),/NaN)

    psfile='/home/ss901165/idl/hadgem3_monwg/isv_metrics/hadgem3_monwg_isv_metrics_horizontal_variance_u850.'+psfile_title+'_'+low_limit_str+high_limit_str+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,IDL=3,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=SQRT(precip_filtered_variance),X=longitude,Y=latitude,$
      TITLE='Std. dev. in '+low_limit_str+'-'+high_limit_str+' day filtered U850 (m/s) - '+plot_title,$
      /NOLINES
    PSCLOSE,/NOVIEW

ENDFOR

STOP

END
