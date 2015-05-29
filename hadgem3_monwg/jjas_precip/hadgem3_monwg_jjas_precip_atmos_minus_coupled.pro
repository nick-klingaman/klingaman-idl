PRO hadgem3_monwg_jjas_precip_atmos_minus_coupled

hadgem3a_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahrqc/ahrqc.precip.apr-oct.mmeans_20years.nc'
hadgem3ao_clim_mmean_file='/home/ss901165/um_output2/hadgem3_monwg/ahsaf/ahsaf.precip.clim.mmeans_30years.nc'

hadgem3a_offset_jun=2
hadgem3a_offset_sep=5
hadgem3a_ntime=hadgem3a_offset_sep-hadgem3a_offset_jun+1
hadgem3a_nyears=20

hadgem3ao_offset_jun=5
hadgem3ao_offset_sep=8
hadgem3ao_ntime=hadgem3ao_offset_sep-hadgem3ao_offset_jun+1
hadgem3ao_nyears=30

box=[-30,40,30,180]

; Get grid information
hadgem3a_longitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'longitude')
hadgem3a_latitude=OPEN_AND_EXTRACT(hadgem3a_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3a_latitude,hadgem3a_longitude,hadgem3a_box_tx,/LIMIT
hadgem3a_nlon=N_ELEMENTS(hadgem3a_longitude)
hadgem3a_nlat=N_ELEMENTS(hadgem3a_latitude)

hadgem3ao_longitude=OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'longitude')
hadgem3ao_latitude=OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'latitude')
DEFINE_BOUNDARIES,box,hadgem3ao_latitude,hadgem3ao_longitude,hadgem3ao_box_tx,/LIMIT
hadgem3ao_nlon=N_ELEMENTS(hadgem3ao_longitude)
hadgem3ao_nlat=N_ELEMENTS(hadgem3ao_latitude)

; Get monthly mean rainfall for all years
hadgem3a_precip=REFORM(OPEN_AND_EXTRACT(hadgem3a_mmean_file,'precip',$
                                        offset=[hadgem3a_box_tx(1),hadgem3a_box_tx(0),0,hadgem3a_offset_jun,0],$
                                        count=[hadgem3a_nlon,hadgem3a_nlat,1,hadgem3a_ntime,hadgem3a_nyears]))*86400.
hadgem3ao_precip=REFORM(OPEN_AND_EXTRACT(hadgem3ao_clim_mmean_file,'precip',$
                                         offset=[hadgem3ao_box_tx(1),hadgem3ao_box_tx(0),0,hadgem3ao_offset_jun],$
                                         count=[hadgem3ao_nlon,hadgem3ao_nlat,1,hadgem3ao_ntime]))*86400.

; Average over all months
hadgem3a_precip_timemean=fltarr(hadgem3a_nlon,hadgem3a_nlat,hadgem3a_nyears)
FOR i=0,hadgem3a_nlon-1 DO $
  FOR j=0,hadgem3a_nlat-1 DO $
  FOR k=0,hadgem3a_nyears-1 DO $
  hadgem3a_precip_timemean(i,j,k)=MEAN(hadgem3a_precip(i,j,*,k))
hadgem3ao_precip_timemean=fltarr(hadgem3ao_nlon,hadgem3ao_nlat)
FOR i=0,hadgem3ao_nlon-1 DO $
  FOR j=0,hadgem3ao_nlat-1 DO $
  hadgem3ao_precip_timemean(i,j)=MEAN(hadgem3ao_precip(i,j,*))

; Plot as mean of blocks of years (e.g., years 1-5, years 6-10, etc.)
n_periods=4
n_years_per_period=hadgem3a_nyears/n_periods
FOR i=0,n_periods-1 DO BEGIN
    start_year_str=STRTRIM(STRING(i*n_years_per_period+1),1)
    stop_year_str=STRTRIM(STRING((i+1)*n_years_per_period),1)
    hadgem3a_precip_timemean_thisperiod=fltarr(hadgem3a_nlon,hadgem3a_nlat)
    FOR j=0,hadgem3a_nlon-1 DO $
      FOR k=0,hadgem3a_nlat-1 DO $
      hadgem3a_precip_timemean_thisperiod(j,k)=MEAN(hadgem3a_precip_timemean(j,k,i*n_years_per_period:(i+1)*n_years_per_period-1))

    mylevs=['-8.0','-7.0','-6.0','-5.0','-4.0','-3.0','-2.0','-1.0','0.0','1.0','2.0','3.0','4.0','5.0','6.0','7.0','8.0']
    psfile='/home/ss901165/idl/hadgem3_monwg/jjas_precip/hadgem3_monwg_jjas_precip_atmos_minus_coupled.years'+start_year_str+'-'+stop_year_str+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=800,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=12,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    MAP,LONMIN=40,LONMAX=180,LATMIN=-30,LATMAX=30
    LEVS,MANUAL=mylevs
    CON,FIELD=hadgem3a_precip_timemean_thisperiod-hadgem3ao_precip_timemean,X=hadgem3a_longitude,Y=hadgem3a_latitude,$
      TITLE="Difference in JJAS-mean rainfall between mean of HadGEM3-A_ahrqc_n96 (years "+start_year_str+"-"+stop_year_str+") minus clim HadGEM3-AO_ahsaf_n96",$
      /NOLINES,CB_WIDTH=112,CB_NTH=2
    PSCLOSE
ENDFOR

STOP

END

