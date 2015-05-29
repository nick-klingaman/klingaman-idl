PRO hadgem3kpp_mean_state_coupled_ssts_compare_clims

diff_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/ga30_coupled_TandS/hadgem3ao_ajtzr_ga30-minus-meto_ocean_analysis.jan-dec_dmean-amip_clim.sst.nc'

n_days=360
start_days=[0,60,150,240,330]
stop_days=[359,149,239,329,419]
period_names=['annual','mam','jja','son','djf']
n_periods=N_ELEMENTS(start_days)

longitude=OPEN_AND_EXTRACT(diff_infile,'longitude')
latitude=OPEN_AND_EXTRACT(diff_infile,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

diff_ssts=fltarr(n_lon,n_lat,n_days*2)
diff_ssts(*,*,0:n_days-1)=OPEN_AND_EXTRACT(diff_infile,'sst')
diff_ssts(*,*,n_days:2*n_days-1)=OPEN_AND_EXTRACT(diff_infile,'sst')

diff_periods=fltarr(n_periods,n_lon,n_lat)
FOR i=0,n_periods-1 DO $
   FOR j=0,n_lon-1 DO $
      FOR k=0,n_lat-1 DO $
         diff_periods(i,j,k)=MEAN(diff_ssts(j,k,start_days(i):stop_days(i)))

mylevs=['-6.0','-5.2','-4.4','-3.6','-2.8','-2.0','-1.2','-0.4','0.4','1.2','2.0','2.8','3.6','4.4','5.2','6.0']
FOR i=0,n_periods-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/coupled_ssts/hadgem3kpp_mean_state_coupled_ssts_compare_clims.ajtzr_ga30-minus-ukmo_analysis.'+period_names(i)+'.ps'
   PSOPEN,file=psfile,TFONT=2,CHARSIZE=120,MARGIN=1500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   MAP
   CON,X=longitude,Y=latitude,FIELD=REFORM(diff_periods(i,*,*)),/NOLINES,$
       CB_TITLE='Difference in climatological SST',$
       TITLE='ajtzr (GA3.0 coupled assessment) minus UKMO analysis (1980-2009) for '+period_names(i)
   AXES
   PSCLOSE
ENDFOR

STOP
END

