PRO aus_rainfall_percentiles_byregion

; Plot a climatological percentile of daily rainfall for Queensland.
  
; Input file containing rainfall percentiles (note: file is already restricted to a box approximating Queensland)
silo_t62_input_file='/home/ss901165/datasets_mango/SILO/t62/SILO.may-apr_dmeans.1958-2000.precip_percentiles.t62.nc'
silo_era40_input_file='/home/ss901165/datasets_mango/SILO/era40_resolution/SILO.may-apr_dmeans.1958-2000.precip_percentiles.era40.nc'
twentyc_input_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.nc'
ncep_input_file='/home/ss901165/datasets_mango/NCEP_REANALYSIS/precipitation/ncep-ncar_reanalysis.may-apr_dmeans.1958-2000.precip_percentiles.t62_gauss.nc'
era40_input_file='/home/ss901165/datasets/ERA40/PRECIP/era40.may-apr_dmeans.1958-2000.precip_percentiles.aus_domain.nc

box=[-25,140,-10,153]
region_name='northeast'

n_datasets=5
dataset_names=['SILO on T62 grid','SILO on ERA-40 grid','20th Century Reanalysis','NCEP-NCAR Reanalysis','ERA-40 Reanalysis']

; Set the filesystem path to the PostScript file
psfile='/home/sf205042/idl/aus_rainfall_percentiles_byregion.'+region_name+'.ps'
PSOPEN,file=psfile,FONT=6,CHARSIZE=100,MARGIN=1500,SPACE2=200,XOFFSET=1000,YOFFSET=1000,TFONT=6,$
       TCHARSIZE=80,SPACE3=200
GSET,XMIN=0,XMAX=100,YMIN=1,YMAX=80,TITLE='Amount of rainfall at climatological percentiles - '+region_name+' region - 1958-2000',/YLOG

all_colors=strarr(n_datasets)
all_styles=intarr(n_datasets)
FOR i=0,n_datasets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         input_file=silo_t62_input_file
         color='red'
         style=0
      END
      1 : BEGIN
         input_file=silo_era40_input_file         
         color='red'
         style=2
      END
      2 : BEGIN
         input_file=twentyc_input_file
         color='blue'
         style=0
      END
      3 : BEGIN
         input_file=ncep_input_file
         color='purple'
         style=0
      END
      4 : BEGIN
         input_file=era40_input_file
         color='brown'
         style=0
      END
   ENDCASE
   all_colors(i)=color
   all_styles(i)=style
   
                                ; Read latitude and longitude from the file
   longitude=OPEN_AND_EXTRACT(input_file,'longitude')
   latitude=OPEN_AND_EXTRACT(input_file,'latitude')
   DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
                                ; Get the number of latitude and
                                ; longitude points in the box
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
                                ; Read the percentiles available in the file
   percentiles=OPEN_AND_EXTRACT(input_file,'percentile')
   n_percentiles=N_ELEMENTS(percentiles)

                                ; Read the climatological percentile of daily rainfall
   clim_percentile=REFORM(OPEN_AND_EXTRACT(input_file,'amount_at_percentile',$
                                           offset=[box_tx(1),box_tx(0),0],$
                                           count=[n_lon,n_lat,n_percentiles]))
   clim_percentile[where(clim_percentile eq 2e20)]=!Values.F_NaN

                                ; Create a new vector to hold the area-average amount of rainfall at
                                ; each percentile
   area_average_clim_percentile=fltarr(n_percentiles)
   FOR j=0,n_percentiles-1 DO BEGIN
                                ; For each percentile, compute the
                                ; area average.
      area_average_clim_percentile(j)=MEAN(clim_percentile(*,*,j),/NaN)
   ENDFOR

   GPLOT,X=percentiles,Y=area_average_clim_percentile,COL=FSC_COLOR(color),STYLE=style
   AXES,XSTEP=5,YVALS=['1','2','3','4','7','10','13','16','20','25','30','35','40','45','50','55','60','70','80'],$
        XTITLE='Climatological percentile of daily rainfall',YTITLE='Amount of rainfall at percentile (mm)'
ENDFOR

GLEGEND,labels=REVERSE(dataset_names),COL=REVERSE(FSC_COLOR(all_colors)),STYLE=REVERSE(all_styles),LEGPOS=1

PSCLOSE

STOP
END



