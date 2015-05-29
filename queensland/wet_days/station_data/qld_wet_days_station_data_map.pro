PRO qld_wet_days_station_data_map

; Make a map of selected rain gauge stations in Queensland
input_file='/home/ss901165/datasets/AUS_RAIN_GAUGE/data/QLD2008/qld_hq_rain_gauges.jan-dec_dmeans.1975-2007.nc'
data_type='1975-2007_98pct'

; Read station latitude and longitude
station_latitude=OPEN_AND_EXTRACT(input_file,'station_lat')
station_longitude=OPEN_AND_EXTRACT(input_file,'station_lon')

; Make map
psfile='/home/ss901165/idl/queensland/wet_days/station_data/qld_wet_days_station_data_map.'+data_type+'.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
MAP,LONMIN=137,LONMAX=154,LATMIN=-30,LATMAX=-9,/HIRES,land=FSC_COLOR("white"),ocean=FSC_COLOR("skyblue"),/SET
GPLOT,X=station_longitude,Y=station_latitude,SYM=4,SIZE=25,/NOLINES,COL=FSC_COLOR("red")
AXES,XSTEP=3,YSTEP=2,XMINOR=1,YMINOR=1
DRAWSTATES,'/home/ss901165/idl/queensland/shapefiles/aust_cd66eaststates.shp',ATTRIBUTE_NAME='STE',COLOR="black",statenames='3',THICK=3
GPLOT,X=138,Y=-8.5,TEXT='Locations of stations with >=98% complete daily rainfall data for 1975-2007',ALIGN=0.0,CHARSIZE=100
GPLOT,X=149,Y=-16,TEXT='Number of stations = '+STRTRIM(STRING(N_ELEMENTS(station_longitude)),1)
PSCLOSE,/NOVIEW

; Test data
;n_time=N_ELEMENTS(REFORM(OPEN_AND_EXTRACT(input_file,'time')))
;rainfall=REFORM(OPEN_AND_EXTRACT(input_file,'rainfall'));,offset=[0,0],count=[n_time,1]))
;number=REFORM(OPEN_AND_EXTRACT(input_file,'station_number'));,offset=[0],count=[1]))

STOP
END

