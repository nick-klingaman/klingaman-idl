PRO mjo_indices_phase_composites_lag

rmm_file='/home/ss901165/um_output6/xihvd/rmm_indices.nc'
z500_file='/home/ss901165/um_output6/xihvd/hadgem3kpp_1.5xentrain_ga30_50N50S.jan-dec_dmeans.years1-60.z500.nc'

n_years=60
n_days=360
box=[30,0,90,360]
lag=12
lon=OPEN_AND_EXTRACT(z500_file,'longitude')
lat=OPEN_AND_EXTRACT(z500_file,'latitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
n_lon=N_ELEMENTS(lon)
n_lat=N_ELEMENTS(lat)

amp=OPEN_AND_EXTRACT(rmm_file,'amplitude')
phase=OPEN_AND_EXTRACT(rmm_file,'phase')

z500_byphase=fltarr(9,n_lon,n_lat)

z500=OPEN_AND_EXTRACT(z500_file,'ht',offset=[box_tx(1),box_tx(0),0,0],$
                      count=[n_lon,n_lat,n_days,n_years])

z500_clim=fltarr(n_lon,n_lat,n_days)
FOR i=0,n_lon-1 DO $
   FOR j=0,n_lat-1 DO $
      FOR k=0,n_days-1 DO $
         z500_clim(i,j,k)=MEAN(z500(i,j,k,*))

z500_ts=fltarr(n_lon,n_lat,n_days*n_years)
FOR i=0,n_years-1 DO $
   z500_ts(*,*,i*360:(i+1)*360-1)=z500(*,*,*,i)-z500_clim
amp_ts=fltarr(n_days*n_years)
phase_ts=fltarr(n_days*n_years)
FOR i=0,n_years-1 DO BEGIN
   amp_ts(i*360:(i+1)*360-1)=amp(i,*)
   phase_ts(i*360:(i+1)*360-1)=phase(i,*)
ENDFOR

FOR k=0xc,8 DO BEGIN
   IF k eq 0 THEN BEGIN
      times=where(amp lt 1.0 and amp gt 0)
   ENDIF ELSE $
      times=where(amp ge 1.0 and phase eq k)
   times=times[where(times le n_days*n_years-lag-1)]
   FOR i=0,n_lon-1 DO BEGIN
      FOR j=0,n_lat-1 DO BEGIN
         thispt=REFORM(z500_ts(i,j,*))
         z500_byphase(k,i,j)=MEAN(thispt(times+lag))
      ENDFOR
   ENDFOR
ENDFOR

mylevs=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']

psfile='/home/ss901165/idl/mjo_indices/mjo_indices_phase_composites_lag.z500_lag10.ps'
PSOPEN,file=psfile,TFONT=6,CHARSIZE=110,TCHARSIZE=100,FONT=6,YPLOTS=3,XPLOTS=3,YSPACING=2000,XSPACING=1000

FOR j=0,8 DO BEGIN
   POS,XPOS=(j/3)+1,YPOS=3-(j MOD 3)
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
   LEVS,MANUAL=mylevs
   MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=-180,LONMAX=180,/NH
   CON,X=lon,Y=lat,FIELD=REFORM(z500_byphase(j,*,*)),POSITIVE_STYLE=2,NEGATIVE_STYLE=1,$
       /NOCOLBAR,/NOLINELABELS,$
       TITLE='Phase '+STRTRIM(STRING(j),1)+' at lag = '+STRTRIM(STRING(lag),1)
   AXES,XSTEP=30,YSTEP=15
ENDFOR
COLBAR,COORDS=[5000,1000,25000,1500],TITLE='Anomalies in Z500 from daily climatology (m)'
PSCLOSE

STOP
END


