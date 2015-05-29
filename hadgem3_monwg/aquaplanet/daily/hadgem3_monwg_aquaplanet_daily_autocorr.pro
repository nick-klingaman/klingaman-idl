PRO hadgem3_monwg_aquaplanet_daily_autocorr

n_runs=1
                     ;H 3H 6H D 2D 4D 8D 16D 32D
meaning_lengths=LONG([3, 3, 2,4, 2, 2, 2,  2,  2]) ; Each as a multiple of the last
n_meaning_lengths=N_ELEMENTS(meaning_lengths)

corr_levs=['-0.95','-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
           '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']

pdfs=fltarr(n_runs,n_meaning_lengths+1)
colors=strarr(n_runs)
styles=strarr(n_runs)
model_names=strarr(n_runs)
FOR i=0,n_runs-1 DO BEGIN
   CASE i OF
      2 : BEGIN
         infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_ctlent'
         colors(i)='red'
         styles(i)=0
      END
      3 : BEGIN
         infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_1.5xent'
         colors(i)='blue'
         styles(i)=0
      END
      4 : BEGIN
         infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_2.5xent'
         colors(i)='orange'
         styles(i)=0
      END
      0 : BEGIN
         infile='/home/ss901165/um_output5/xhccy/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_1.5xent_2xcape'
         colors(i)='purple'
         styles(i)=0
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xhccx/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_0.5xcape.jan-dec_dmeans.years1-2.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_0.5xcape'
         colors(i)='violetred'
         styles(i)=0
      END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
;   DEFINE_BOUNDARIES,[-10,60,10,80],latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)

   autocorr=fltarr(n_meaning_lengths+1,n_lon,n_lat)
   
   print,'Reading timestep precipitation data ...'
   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip') ; Convert to mm/day
;   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip',offset=[box_tx(1),box_tx(0),0],count=[n_lon,n_lat,LONG(72*1080)])*72.
   
   our_timestep_precip=timestep_precip
   our_ntime=N_ELEMENTS(our_timestep_precip(0,0,*))
   FOR j=0,n_meaning_lengths DO BEGIN
      print,' ---> '+STRTRIM(STRING(j),1)+' of '+STRTRIM(STRING(n_meaning_lengths),1)

      FOR k=0,n_lon-1 DO $
         FOR m=0,n_lat-1 DO $
            autocorr(j,k,m)=A_CORRELATE(REFORM(our_timestep_precip(k,m,*)),1)      
      
      psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_autocorr.'+model_names(i)+$
             '.meaning_length'+STRTRIM(STRING(j),1)+'.ps'
      PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=120,YSIZE=9000,SPACE2=2000,YOFFSET=1500;,SPACE1=1000
      total_mean=1
      FOR k=0,j-1 DO $
         total_mean=total_mean*meaning_lengths(k)
      
      GSET,XMIN=0,XMAX=360,YMIN=MIN(latitude),YMAX=MAX(latitude)
      CS,SCALE=1,NCOLS=N_ELEMENTS(corr_levs)+1
      LEVS,MANUAL=corr_levs
      
      CON,X=longitude,Y=latitude,FIELD=REFORM(autocorr(j,*,*)),$
          TITLE='Lag-1 autocorrelation of precipitation from timestep data over 15S-15N (meaning '+$
          STRTRIM(STRING(total_mean),1)+' timesteps)',$
          CB_TITLE='Correlation coefficient',/NOLINES,/BLOCK

      AXES,XSTEP=30,YSTEP=3,XTITLE='Longitude (degrees east)',YTITLE='Latitude (degrees north)'
      PSCLOSE,/NOVIEW
      
      ; Do meaning for next pass
      IF j ne n_meaning_lengths THEN BEGIN
         temp=our_timestep_precip
         our_ntime=our_ntime/meaning_lengths(j)
         our_timestep_precip=fltarr(n_lon,n_lat,our_ntime)
         print,'Meaning into new timeseries of '+STRTRIM(STRING(our_ntime),1)+' points ...'
         FOR k=0,n_lon-1 DO BEGIN
            print,'-----> Longitude '+STRTRIM(STRING(k+1),1)+' ...'
            FOR m=0,n_lat-1 DO $
               FOR n=LONG(0),LONG(our_ntime)-1 DO $
                  our_timestep_precip(k,m,n)=MEAN(temp(k,m,n*meaning_lengths(j):(n+1)*meaning_lengths(j)-1))      
         ENDFOR
      ENDIF
      
   ENDFOR
ENDFOR

STOP
END
