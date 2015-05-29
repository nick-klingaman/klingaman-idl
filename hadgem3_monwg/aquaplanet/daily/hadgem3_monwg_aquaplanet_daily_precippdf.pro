PRO hadgem3_monwg_aquaplanet_daily_precippdf

precip_bins=[0.1,0.2,0.4,0.6,1.0,1.5,2.0,3.0,5.0,8.0,12.0,16.0,20,25,30,40,50,70]
n_bins=N_ELEMENTS(precip_bins)
n_runs=6
                     ;H 3H 6H D 2D 4D 8D 16D 32D
meaning_lengths=LONG([3, 3, 2,4]);, 2, 2, 2,  2,  2]) ; Each as a multiple of the last
;meaning_lengths=LONG([576])
n_meaning_lengths=N_ELEMENTS(meaning_lengths)

pdfs=fltarr(n_runs,n_meaning_lengths+2,n_bins+1)
colors=strarr(n_runs)
styles=strarr(n_runs)
model_names=strarr(n_runs)
FOR i=0,n_runs-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
         ;model_names(i)='fixnhsol5N_sin2sin4_ctlent'
         model_names(i)='GA3.0 control'
         colors(i)='black'
         styles(i)=0         
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='fixnhsol5N_sin2sin4_1.5xent'
         model_names(i)='GA3.0 1.5x ent & det'
         colors(i)='red'
         styles(i)=0
      END
      2 : BEGIN
         infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='GA3.0 2.5x ent & det'
         colors(i)='orange'
         styles(i)=0
      END
      3 : BEGIN
         infile='/home/ss901165/um_output5/xhccw/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='GA3.0 2x CAPE dt'
         colors(i)='blue'
         styles(i)=0
      END
      4 : BEGIN
         infile='/home/ss901165/um_output5/xhccx/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_0.5xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='GA3.0 0.5x CAPE dt'
         colors(i)='dodgerblue'
         styles(i)=0
      END
      5 : BEGIN
         infile='/home/ss901165/um_output5/xhccy/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='GA3.0 1.5x ent and 2x CAPE dt'
         colors(i)='purple'
         styles(i)=0
      END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   DEFINE_BOUNDARIES,[-10,0,10,360],latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
   print,'Reading timestep precipitation data ...'
;   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip')*72. ; Convert to mm/day
   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip',offset=[box_tx(1),box_tx(0),0],count=[n_lon,n_lat,LONG(72)*1080])*72.
   
   our_timestep_precip=timestep_precip
   our_ntime=N_ELEMENTS(our_timestep_precip(0,0,*))
   FOR j=0,n_meaning_lengths DO BEGIN
      print,' ---> '+STRTRIM(STRING(j),1)+' of '+STRTRIM(STRING(n_meaning_lengths),1)
      print,'Binning rainfall ...'
      FOR k=0,n_bins-2 DO BEGIN
         IF TOTAL(where(our_timestep_precip ge precip_bins(k) and our_timestep_precip lt precip_bins(k+1))) ge 0 THEN $
            pdfs(i,j,k+1)=N_ELEMENTS(where(our_timestep_precip ge precip_bins(k) and $
                                           our_timestep_precip lt precip_bins(k+1)))
      ENDFOR
      IF TOTAL(where(our_timestep_precip lt precip_bins(0))) ge 0 THEN $
         pdfs(i,j,0)=N_ELEMENTS(where(our_timestep_precip lt precip_bins(0)))
      IF TOTAL(where(our_timestep_precip ge precip_bins(n_bins-1))) ge 0 THEN $
         pdfs(i,j,n_bins)=N_ELEMENTS(where(our_timestep_precip gt precip_bins(n_bins-1)))

      pdfs(i,j,*)=pdfs(i,j,*)/FLOAT(our_ntime*n_lon*n_lat)
      
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
   print,'Mean precip = ',MEAN(our_timestep_precip)
   
   temp=our_timestep_precip
   our_timestep_precip=fltarr(n_lon,n_lat)
   FOR k=0,n_lon-1 DO $
      FOR m=0,n_lat-1 DO $
         our_timestep_precip(k,m)=MEAN(temp(k,m,*))
   
   FOR k=0,n_bins-2 DO BEGIN
      IF TOTAL(where(our_timestep_precip ge precip_bins(k) and our_timestep_precip lt precip_bins(k+1))) ge 0 THEN $
         pdfs(i,n_meaning_lengths+1,k+1)=N_ELEMENTS(where(our_timestep_precip ge precip_bins(k) and $
                                           our_timestep_precip lt precip_bins(k+1)))
   ENDFOR
   IF TOTAL(where(our_timestep_precip lt precip_bins(0))) ge 0 THEN $
      pdfs(i,n_meaning_lengths+1,0)=N_ELEMENTS(where(our_timestep_precip lt precip_bins(0)))
   IF TOTAL(where(our_timestep_precip ge precip_bins(n_bins-1))) ge 0 THEN $
      pdfs(i,n_meaning_lengths+1,n_bins)=N_ELEMENTS(where(our_timestep_precip gt precip_bins(n_bins-1)))
   
   pdfs(i,n_meaning_lengths+1,*)=pdfs(i,n_meaning_lengths+1,*)/FLOAT(n_lon*n_lat)
   
ENDFOR

IF TOTAL(where(pdfs lt 0.002)) ge 0 THEN pdfs[where(pdfs lt 0.002)]=!Values.F_NaN

FOR j=0,n_meaning_lengths DO BEGIN
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_precippdf.meaning_length'+STRTRIM(STRING(j),1)+'_new.ps'
;psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_precippdf.isv_lengths'+STRTRIM(STRING(j),1)+'_new.ps'
   PSOPEN,file=psfile,TFONT=6,FONT=6,CHARSIZE=150,SPACE2=200,SPACE3=200,XOFFSET=1000
   total_mean=1
   FOR k=0,j-1 DO $
      total_mean=total_mean*meaning_lengths(k)
   GSET,XMIN=0,XMAX=n_bins+1,YMIN=0.002,YMAX=0.5,/YLOG,TITLE='Precipitation rates over 15S-15N (meaning '+STRTRIM(STRING(total_mean),1)+' timesteps)'
   FOR k=0,n_runs-1 DO $
      GPLOT,X=indgen(n_bins+1)+0.5,Y=REFORM(pdfs(k,j,*)),COL=FSC_COLOR(colors(k)),STYLE=styles(k),SYM=3,SIZE=75
   AXES,XVALS=indgen(n_bins+2),XLABELS=['<',STRMID(STRTRIM(STRING(precip_bins),1),0,3),'>'],$
        YVALS=['0.002','0.003','0.004','0.006','0.008','0.01',$
               '0.015','0.02','0.03','0.04','0.06','0.08','0.1','0.15','0.2','0.3','0.4','0.5'],YTITLE='Probability',XTITLE='Precipitation rate (mm day!U-1!N)'
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(colors)),STYLE=REVERSE(styles),LEGPOS=3
;   GLEGEND,labels=REVERSE(['1 timestep','576 timesteps (8 day)']),COL=[FSC_COLOR('black'),FSC_COLOR('black')],STYLE=REVERSE(styles),LEGPOS=5,SYM=REVERSE(syms)
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END
