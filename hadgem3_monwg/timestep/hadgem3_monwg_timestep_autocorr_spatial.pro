PRO hadgem3_monwg_timestep_autocorr_spatial

amzgg='/home/ss901165/um_output3/hadgem3_monwg/amzgg'
anbba='/home/ss901165/um_output3/hadgem3_monwg/anbba'

box=[-18,40,18,200]
n_sets=1

mylevs=['1','2','3','5','7','10','13','17','21','25','30','35','40','50','60','80','100']

FOR i=0,n_sets-1 DO BEGIN
    CASE i OF
       0: BEGIN
          infile=amzgg+'/amzgga.jun-sep_tsmeans.1982.precip.nc'
          varname='precip'
          plot_title='amzgg (GA5.0, N96) total precip tstep'
          start_read=0
          n_time=8640
          multiplier=86400./72.
          mean_mult=72.
          heaviside=20
          heaviside_ts=heaviside/72.
          psfile_title='amzgg_precip_'+STRTRIM(STRING(heaviside),1)+'mm'
          sig_level=0.20
          n_lags=289
          sym_size=70
       END 
       1: BEGIN
          infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.n96.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512 -> N96) total precip tstep'
          psfile_title='anbba_precip_n96'
          start_read=0
          n_time=17280
          multiplier=86400./144.
          mean_mult=144.
          heaviside=1/144.
          sig_level=0.20
          n_lags=578
          sym_size=70
       END
       2: BEGIN
          infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.nc'
          varname='precip'
          plot_title='anbba (GA5.0, N512) total precip tstep'
          psfile_title='anbba_precip_25mm'
          start_read=0
          n_time=17280
          multiplier=86400./144.
          mean_mult=144.
          heaviside=25/144.
          sig_level=0.20
          n_lags=578
          sym_size=20
       END
    ENDCASE
    
    lags=indgen(n_lags)-n_lags/2
    longitude=OPEN_AND_EXTRACT(infile,'longitude')
    latitude=OPEN_AND_EXTRACT(infile,'latitude')
    DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
    n_lat=N_ELEMENTS(latitude)
    n_lon=N_ELEMENTS(longitude)
    
;    allyears_precip=fltarr(n_lon,n_lat,n_days*n_years)
    total_heaviside=fltarr(n_lon,n_lat)
    precip=REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                   offset=[box_tx(1),box_tx(0),start_read],$
                                   count=[n_lon,n_lat,n_time]))*multiplier  
    acorr_mean=fltarr(n_lon,n_lat,n_lags)
    
    mean_precip=fltarr(n_lon,n_lat)
    FOR k=0,n_lon-1 DO $
       FOR m=0,n_lat-1 DO $
          mean_precip(k,m)=MEAN(precip(k,m,*))*mean_mult

    precip[where(precip le heaviside_ts)]=0.
    precip[where(precip gt heaviside_ts)]=1.
    
    FOR k=0,n_lon-1 DO BEGIN
       FOR m=0,n_lat-1 DO BEGIN
          FOR n=1,n_time-1 DO BEGIN
             IF precip(k,m,n) eq 2E20 THEN $
                precip(k,m,n)=precip(k,m,n-1)                    
          ENDFOR
          acorr_mean(k,m,*)=A_CORRELATE(REFORM(precip(k,m,*)),lags)
          total_heaviside(k,m)=N_ELEMENTS(where(precip(k,m,*) gt heaviside_ts))
       ENDFOR
    ENDFOR
    
    always_rain=fltarr(n_lon,n_lat)
    always_dry=fltarr(n_lon,n_lat)
    FOR k=0,n_lon-1 DO BEGIN
       FOR m=0,n_lat-1 DO BEGIN
          IF total_heaviside(k,m) ge n_time*0.98 THEN $
             always_rain(k,m)=1
          IF total_heaviside(k,m) le n_time*0.02 THEN $
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

    mean_levs=['3','6','9','12','15','18','21','24']
    acorr_mean[where(acorr_mean lt 0)]=acorr_mean[where(acorr_mean lt 0)]*1.732
    FOR k=0,n_lags-1 DO BEGIN
       temp=REFORM(acorr_mean(*,*,k))
       temp[where(always_dry eq 1)]=!Values.F_NaN
       acorr_mean(*,*,k)=temp
    ENDFOR

    psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_autocorr_spatial.'+psfile_title+'.ps'
    PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
    CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
    MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
    LEVS,MANUAL=mylevs
    CON,FIELD=-decorrelation_time,X=longitude,Y=latitude,/BLOCK,/NOLINES,$
        TITLE='De-correlation time for JJA 1982 rain '+plot_title+', heaviside='+STRTRIM(STRING(heaviside),1)+' mm/day, X=<5% rain',CB_TITLE='Timesteps'
    FOR k=0,n_lon-1 DO BEGIN
       FOR m=0,n_lat-1 DO BEGIN
          IF always_rain(k,m) eq 1 THEN $
             GPLOT,X=longitude(k),Y=latitude(m),SYM=8,SIZE=sym_size
          IF always_dry(k,m) eq 1 THEN $
             GPLOT,X=longitude(k),Y=latitude(m),SYM=6,SIZE=sym_size
       ENDFOR
    ENDFOR   
    LEVS,MANUAL=mean_levs
    CON,FIELD=mean_precip,X=longitude,Y=latitude,/NOFILL,POSITIVE_STYLE=2,THICK=150
    PSCLOSE,/NOVIEW

    plot_lags=[1,2,3,4,5]
    n_plot_lags=N_ELEMENTS(plot_lags)
    corr_levs=['-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65']
    FOR j=0,n_plot_lags-1 DO BEGIN
       psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_autocorr_spatial.'+psfile_title+'_lag'+STRTRIM(STRING(plot_lags(j)),1)+'.ps'
       PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100
       CS,SCALE=1,NCOLS=N_ELEMENTS(corr_levs)+1,white=[9]
       MAP,LONMIN=box(1),LATMIN=box(0),LONMAX=box(3),LATMAX=box(2)
       LEVS,MANUAL=corr_levs
       this_lag=where(lags eq plot_lags(j))
       print,this_lag
       CON,FIELD=REFORM(acorr_mean(*,*,this_lag)),X=longitude,Y=latitude,/BLOCK,/NOLINES,$
           TITLE='Auto-corr of tstep rain at lag='+STRTRIM(STRING(plot_lags(j)),1)+' for '+plot_title+$
           ', heaviside='+STRTRIM(STRING(heaviside),1)+' mm/day, X=<5% rain',CB_TITLE='Correlation'
       FOR k=0,n_lon-1 DO BEGIN
          FOR m=0,n_lat-1 DO BEGIN
             IF always_rain(k,m) eq 1 THEN $
                GPLOT,X=longitude(k),Y=latitude(m),SYM=8,SIZE=sym_size
             IF always_dry(k,m) eq 1 THEN $
                GPLOT,X=longitude(k),Y=latitude(m),SYM=6,SIZE=sym_size
          ENDFOR
       ENDFOR   
       LEVS,MANUAL=mean_levs
       CON,FIELD=mean_precip,X=longitude,Y=latitude,/NOFILL,POSITIVE_STYLE=2,THICK=150
       PSCLOSE
    ENDFOR
ENDFOR

STOP
END


