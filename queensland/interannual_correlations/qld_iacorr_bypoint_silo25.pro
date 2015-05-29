PRO qld_iacorr_bypoint_silo25

; Correlate the annual-mean rainfall in each gridpoint with all others

ameans_infile='/home/ss901165/datasets_mango/SILO/two_and_a_half/SILO.may-apr_ameans.1900-2008.precip.2.5x2.5.nc'

box=[-30,138,-10,154]
silo_latitude=OPEN_AND_EXTRACT(ameans_infile,'latitude')
silo_longitude=OPEN_AND_EXTRACT(ameans_infile,'longitude')
DEFINE_BOUNDARIES,box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)
silo_nyears=109

; Read annual means on grid
precip_ameans=REFORM(OPEN_AND_EXTRACT(ameans_infile,'rain',$
                                      offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                      count=[silo_nlon,silo_nlat,silo_nyears]))

mylevs=['0','0.05','0.1','0.15','0.2','0.25','0.3','0.35','0.4','0.45','0.5','0.55','0.6','0.65','0.7','0.75','0.8','0.85','0.9','0.95','1.00']

FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF precip_ameans(i,j,0) lt 10000 THEN BEGIN
         this_pt=REFORM(precip_ameans(i,j,*))
         precip_ameans_correlations=fltarr(silo_nlon,silo_nlat)
         FOR k=0,silo_nlon-1 DO BEGIN
            FOR m=0,silo_nlat-1 DO BEGIN
               IF precip_ameans(k,m,0) lt 10000 THEN BEGIN
                  precip_ameans_correlations(k,m)=CORRELATE(this_pt,REFORM(precip_ameans(k,m,*)))
               ENDIF ELSE $
                  precip_ameans_correlations(k,m)=!Values.F_NaN
            ENDFOR
         ENDFOR
         print,precip_ameans_correlations
         psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silo25.pt'+STRTRIM(STRING(i*silo_nlat+j),1)+'.may-apr.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,/PORTRAIT,CB_WIDTH=112
         CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
         MAP,LONMIN=138.75,LONMAX=153.75,LATMIN=box(0),LATMAX=box(2)
         LEVS,MANUAL=mylevs
         CON,FIELD=precip_ameans_correlations,X=silo_longitude,Y=silo_latitude,$
             TITLE='Corr against point '+STRTRIM(STRING(silo_latitude(j)),1)+'S '+STRTRIM(STRING(silo_longitude(i)),1)+'E - SILO 2.5x2.5 (1900-2008)',$
             /NOLINES,/BLOCK
         PSCLOSE,/NOVIEW
      ENDIF
   ENDFOR
ENDFOR

zeros=fltarr(silo_nlon,silo_nlat)
zeros(*,*)=!Values.F_NaN
psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silo25.blank_may-apr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,/PORTRAIT,CB_WIDTH=112
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=138.75,LONMAX=153.75,LATMIN=box(0),LATMAX=box(2)
LEVS,MANUAL=mylevs
CON,FIELD=precip_ameans_correlations,X=silo_longitude,Y=silo_latitude,/BLOCK,/NOLINES
;LEVS,MANUAL=mylevs
;CON,FIELD=precip_ameans_correlations,X=silo_longitude,Y=silo_latitude,$
;    TITLE='Corr against point '+STRTRIM(STRING(silo_latitude(j)),1)+'S '+STRTRIM(STRING(silo_longitude(i)),1)+'E - SILO 2.5x2.5 (1900-2008)',$
;    /NOLINES,/BLOCK
PSCLOSE

STOP
END

