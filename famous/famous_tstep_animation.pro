PRO famous_tstep_animation

infile='/home/ss901165/um_output6/xhhbf/apy.nc'

varnames=['temp','iceconc','precip']
vardescs=['Surface Temperature (K)','Ice fraction (unitless)','Precipitation (mm day!U-1!N)']

FOR j=0,N_ELEMENTS(varnames)-1 DO BEGIN
   CASE varnames(j) OF
      'temp' : BEGINprecip
	 levels=['220','225','230','235','240','245','250','255','260','265','270','275','280','285','290','295','300','305','310']
         revcs=0
         white=[99]
         scale=1
         mult=1.
      END
      'iceconc' : BEGIN
         levels=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9']
         scale=25
         white=[2]
         revcs=0
         mult=1.
      END
      'precip' : BEGIN
         levels=['1','3','5','7','9','11','13','15','17','19','21','23','25','27']
         scale=25
         white=[2]
         revcs=0
         mult=86400.
      END
   ENDCASE

   lon=OPEN_AND_EXTRACT(infile,'longitude')
   lat=OPEN_AND_EXTRACT(infile,'latitude')
   data=REFORM(OPEN_AND_EXTRACT(infile,varnames(j)))*mult
   ntime=N_ELEMENTS(data(0,0,*))

FOR i=0,ntime-1 DO BEGIN
   yearstr=STRTRIM(STRING(i+1),1)
   IF i lt 99 THEN $
      yearstr='0'+yearstr
   IF i lt 9 THEN $
      yearstr='0'+yearstr
   psfile='/home/ss901165/idl/famous/xhhbf.'+varnames(j)+'.step_'+yearstr+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2000,SPACE2=600
   MAP,LONMIN=0,LONMAX=360,LATMIN=-90,LATMAX=90
   IF revcs eq 1 THEN BEGIN
      CS,SCALE=scale,/REV,NCOLS=N_ELEMENTS(levels)+1,white=white 
   ENDIF ELSE $
      CS,SCALE=scale,NCOLS=N_ELEMENTS(levels)+1,white=white
   LEVS,MANUAL=levels
   CON,X=lon,Y=lat,FIELD=REFORM(data(*,*,i)),/BLOCK,/NOLINES,$
       TITLE='Annual-mean '+vardescs(j)+' for year '+yearstr+' - Eyeball Earth (xhhbf) C-cyc, 1-yr day',CB_TITLE=vardescs(j)
   AXES
   PSCLOSE,/NOVIEW
ENDFOR
ENDFOR

STOP
END

