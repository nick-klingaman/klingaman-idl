PRO qld_eots_smeans_silo025_trends

n_seasons=4
n_eots=3
n_years=108
silo_start_year=1900
silo_start_year_str=STRTRIM(STRING(silo_start_year),1)
sig_level=0.367

season_names=['dec-feb','mar-may','jun-aug','sep-nov']

FOR i=0,n_seasons-1 DO BEGIN
   infile='/home/ss901165/datasets_mango/SILO/one_quarter/SILO.'+season_names(i)+'_smeans.1900-2007.eots.nc'
   
   eots_ts=OPEN_AND_EXTRACT(infile,'loading',$
                            offset=[0,0],count=[n_years,n_eots])
   
   print,'For EOTs in '+season_names(i)
   running_trends=fltarr(n_eots,n_years-29)
   running_correlations=fltarr(n_eots,n_years-29)
   FOR j=0,n_eots-1 DO BEGIN
      temp=REGRESS(indgen(n_years),REFORM(eots_ts(*,j)),$
                   CORRELATION=correlation)      
      FOR k=29,n_years-1 DO BEGIN
         temp=REGRESS(indgen(30),REFORM(eots_ts(k-29:k,j)),$
                      CORRELATION=temp2)
         running_correlations(j,k-29)=temp2(0)
         running_trends(j,k-29)=temp(0)
      ENDFOR
      trend=temp(0)      
      print,'EOT '+STRTRIM(STRING(j),1)+': '+STRTRIM(STRING(trend),1)+' '+STRTRIM(STRING(correlation),1)
   ENDFOR
   psfile='/home/ss901165/idl/queensland/eots/qld_eots_smeans_silo025_trends.'+season_names(i)+'_30year.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1800,SPACE2=300,XOFFSET=400,YOFFSET=500,TFONT=2,TCHARSIZE=100
   black=FSC_COLOR("black",9)
   red=FSC_COLOR("red",10)
   blue=FSC_COLOR("blue",11)
   GSET,XMIN=silo_start_year+30,XMAX=silo_start_year+n_years,YMIN=-10,YMAX=10
   FOR j=0,n_eots-1 DO BEGIN
      GPLOT,X=indgen(n_years-30)+silo_start_year+30,Y=REFORM(running_trends(j,*)),COL=j+9
      FOR k=0,n_years-31 DO $
         IF ABS(running_correlations(j,k)) gt sig_level THEN $
            GPLOT,X=silo_start_year+k+30,Y=running_trends(j,k),SYM=3,SIZE=80,COL=j+9
   ENDFOR
   AXES,XSTEP=5,YSTEP=2,XMINOR=1,YMINOR=1
   PSCLOSE
   
ENDFOR

STOP
END
