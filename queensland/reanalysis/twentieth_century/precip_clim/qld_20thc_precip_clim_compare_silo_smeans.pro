PRO qld_20thc_precip_clim_compare_silo_smeans

; Compare the precipitation over Queensland from the 20th century
; reanalysis to that from SILO.
n_seasons=2
qld_box=[-10,138,-30,154]
FOR p=0,n_seasons-1 DO BEGIN
   CASE p OF 
      0: BEGIN         
         twentyc_smeans_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.nov-apr_smeans.1891-2007.precip.nc'
         silo_smeans_infile='/home/ss901165/datasets_mango/SILO/t62/SILO.nov-apr_smeans.1891-2007.precip.t62.nc'
         smean_period='nov-apr'
         ndays_in_season=180
         mylevs=[100,200,300,400,500,600,700,800,900,1000,1200,1400,1600,1800]
         mylevs_diff=[-550,-450,-350,-250,-150,-50,50,150,250,350,450,550]
         mylevs_ratio=['0.45','0.55','0.65','0.75','0.85','0.95','1.05','1.15','1.25','1.35','1.45','1.55']
         precip_max=1300
         precip_min=100
         precip_step=100
         nyear=117
         ratio_min=-0.2
         ratio_max=1.3
         ratio_white=8
      END
      1 : BEGIN
         twentyc_smeans_infile='/home/ss901165/datasets_mango/20THC_REANALYSIS/precip/20thc_reanalysis.may-oct_smeans.1891-2007.precip.nc'
         silo_smeans_infile='/home/ss901165/datasets_mango/SILO/t62/SILO.may-oct_smeans.1891-2008.precip.t62.nc'
         smean_period='may-oct'
         ndays_in_season=185
         mylevs=[50,100,150,200,250,300,350,400,500,600,700,800]
         mylevs_diff=[-275,-225,-175,-125,-75,-25,25,75,125,175,225,275]
         mylevs_ratio=['0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
         precip_max=400
         precip_min=0
         precip_step=50
         nyear=117
         ratio_min=-0.2
         ratio_max=1.6
         ratio_white=6
      END
   ENDCASE
   twentyc_latitude=OPEN_AND_EXTRACT(twentyc_smeans_infile,'latitude')
   twentyc_longitude=OPEN_AND_EXTRACT(twentyc_smeans_infile,'longitude')
   DEFINE_BOUNDARIES,qld_box,twentyc_latitude,twentyc_longitude,twentyc_box_tx,/LIMIT
   twentyc_nlon=N_ELEMENTS(twentyc_longitude)
   twentyc_nlat=N_ELEMENTS(twentyc_latitude)

   silo_latitude=OPEN_AND_EXTRACT(silo_smeans_infile,'latitude')
   silo_longitude=OPEN_AND_EXTRACT(silO_smeans_infile,'longitude')
   DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,silo_box_tx,/LIMIT
   silo_nlon=N_ELEMENTS(silo_longitude)
   silo_nlat=N_ELEMENTS(silo_latitude)

   twentyc_smeans=REFORM(OPEN_AND_EXTRACT(twentyc_smeans_infile,'PRATE',$
                                          offset=[twentyc_box_tx(1),twentyc_box_tx(0),0,0],$
                                          count=[twentyc_nlon,twentyc_nlat,1,nyear]))*86400.
   silo_smeans=REFORM(OPEN_AND_EXTRACT(silo_smeans_infile,'rain',$
                                       offset=[silo_box_tx(1),silo_box_tx(0),0],$
                                       count=[silo_nlon,silo_nlat,nyear]))
   mask=intarr(silo_nlon,silo_nlat)
   mask(*,*)=0
   t63_weights=fltarr(silo_nlon,silo_nlat)
   FOR i=0,silo_nlon-1 DO BEGIN
      FOR j=0,silo_nlat-1 DO BEGIN
         IF silo_smeans(i,j) lt 1E20 THEN BEGIN
            mask(i,j)=1
         ENDIF
      ENDFOR
   ENDFOR
   nvalid_pts=N_ELEMENTS(where(mask eq 1))
   FOR i=0,silo_nlon-1 DO BEGIN
      FOR j=0,silo_nlat-1 DO BEGIN
         IF mask(i,j) eq 1 THEN BEGIN
            t63_weights(i,j)=COS(3.14159*silo_latitude(j)/180.)
         ENDIF ELSE $
            t63_weights(i,j)=0.
      ENDFOR
   ENDFOR
   t63_weights=t63_weights/TOTAL(t63_weights)
   
   twentyc_smeans_qld_aavg=fltarr(nyear)
   silo_smeans_qld_aavg=fltarr(nyear)
   FOR i=0,nyear-1 DO BEGIN
      silo_smeans_qld_aavg(i)=TOTAL(REFORM(silo_smeans(*,*,i))*t63_weights)
      twentyc_smeans_qld_aavg(i)=TOTAL(REFORM(twentyc_smeans(*,*,i))*t63_weights)
   ENDFOR
   twentyc_smeans_clim=fltarr(twentyc_nlon,twentyc_nlat)
   silo_smeans_clim=fltarr(silo_nlon,silo_nlat)
   FOR i=0,silo_nlon-1 DO $
      FOR j=0,silo_nlat-1 DO $
         silo_smeans_clim(i,j)=MEAN(silo_smeans(i,j,*))
   FOR i=0,twentyc_nlon-1 DO BEGIN
      FOR j=0,twentyc_nlat-1 DO BEGIN
         IF mask(i,j) eq 1 THEN BEGIN
            twentyc_smeans_clim(i,j)=MEAN(twentyc_smeans(i,j,*))
         ENDIF ELSE BEGIN
            twentyc_smeans_clim(i,j)=!Values.F_NaN
         ENDELSE
      ENDFOR
   ENDFOR
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.silo_t62_'+smean_period+'_1891-2007_clim.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
   CON,X=silo_longitude,Y=silo_latitude,FIELD=silo_smeans_clim*ndays_in_season,/NOLINELABELS,$
       TITLE='Clim season-total ('+smean_period+') precip from SILO on T62 grid (as 20th cent.) - 1891-2008',/BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.20thc_t62_'+smean_period+'_1891-2007_clim.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   LEVS,MANUAL=mylevs
   MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
   CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_smeans_clim*ndays_in_season,/NOLINELABELS,$
       TITLE='Clim season-total ('+smean_period+') precip from 20th Cent V2 - 1891-2008',/BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.20thc-minus-silo_t62_'+smean_period+'_1891-2007_clim.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[8]
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
   CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_smeans_clim*ndays_in_season-silo_smeans_clim*ndays_in_season,/NOLINELABELS,$
       TITLE='Diff in clim season-total ('+smean_period+') precip for 20thC V2 minus SILO - 1891-2008',/BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
   
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.20thc-ratio-silo_t62_'+smean_period+'_1891-2007_clim.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=300,XOFFSET=1200,YOFFSET=1000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,/PORTRAIT
   CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_ratio)+1,/REV,white=[ratio_white]
   LEVS,MANUAL=mylevs_ratio
   MAP,LATMIN=qld_box(2),LATMAX=qld_box(0),LONMIN=qld_box(1)+1,LONMAX=qld_box(3)-0.25,/HIRES
   CON,X=silo_longitude,Y=silo_latitude,FIELD=twentyc_smeans_clim*ndays_in_season/(silo_smeans_clim*ndays_in_season),/NOLINELABELS,$
       TITLE='Ratio of clim season-total ('+smean_period+') precip for 20thC V2 divided by SILO - 1891-2008',/BLOCK,/NOLINES
   PSCLOSE,/NOVIEW
 
   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.20thc_silo_t62_'+smean_period+'_1891-2007_ts.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=1000,TFONT=2,TCHARSIZE=80
   GSET,XMIN=1890,XMAX=2008,YMIN=precip_min,YMAX=precip_max,TITLE='Comparison of 20thC V2 (ensemble-mean) and SILO (on T62 grid) area-avg, seasonal total ('+smean_period+') rainfall for Queensland - 1891-2007'
   black=FSC_COLOR("black",2)
   red=FSC_COLOR("red",3)
   HIST,X=indgen(nyear)+1891,Y=silo_smeans_qld_aavg*ndays_in_season,FILLCOL=2,WIDTH=45
   GPLOT,X=indgen(nyear)+1891,Y=twentyc_smeans_qld_aavg*ndays_in_season,COL=3,THICK=200
   AXES,XSTEP=10,YSTEP=precip_step,XTITLE='Year at beginning of '+smean_period+' seasonal-mean',YTITLE='Queensland area-averaged, seasonal-total ('+smean_period+') rainfall (mm/year)',/NORIGHT
   GSET,XMIN=1890,XMAX=2008,YMIN=ratio_min,YMAX=ratio_max
   GPLOT,X=indgen(nyear)+1891,Y=twentyc_smeans_qld_aavg*ndays_in_season/(silo_smeans_qld_aavg*ndays_in_season),STYLE=2,THICK=150,COL=3
   AXES,XSTEP=10,YSTEP=0.1,YTITLE='20thC V2 (ens-mean) QLD area-avg, ann-mean rain as fraction of SILO',/ONLYRIGHT,NDECS=2
   GPLOT,X=indgen(nyear)+1891,Y=REPLICATE(1.0,nyear),STYLE=1,COL=2,THICK=125
   items=['Ratio of 20thC V2 ens-mean to SILO','20thC ens-mean rainfall','SILO rainfall (on T62 grid)']
   GLEGEND,labels=items,LEGPOS=9,COL=[3,3,2],STYLE=[2,0,0],SYM=[1,1,1],SIZE=[0,0,100],LENGTH=[100,100,0]
   PSCLOSE,/NOVIEW

   hadisst_nino4_infile='/home/ss901165/datasets/NINO/nino4_hadisst.'+smean_period+'_smeans.1871-2008.nc'
   hadisst_offset=20
   hadisst_nino4=REFORM(OPEN_AND_EXTRACT(hadisst_nino4_infile,'NINO4',offset=[hadisst_offset],count=[nyear]))
   hadisst_nino4=hadisst_nino4-MEAN(hadisst_nino4)

   elnino_years=where(hadisst_nino4 gt 0.4)
   lanina_years=where(hadisst_nino4 lt -0.4)
   neutral_years=where(hadisst_nino4 le 0.4 and hadisst_nino4 ge -0.4)
   elnino_regression=REGRESS(silo_smeans_qld_aavg[elnino_years]*ndays_in_season,twentyc_smeans_qld_aavg[elnino_years]*ndays_in_season,CONST=elnino_constant,CORRELATION=elnino_correlation)
   lanina_regression=REGRESS(silo_smeans_qld_aavg[lanina_years]*ndays_in_season,twentyc_smeans_qld_aavg[lanina_years]*ndays_in_season,CONST=lanina_constant,CORRELATION=lanina_correlation)
   neutral_regression=REGRESS(silo_smeans_qld_aavg[neutral_years]*ndays_in_season,twentyc_smeans_qld_aavg[neutral_years]*ndays_in_season,CONST=neutral_constant,CORRELATION=neutral_correlation)
   total_regression=REGRESS(silo_smeans_qld_aavg*ndays_in_season,twentyc_smeans_qld_aavg*ndays_in_season,CONST=total_constant,CORRELATION=total_correlation)

   psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_clim/qld_20thc_precip_clim_compare_silo_smeans.20thc_silo_t62_'+smean_period+'_1891-2007_scatter.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=2500,SPACE2=300,XOFFSET=400,YOFFSET=1000,TFONT=2,TCHARSIZE=100
   GSET,XMIN=precip_min,XMAX=precip_max,YMIN=precip_min,YMAX=precip_max,TITLE='Comparison of 20thC V2 and SILO (on T62 grid) area-averaged, seasonal-mean ('+smean_period+') rainfall for Queensland - 1891-2007'
   red=FSC_COLOR("red",3)
   blue=FSC_COLOR("blue",4)
   black=FSC_COLOR("black",2)
   GPLOT,X=silo_smeans_qld_aavg*ndays_in_season,Y=twentyc_smeans_qld_aavg*ndays_in_season,COL=2,THICK=200,SYM=6,/NOLINES
   GPLOT,X=silo_smeans_qld_aavg[lanina_years]*ndays_in_season,Y=twentyc_smeans_qld_aavg[lanina_years]*ndays_in_season,COL=4,THICK=200,SYM=6,/NOLINES
   GPLOT,X=silo_smeans_qld_aavg[elnino_years]*ndays_in_season,Y=twentyc_smeans_qld_aavg[elnino_years]*ndays_in_season,COL=3,THICK=200,SYM=6,/NOLINES
   
   range=precip_max-precip_min
   GPLOT,X=findgen(10)*range/10.+precip_min,Y=(findgen(10)*range/10.+precip_min)*elnino_regression(0)+elnino_constant,THICK=150,COL=3
   GPLOT,X=findgen(10)*range/10.+precip_min,Y=(findgen(10)*range/10.+precip_min)*lanina_regression(0)+lanina_constant,THICK=150,COL=4
   GPLOT,X=findgen(10)*range/10.+precip_min,Y=(findgen(10)*range/10.+precip_min)*total_regression(0)+total_constant,THICK=150,COL=2
   GPLOT,X=findgen(10)*range/10.+precip_min,Y=(findgen(10)*range/10.+precip_min)*neutral_regression(0)+neutral_constant,THICK=150,COL=2,STYLE=2
   
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.95,TEXT='For La Nina years (Nino 4 < -0.4) - '+STRTRIM(STRING(N_ELEMENTS(lanina_years)),1)+' years',ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.9,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(lanina_regression(0)),1),0,7)+' * SILO_rain + '+$
         STRMID(STRTRIM(STRING(lanina_constant),1),0,5),ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.85,TEXT='Correlation = '+STRMID(STRTRIM(STRING(lanina_correlation),1),0,5),ALIGN=0.0
   
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.8,TEXT='For El Nino years (Nino 4 > 0.4) - '+STRTRIM(STRING(N_ELEMENTS(elnino_years)),1)+' years',ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.75,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(elnino_regression(0)),1),0,7)+' * SILO_rain + '+$
         STRMID(STRTRIM(STRING(elnino_constant),1),0,5),ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.7,TEXT='Correlation = '+STRMID(STRTRIM(STRING(elnino_correlation),1),0,5),ALIGN=0.0
   
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.65,TEXT='For neutral years (-0.4 <= Nino 4 <= 0.4) - '+STRTRIM(STRING(N_ELEMENTS(neutral_years)),1)+' years',ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.6,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(neutral_regression(0)),1),0,7)+' * SILO_rain + '+$
         STRMID(STRTRIM(STRING(neutral_constant),1),0,6),ALIGN=0.0
   GPLOT,X=precip_min+range*0.05,Y=precip_min+range*0.55,TEXT='Correlation = '+STRMID(STRTRIM(STRING(neutral_correlation),1),0,5),ALIGN=0.0
   
   GPLOT,X=precip_min+range*0.6,Y=precip_min+range*0.95,TEXT='For all years - '+STRTRIM(STRING(nyear),1)+' years',ALIGN=0.0
   GPLOT,X=precip_min+range*0.6,Y=precip_min+range*0.9,TEXT='20thC_rain = '+STRMID(STRTRIM(STRING(total_regression(0)),1),0,7)+' * SILO_rain + '+$
         STRMID(STRTRIM(STRING(total_constant),1),0,5),ALIGN=0.0
   GPLOT,X=precip_min+range*0.6,Y=precip_min+range*0.85,TEXT='Correlation = '+STRMID(STRTRIM(STRING(total_correlation),1),0,5),ALIGN=0.0
   
   GPLOT,X=[precip_min,precip_max],Y=[precip_min,precip_max],STYLE=1,COL=2
   AXES,XSTEP=precip_step,YSTEP=precip_step,XTITLE='SILO (T62) Queensland area-averaged '+smean_period+' total rainfall (mm/year)',YTITLE='20thC V2 Queensland area-averaged '+smean_period+' total rainfall (mm/year)'
   PSCLOSE,/NOVIEW

ENDFOR


STOP

END

