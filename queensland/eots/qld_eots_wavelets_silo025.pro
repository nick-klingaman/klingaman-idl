PRO qld_eots_wavelets_silo025

; Give files containing patterns and loadings of EOTs for annual and
; seasonal means.

silo_dir='/home/ss901165/datasets_mango/SILO/one_quarter'
silo_ameans_infile=silo_dir+'/SILO.may-apr_ameans.1900-2008.eots.nc'
silo_smeans_infiles=[silo_dir+'/SILO.dec-feb_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.mar-may_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.jun-aug_smeans.1900-2007.eots.nc',$
                     silo_dir+'/SILO.sep-nov_smeans.1900-2007.eots.nc']
n_seasons=N_ELEMENTS(silo_smeans_infiles)

season_periods=['dec-feb','mar-may','jun-aug','sep-nov']

; Number of EOTs to consider
n_eots=6
; Number of years for each EOT
n_years=108

silo_ameans_eots=REFORM(OPEN_AND_EXTRACT(silo_ameans_infile,'loading'))

silo_smeans_eots=fltarr(n_seasons,n_years,n_eots)
FOR i=0,n_seasons-1 DO $
   silo_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(silo_smeans_infiles(i),'loading'))

;mylevs=['4000','8000','16000','32000','64000','128000','256000','512000']
;mylevs_lowseason=['400','800','1600','3200','6400','12800','25600','51200']
FOR i=0,n_eots-1 DO BEGIN
   wave = WAVELET(silo_ameans_eots(*,i),1,$
                  lag1=A_CORRELATE(silo_ameans_eots(*,i),2),siglvl=0.90,signif=sig90,$
                  period=period,scale=scale)
   wave = WAVELET(silo_ameans_eots(*,i),1,$
                  lag1=A_CORRELATE(silo_ameans_eots(*,i),2),siglvl=0.95,signif=sig95,$
                  period=period,scale=scale,coi=coi)
   power=ABS(wave)^2

   mylevs=indgen(11)*MAX(power)/10

   psfile='/home/ss901165/idl/queensland/eots/qld_eots_wavelets_silo025.may-apr_amean.eot'+STRTRIM(STRING(i+1),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=1200,TFONT=2,TCHARSIZE=100
   CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1,/REV
   GSET,XMIN=1900,XMAX=2008,YMIN=50,YMAX=2,/YLOG
   LEVS,MANUAL=mylevs
   CON,FIELD=power,X=indgen(n_years)+1900,Y=period,$
       TITLE="Wavelet transform of EOT "+STRTRIM(STRING(i+1),1)+" for May-Apr annual-total (1900-2008) - SILO 0.25",$
       /NOLINELABELS,CB_TITLE='Power (unitless)'
   LEVS,MANUAL=['0','90']
   white=FSC_COLOR("white",50)
   power_sig90=fltarr(n_years,N_ELEMENTS(period))
   power_sig95=fltarr(n_years,N_ELEMENTS(period))
   FOR j=0,N_ELEMENTS(period)-1 DO BEGIN
      power_sig90(*,j)=power(*,j)/sig90(j)
      power_sig95(*,j)=power(*,j)/sig95(j)
   ENDFOR
   CON,FIELD=power_sig90*90,X=indgen(n_years)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
   LEVS,MANUAL=['0','95']
   CON,FIELD=power_sig95*95,X=indgen(n_years)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[50,50,50,50]
   AXES,XSTEP=10,yvals=[2,3,4,5,6,8,10,15,20,30,40,50],xtitle='Year',ytitle='Period (years)'
   GPLOT,X=indgen(n_years)+1900,Y=coi,COL=50,STYLE=1
   
   PSCLOSE,/NOVIEW
   FOR k=0,n_seasons-1 DO BEGIN
      wave = WAVELET(silo_smeans_eots(k,*,i),1,$
                  lag1=A_CORRELATE(silo_smeans_eots(k,*,i),1),siglvl=0.90,signif=sig90,$
                  period=period,scale=scale)
      wave = WAVELET(silo_smeans_eots(k,*,i),1,$
                  lag1=A_CORRELATE(silo_smeans_eots(k,*,i),1),siglvl=0.95,signif=sig95,$
                  period=period,scale=scale,coi=coi)
      power=ABS(wave)^2
      mylevs=indgen(11)*FLOOR(MAX(power)/10)
      mylevs=mylevs[1:N_ELEMENTS(mylevs)-1]
                                ;psfile='/home/ss901165/idl/queensland/eots/qld_eots_wavelets_silo025.'+season_periods(k)+'_smean.eot'+STRTRIM(STRING(i+1),1)+'.ps'
      psfile='/home/ss901165/idl/queensland/eots/qld_eots_wavelets_silo025.'+season_periods(k)+'_smean.eot'+STRTRIM(STRING(i+1),1)+'.bw.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=1200,TFONT=2,TCHARSIZE=100
                                ;CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
      CS,SCALE=7,NCOLS=N_ELEMENTS(mylevs)+1
      GSET,XMIN=1900,XMAX=2008,YMIN=50,YMAX=2,/YLOG
      ;IF k eq 2 THEN BEGIN
      ;   LEVS,MANUAL=mylevs_lowseason
      ;ENDIF ELSE $
      LEVS,MANUAL=mylevs
      CON,FIELD=power,X=indgen(n_years)+1900,Y=period,$
          ;TITLE="Wavelet transform of EOT "+STRTRIM(STRING(i+1),1)+" for "+season_periods(k)+" seasonal-total (1900-2008) - SILO 0.25",$
          /NOLINES,CB_TITLE='Power (unitless)'
      LEVS,MANUAL=['0','90']
      white=FSC_COLOR("white",50)
      black=FSC_COLOR("black",51)
      power_sig90=fltarr(n_years,N_ELEMENTS(period))
      power_sig95=fltarr(n_years,N_ELEMENTS(period))
      FOR j=0,N_ELEMENTS(period)-1 DO BEGIN
         power_sig90(*,j)=power(*,j)/sig90(j)
         power_sig95(*,j)=power(*,j)/sig95(j)
      ENDFOR
      CON,FIELD=power_sig90*90,X=indgen(n_years)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[51,51,51,51],THICK=250
      LEVS,MANUAL=['0','95']
      CON,FIELD=power_sig95*95,X=indgen(n_years)+1900,Y=period,/NOFILL,/NOCOLBAR,COL=[51,51,51,51],THICK=250
      AXES,XSTEP=10,yvals=[2,3,4,5,6,8,10,12,15,20,25,30,40,50],xtitle='Year',ytitle='Period (years)'
      GPLOT,X=indgen(n_years-6)+1900+3,Y=coi(3:N_ELEMENTS(coi)-4),COL=51,STYLE=2,THICK=150
      PSCLOSE,/NOVIEW
   ENDFOR
ENDFOR

STOP

END
