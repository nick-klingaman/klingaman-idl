PRO qld_higem_eots_wavelets_eafeb

; Give files containing patterns and loadings of EOTs for annual and
; seasonal means.

higem_dir='/home/ss901165/higem_qccce/es_control_eafeb'
higem_smeans_infiles=[higem_dir+'/higem_eafeb.dec-feb_smeans.h9-w8.eots.nc',$
                     higem_dir+'/higem_eafeb.mar-may_smeans.h9-w8.eots.nc',$
                     higem_dir+'/higem_eafeb.jun-aug_smeans.h9-w8.eots.nc',$
                     higem_dir+'/higem_eafeb.sep-nov_smeans.h9-w8.eots.nc']
n_seasons=N_ELEMENTS(higem_smeans_infiles)

season_periods=['dec-feb','mar-may','jun-aug','sep-nov']

; Number of EOTs to consider
n_eots=6
; Number of years for each EOT
n_years=149

higem_smeans_eots=fltarr(n_seasons,n_years,n_eots)
FOR i=0,n_seasons-1 DO $
   higem_smeans_eots(i,*,*)=REFORM(OPEN_AND_EXTRACT(higem_smeans_infiles(i),'loading'))

;mylevs=['4000','8000','16000','32000','64000','128000','256000','512000']
;mylevs_lowseason=['400','800','1600','3200','6400','12800','25600','51200']
FOR i=0,n_eots-1 DO BEGIN
   FOR k=0,n_seasons-1 DO BEGIN
      wave = WAVELET(higem_smeans_eots(k,*,i),1,$
                  lag1=A_CORRELATE(higem_smeans_eots(k,*,i),1),siglvl=0.90,signif=sig90,$
                  period=period,scale=scale)
      wave = WAVELET(higem_smeans_eots(k,*,i),1,$
                  lag1=A_CORRELATE(higem_smeans_eots(k,*,i),1),siglvl=0.95,signif=sig95,$
                  period=period,scale=scale,coi=coi)
      power=ABS(wave)^2
      mylevs=indgen(11)*FLOOR(MAX(power)/10)
      mylevs=mylevs[1:N_ELEMENTS(mylevs)-1]
      psfile='/home/ss901165/idl/queensland/higem/eots/qld_higem_eots_wavelets_eafeb.'+season_periods(k)+'_smean.eot'+STRTRIM(STRING(i+1),1)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1500,SPACE2=800,XOFFSET=1000,YOFFSET=1200,TFONT=2,TCHARSIZE=100
      CS,SCALE=24,NCOLS=N_ELEMENTS(mylevs)+1
      GSET,XMIN=0,XMAX=n_years-1,YMIN=80,YMAX=2,/YLOG
      LEVS,MANUAL=mylevs
      CON,FIELD=power,X=indgen(n_years),Y=period,$
          TITLE="Wavelet transform of EOT "+STRTRIM(STRING(i+1),1)+" for "+season_periods(k)+" seasonal-total (h9-w8) - HiGEM control",$
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
      CON,FIELD=power_sig90*90,X=indgen(n_years),Y=period,/NOFILL,/NOCOLBAR,COL=[51,51,51,51],THICK=250
      LEVS,MANUAL=['0','95']
      CON,FIELD=power_sig95*95,X=indgen(n_years),Y=period,/NOFILL,/NOCOLBAR,COL=[51,51,51,51],THICK=250
      AXES,XSTEP=10,yvals=[2,3,4,5,6,8,10,15,20,25,30,40,50,60,80],xtitle='Year',ytitle='Period (years)'
      GPLOT,X=indgen(n_years-6)+3,Y=coi(3:N_ELEMENTS(coi)-4),COL=51,STYLE=2,THICK=150      
      PSCLOSE
   ENDFOR
ENDFOR

STOP

END
