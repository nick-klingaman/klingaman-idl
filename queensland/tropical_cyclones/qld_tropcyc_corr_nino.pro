PRO qld_tropcyc_corr_nino

  ibtracs_infile='/home/ss901165/datasets/IBTRACS/stat_trs_scl.oct-may_smeans.1979-2007.IBTRACS_SP.nc'
  nino_infile='/home/ss901165/datasets/NINO/nino4_hadisst.sep-nov_smeans.1979-2007.nc'

  ibtracs_longitude=OPEN_AND_EXTRACT(ibtracs_infile,'long')
  ibtracs_latitude=OPEN_AND_EXTRACT(ibtracs_infile,'lat')
  ibtracs_nlon=N_ELEMENTS(ibtracs_longitude)
  ibtracs_nlat=N_ELEMENTS(ibtracs_latitude)

  ibtracs_tden=OPEN_AND_EXTRACT(ibtracs_infile,'tden')
  nino4_ts=OPEN_AND_EXTRACT(nino_infile,'NINO4')
                                
  corr_tden=fltarr(ibtracs_nlon,ibtracs_nlat)
  FOR i=0,ibtracs_nlon-1 DO BEGIN
     FOR j=0,ibtracs_nlat-1 DO BEGIN
        corr_tden(i,j)=CORRELATE(REFORM(ibtracs_tden(i,j,*)),$
                                 REFORM(nino4_ts))
     ENDFOR
  ENDFOR

  mylevs_corr=['-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
               '0.05','0.15','0.25','0.35','0.45','0.55']
  psfile='/home/ss901165/idl/queensland/tropical_cyclones/qld_tropcyc_corr_nino.ibtracs_tden.1979-2007.ps'
  PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=1200,YOFFSET=2000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110,$
         YSIZE=10000,SPACE3=500
  CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_corr)+1,white=[8]
  LEVS,MANUAL=mylevs_corr
  MAP,LATMIN=-45,LATMAX=0,LONMIN=20,LONMAX=250
  CON,X=ibtracs_longitude,Y=ibtracs_latitude,FIELD=corr_tden,/NOLINELABELS,$
      TITLE='Correlation between IBTrACS tropical-cyclone track density and Sep-Nov Nino 4',/BLOCK,/NOLINES
  PSCLOSE


  STOP

END
