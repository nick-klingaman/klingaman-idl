PRO hadisst_compute_ipo_eof

  ;hadisst_infile='/home/ss901165/datasets/HADISST/HadISST.jan-dec_mmeans.1870-2009.sst.nc'
  hadisst_infile='/home/ss901165/datasets/HADISST/HadISST.jan-dec_mmeans.1870-2009.sst_lowpassfilter_13yr.nc'
 
  box=[-40,-180,60,180]

  longitude=OPEN_AND_EXTRACT(hadisst_infile,'longitude')
  latitude=OPEN_AND_EXTRACT(hadisst_infile,'latitude')
  DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
  nlon=N_ELEMENTS(longitude)
  nlat=N_ELEMENTS(latitude)
  nyears=140
  nmonths=12

;  hadisst_mmean_ssts=REFORM(OPEN_AND_EXTRACT(hadisst_infile,'sst',$
;                                             offset=[box_tx(1),box_tx(0),0],$
;                                             count=[nlon,nlat,nyears*nmonths]));,[nlon,nlat,nyears*nmonths])
  hadisst_mmean_ssts_lpf_good=fltarr(nlon,nlat,(nyears-26)*12)
  hadisst_mmean_ssts_lpf_good=REFORM(OPEN_AND_EXTRACT(hadisst_infile,'sst',offset=[box_tx(1),box_tx(0),0],$
                                                     count=[nlon,nlat,nmonths*(nyears-26)]),[nlon,nlat,nmonths*(nyears-26)])
 ; hadisst_mmean_ssts_lpf_good[where(ABS(hadisst_mmean_ssts_lpf_good) ge 1e10)]=0.

  ;STOP
;  FOR i=0,nlon-1 DO BEGIN
;     print,'Low-pass filtering longitude band '+STRTRIM(STRING(i+1),1)+' of '+STRTRIM(STRING(nlon),1)+' ...'     
;     FOR j=0,nlat-1 DO BEGIN   
;        IF hadisst_mmean_ssts(i,j,0) ne -1e30 THEN BEGIN
;           hadisst_mmean_ssts_lpf=LANCZOS_BANDPASS(REFORM(hadisst_mmean_ssts(i,j,*)),0.,1/(13.*12.),300)
;           hadisst_mmean_ssts_lpf_good(i,j,*)=hadisst_mmean_ssts_lpf(13*12:(nyears-13)*12-1)
;        ENDIF ELSE $
;           hadisst_mmean_ssts_lpf_good(i,j,*)=2e20
;     ENDFOR
;  ENDFOR

  filter_outfile='/home/ss901165/datasets/HADISST/HadISST.jan-dec_mmeans.1870-2009.sst_lowpassfilter_13yr.nc'
  id=NCDF_CREATE(filter_outfile,/CLOBBER)
  dimids=intarr(4)
  varids=intarr(5)
  dimids(0)=NCDF_DIMDEF(id,'longitude',nlon)
  dimids(1)=NCDF_DIMDEF(id,'latitude',nlat)
  dimids(2)=NCDF_DIMDEF(id,'time',(nyears-26)*nmonths)
  varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
  varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
  varids(2)=NCDF_VARDEF(id,'time',dimids(2))
  varids(4)=NCDF_VARDEF(id,'sst',[dimids(0),dimids(1),dimids(2)])
  
  NCDF_ATTPUT,id,varids(4),'missing_value',2e20
  
  NCDF_CONTROL,id,/ENDEF
  
  NCDF_VARPUT,id,varids(0),longitude
  NCDF_VARPUT,id,varids(1),latitude
  NCDF_VARPUT,id,varids(2),indgen((nyears-26)*nmonths)*15
  NCDF_VARPUT,id,varids(4),hadisst_mmean_ssts_lpf_good
  
  NCDF_CLOSE,id
  
  eof_input=hadisst_mmean_ssts_lpf_good
  EOFcalc,eof_input,latitude,hadisst_mmean_ssts_eofs,hadisst_mmean_ssts_pcs,$
          hadisst_mmean_ssts_svls,hadisst_mmean_ssts_fves,5
  
  eof_outfile='/home/ss901165/datasets/HADISST/HadISST.ipo_eofs.from_13yr_lowpass.nc'
  id=NCDF_CREATE(eof_outfile,/CLOBBER)
  dimids=intarr(5)
  varids=intarr(6)
  dimids(0)=NCDF_DIMDEF(id,'longitude',nlon)
  dimids(1)=NCDF_DIMDEF(id,'latitude',nlat)
  dimids(2)=NCDF_DIMDEF(id,'time',(nyears-26)*nmonths)
  dimids(3)=NCDF_DIMDEF(id,'eof',5)
  varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
  varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
  varids(2)=NCDF_VARDEF(id,'time',dimids(2))  
  varids(3)=NCDF_VARDEF(id,'eofs',[dimids(0),dimids(1),dimids(3)])
  varids(4)=NCDF_VARDEF(id,'pcs',[dimids(2),dimids(3)])
  varids(5)=NCDF_VARDEF(id,'varexp',[dimids(3)])

  NCDF_CONTROL,id,/ENDEF
  NCDF_VARPUT,id,varids(0),longitude
  NCDF_VARPUT,id,varids(1),latitude
  NCDF_VARPUT,id,varids(2),indgen(nmonths*(nyears-26))*15
  NCDF_VARPUT,id,varids(3),hadisst_mmean_ssts_eofs
  NCDF_VARPUT,id,varids(4),hadisst_mmean_ssts_pcs
  NCDF_VARPUT,id,varids(5),hadisst_mmean_ssts_fves

  NCDF_CLOSE,id

  mylevs_corr=['-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05',$
               '0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85']
  mylevs_eof=findgen(11)*(MAX(hadisst_mmean_ssts_eofs)-MIN(hadisst_mmean_ssts_eofs))/10.+MIN(hadisst_mmean_ssts_eofs)
  FOR i=0,4 DO BEGIN
     psfile='/home/ss901165/idl/hadisst/hadisst_compute_ipo_eof_mmeans.eof'+STRTRIM(STRING(i+1),1)+'.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_eof)+1
     MAP,/hires,LONMIN=0,LONMAX=360,LATMIN=MIN(latitude),LATMAX=MAX(latitude)
     LEVS,MANUAL=mylevs_eof
     CON,X=longitude,Y=latitude,FIELD=REFORM(hadisst_mmean_ssts_eofs(*,*,i)),/NOLINELABELS
     AXES
     PSCLOSE,/NOVIEW

     eof_corr=fltarr(nlon,nlat)
     FOR j=0,nlon-1 DO $
        FOR k=0,nlat-1 DO $
           eof_corr(j,k)=CORRELATE(hadisst_mmean_ssts_lpf_good(j,k,*),hadisst_mmean_ssts_pcs(*,i))
     
     psfile='/home/ss901165/idl/hadisst/hadisst_compute_ipo_eof_mmeans.eof'+STRTRIM(STRING(i+1),1)+'_corr.ps'
     PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1500,SPACE2=1000,XOFFSET=500,YOFFSET=2500,TFONT=2,TCHARSIZE=100,CB_WIDTH=105
     CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_corr)+1
     MAP,/hires,LONMIN=0,LONMAX=360,LATMIN=MIN(latitude),LATMAX=MAX(latitude)
     LEVS,MANUAL=mylevs_corr
     CON,X=longitude,Y=latitude,FIELD=REFORM(eof_corr),/NOLINELABELS,/BLOCK,/NOLINES
     AXES
     PSCLOSE,/NOVIEW
  ENDFOR

  STOP

END
