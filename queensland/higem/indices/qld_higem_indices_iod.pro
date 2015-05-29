PRO qld_higem_indices_iod

; Construct an IOD index from HiGEM data

sst_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.surf_temp.pac_domain.nc'
mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
n_months=12
n_years=150

; Read SST over two boxes
wbox=[-10,50,10,70]
ebox=[-10,90,0,110]
latitude=OPEN_AND_EXTRACT(sst_infile,'latitude')
longitude=OPEN_AND_EXTRACT(sst_infile,'longitude')
DEFINE_BOUNDARIES,wbox,latitude,longitude,wbox_tx
w_nlon=wbox_tx(3)-wbox_tx(1)
w_nlat=wbox_tx(2)-wbox_tx(0)
DEFINE_BOUNDARIES,ebox,latitude,longitude,ebox_tx
e_nlon=ebox_tx(3)-ebox_tx(1)
e_nlat=ebox_tx(2)-ebox_tx(0)

mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
DEFINE_BOUNDARIES,wbox,mask_latitude,mask_longitude,mask_wbox_tx
DEFINE_BOUNDARIES,ebox,mask_latitude,mask_longitude,mask_ebox_tx

sst_wbox=REFORM(OPEN_AND_EXTRACT(sst_infile,'temp',$
                                 offset=[wbox_tx(1),wbox_tx(0),0,0],$
                                 count=[w_nlon,w_nlat,n_months,n_years]))
mask_wbox=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                  offset=[mask_wbox_tx(1),mask_wbox_tx(0),0,0],$
                                  count=[w_nlon,w_nlat,1,1]))
sst_ebox=REFORM(OPEN_AND_EXTRACT(sst_infile,'temp',$
                                 offset=[ebox_tx(1),ebox_tx(0),0,0],$
                                 count=[e_nlon,e_nlat,n_months,n_years]))
mask_ebox=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                                  offset=[mask_ebox_tx(1),mask_ebox_tx(0),0,0],$
                                  count=[w_nlon,w_nlat,1,1]))

sst_wbox_aavg=fltarr(n_months,n_years)
sst_ebox_aavg=fltarr(n_months,n_years)

FOR i=0,w_nlon-1 DO BEGIN
   FOR j=0,w_nlat-1 DO BEGIN
      IF mask_wbox(i,j) gt 0 THEN BEGIN
         sst_wbox(i,j,*,*)=!Values.F_NaN
      ENDIF ELSE $
         sst_wbox(i,j,*,*)=sst_wbox(i,j,*,*)-MEAN(sst_wbox(i,j,*,*))
   ENDFOR
ENDFOR
FOR i=0,e_nlon-1 DO BEGIN
   FOR j=0,e_nlat-1 DO BEGIN
      IF mask_ebox(i,j) gt 0 THEN BEGIN
         sst_ebox(i,j,*,*)=!Values.F_NaN
      ENDIF ELSE $
         sst_ebox(i,j,*,*)=sst_ebox(i,j,*,*)-MEAN(sst_ebox(i,j,*,*))
   ENDFOR
ENDFOR

FOR i=0,n_months-1 DO BEGIN
   FOR j=0,n_years-1 DO BEGIN
      sst_wbox_aavg(i,j)=MEAN(sst_wbox(*,*,i,j),/NaN)
      sst_ebox_aavg(i,j)=MEAN(sst_ebox(*,*,i,j),/NaN)
   ENDFOR
ENDFOR

iod_index=sst_wbox_aavg-sst_ebox_aavg

iod_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.iod_index.nc'
id=NCDF_CREATE(iod_outfile,/CLOBBER)
dimids=intarr(2)
dimids(0)=NCDF_DIMDEF(id,'month',n_months)
dimids(1)=NCDF_DIMDEF(id,'year',n_years)
varids=intarr(3)
varids(0)=NCDF_VARDEF(id,'month',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'year',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'iod_index',[dimids(0),dimids(1)])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),indgen(n_months)
NCDF_VARPUT,id,varids(1),indgen(n_years)
NCDF_VARPUT,id,varids(2),iod_index
NCDF_CLOSE,id

FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='mar-may'
         offset_start=2
         offset_stop=4
         n_years=150
      END
      1 : BEGIN
         season_name='jun-aug'
         offset_start=5
         offset_stop=7
         n_years=150
      END
      2 : BEGIN
         season_name='sep-nov'
         offset_start=8
         offset_stop=10
         n_years=150
      END
      3 : BEGIN
         season_name='dec-feb'
         offset_start=11
         offset_stop=1
         n_years=150
      END
   ENDCASE
   iod_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_smeans.h9-w8.iod_index.nc'  
   smean_iod_index=fltarr(n_years)
   IF offset_stop gt offset_start THEN BEGIN
      FOR j=0,n_years-1 DO $
         smean_iod_index(j)=MEAN(iod_index(offset_start:offset_stop,j))
   ENDIF ELSE BEGIN
      FOR j=0,n_years-2 DO $
         smean_iod_index(j)=iod_index(offset_start:11,j)*FLOAT(12-offset_start)/FLOAT(offset_stop+13-offset_start)+$
         iod_index(0:offset_stop,j+1)*FLOAT(offset_stop+1)/FLOAT(offset_stop+13-offset_start)
      n_years=n_years-1
   ENDELSE

   id=NCDF_CREATE(iod_outfile,/CLOBBER)
   dimid=NCDF_DIMDEF(id,'season',n_years)
   varids=intarr(2)
   varids(0)=NCDF_VARDEF(id,'season',[dimid])
   varids(1)=NCDF_VARDEF(id,'iod_index',[dimid])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),indgen(n_years)
   NCDF_VARPUT,id,varids(1),smean_iod_index(0:n_years-1)
   NCDF_CLOSE,id
ENDFOR

STOP

END

