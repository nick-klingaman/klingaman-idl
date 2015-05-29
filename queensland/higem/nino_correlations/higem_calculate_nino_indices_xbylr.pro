PRO higem_calculate_nino_indices_xbylr

xbylr_indir='/home/ss901165/higem_qccce/hpcx_control_xbylr'
xbylr_nyears=117

nino3_box=[-5,210,5,270]
nino34_box=[-6,190,6,240]
nino4_box=[-6,160,6,210]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
n_months=N_ELEMENTS(months)

xbylr_nino3_ts=fltarr(xbylr_nyears*n_months)
xbylr_nino4_ts=fltarr(xbylr_nyears*n_months)
xbylr_nino34_ts=fltarr(xbylr_nyears*n_months)

FOR m=0,2 DO BEGIN
   CASE m OF
      0 : BEGIN
         box=nino3_box
      END
      1 : BEGIN
         box=nino4_box
      END
      2 : BEGIN
         box=nino34_box
      END
   ENDCASE
   FOR i=0,n_months-1 DO BEGIN
      xbylr_clim_file=xbylr_indir+'/higem_xbylr.'+months(i)+'_mmean_clim.h9-t5.surf_temp.pac_domain.nc'
      xbylr_mmean_file=xbylr_indir+'/higem_xbylr.'+months(i)+'_mmeans.h9-t5.surf_temp.pac_domain.nc'
      
      IF i eq 0 THEN BEGIN
         latitude=OPEN_AND_EXTRACT(xbylr_clim_file,'latitude')
         longitude=OPEN_AND_EXTRACT(xbylr_clim_file,'longitude')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
      ENDIF
      
      xbylr_clim_temps=REFORM(OPEN_AND_EXTRACT(xbylr_clim_file,'temp',$
                                               offset=[box_tx(1),box_tx(0),0,0],$
                                               count=[n_lon,n_lat,1,1]))
      xbylr_mmean_temps=REFORM(OPEN_AND_EXTRACT(xbylr_mmean_file,'temp',$
                                                offset=[box_tx(1),box_tx(0),0,0,0],$
                                                count=[n_lon,n_lat,1,xbylr_nyears,1]))
      xbylr_clim_aavg=MEAN(xbylr_clim_temps(*,*))
      
      xbylr_mmean_aavg=fltarr(xbylr_nyears)
      FOR j=0,xbylr_nyears-1 DO BEGIN
         xbylr_mmean_aavg(j)=MEAN(xbylr_mmean_temps(*,*,j))
         CASE m OF 
            0 : BEGIN
               xbylr_nino3_ts(j*12+i)=xbylr_mmean_aavg(j)-xbylr_clim_aavg
            END
            1 : BEGIN
               xbylr_nino4_ts(j*12+i)=xbylr_mmean_aavg(j)-xbylr_clim_aavg
            END
            2 : BEGIN
               xbylr_nino34_ts(j*12+i)=xbylr_mmean_aavg(j)-xbylr_clim_aavg
            END
         ENDCASE
      ENDFOR
   ENDFOR
ENDFOR

outfile='/home/ss901165/higem_qccce/hpcx_control_xbylr/higem_xbylr.jan-dec_mmeans.h9-t5.nino_indices.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimid=NCDF_DIMDEF(id,'time',/UNLIMITED)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'time',[dimid])
varids(1)=NCDF_VARDEF(id,'nino3',[dimid])
varids(2)=NCDF_VARDEF(id,'nino4',[dimid])
varids(3)=NCDF_VARDEF(id,'nino34',[dimid])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),findgen(xbylr_nyears*12)+0.5
NCDF_VARPUT,id,varids(1),xbylr_nino3_ts
NCDF_VARPUT,id,varids(2),xbylr_nino4_ts
NCDF_VARPUT,id,varids(3),xbylr_nino34_ts

NCDF_CLOSE,id

STOP

END



