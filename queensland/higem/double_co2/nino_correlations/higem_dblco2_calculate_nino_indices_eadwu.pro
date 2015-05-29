PRO higem_dblco2_calculate_nino_indices_eadwu

eadwu_indir='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu'
eadwu_nyears=32

nino3_box=[-5,210,5,270]
nino34_box=[-6,190,6,240]
nino4_box=[-6,160,6,210]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
n_months=N_ELEMENTS(months)

eadwu_nino3_ts=fltarr(eadwu_nyears*n_months)
eadwu_nino4_ts=fltarr(eadwu_nyears*n_months)
eadwu_nino34_ts=fltarr(eadwu_nyears*n_months)

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
      eadwu_clim_file=eadwu_indir+'/higem_eadwu.'+months(i)+'_mmean_clim.o2-r3.surf_temp.global_domain.nc'
      eadwu_mmean_file=eadwu_indir+'/higem_eadwu.'+months(i)+'_mmeans.o2-r3.surf_temp.global_domain.nc'
      
      IF i eq 0 THEN BEGIN
         latitude=OPEN_AND_EXTRACT(eadwu_clim_file,'latitude')
         longitude=OPEN_AND_EXTRACT(eadwu_clim_file,'longitude')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
      ENDIF
      
      eadwu_clim_temps=REFORM(OPEN_AND_EXTRACT(eadwu_clim_file,'temp_2',$
                                               offset=[box_tx(1),box_tx(0),0,0],$
                                               count=[n_lon,n_lat,1,1]))
      eadwu_mmean_temps=REFORM(OPEN_AND_EXTRACT(eadwu_mmean_file,'temp_2',$
                                                offset=[box_tx(1),box_tx(0),0,0,0],$
                                                count=[n_lon,n_lat,1,eadwu_nyears,1]))
      eadwu_clim_aavg=MEAN(eadwu_clim_temps(*,*))
      
      eadwu_mmean_aavg=fltarr(eadwu_nyears)
      FOR j=0,eadwu_nyears-1 DO BEGIN
         eadwu_mmean_aavg(j)=MEAN(eadwu_mmean_temps(*,*,j))
         CASE m OF 
            0 : BEGIN
               eadwu_nino3_ts(j*12+i)=eadwu_mmean_aavg(j);-eadwu_clim_aavg
            END
            1 : BEGIN
               eadwu_nino4_ts(j*12+i)=eadwu_mmean_aavg(j);-eadwu_clim_aavg
            END
            2 : BEGIN
               eadwu_nino34_ts(j*12+i)=eadwu_mmean_aavg(j);-eadwu_clim_aavg
            END
         ENDCASE
      ENDFOR
   ENDFOR
ENDFOR

outfile='/home/ss901165/higem_qccce/es_2xco2stbl_eadwu/higem_eadwu.jan-dec_mmeans.o2-r3.nino_indices.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimid=NCDF_DIMDEF(id,'time',/UNLIMITED)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'time',[dimid])
varids(1)=NCDF_VARDEF(id,'nino3',[dimid])
varids(2)=NCDF_VARDEF(id,'nino4',[dimid])
varids(3)=NCDF_VARDEF(id,'nino34',[dimid])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),findgen(eadwu_nyears*12)+0.5
NCDF_VARPUT,id,varids(1),eadwu_nino3_ts
NCDF_VARPUT,id,varids(2),eadwu_nino4_ts
NCDF_VARPUT,id,varids(3),eadwu_nino34_ts

NCDF_CLOSE,id

STOP

END



