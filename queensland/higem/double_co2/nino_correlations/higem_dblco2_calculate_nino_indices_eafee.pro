PRO higem_dblco2_calculate_nino_indices_eafee

eafee_indir='/home/ss901165/higem_qccce/es_2pctco2_eafee'
eafee_nyears=99

nino3_box=[-5,210,5,270]
nino34_box=[-6,190,6,240]
nino4_box=[-6,160,6,210]

months=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec']
n_months=N_ELEMENTS(months)

eafee_nino3_ts=fltarr(eafee_nyears*n_months)
eafee_nino4_ts=fltarr(eafee_nyears*n_months)
eafee_nino34_ts=fltarr(eafee_nyears*n_months)

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
      eafee_clim_file=eafee_indir+'/higem_eafee.'+months(i)+'_mmean_clim.k9-u7.surf_temp.global_domain.nc'
      eafee_mmean_file=eafee_indir+'/higem_eafee.'+months(i)+'_mmeans.k9-u7.surf_temp.global_domain.nc'
      
      IF i eq 0 THEN BEGIN
         latitude=OPEN_AND_EXTRACT(eafee_clim_file,'latitude')
         longitude=OPEN_AND_EXTRACT(eafee_clim_file,'longitude')
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
      ENDIF
      
      eafee_clim_temps=REFORM(OPEN_AND_EXTRACT(eafee_clim_file,'temp_2',$
                                               offset=[box_tx(1),box_tx(0),0,0],$
                                               count=[n_lon,n_lat,1,1]))
      eafee_mmean_temps=REFORM(OPEN_AND_EXTRACT(eafee_mmean_file,'temp_2',$
                                                offset=[box_tx(1),box_tx(0),0,0,0],$
                                                count=[n_lon,n_lat,1,eafee_nyears,1]))
      eafee_clim_aavg=MEAN(eafee_clim_temps(*,*))
      
      eafee_mmean_aavg=fltarr(eafee_nyears)
      FOR j=0,eafee_nyears-1 DO BEGIN
         eafee_mmean_aavg(j)=MEAN(eafee_mmean_temps(*,*,j))
         CASE m OF 
            0 : BEGIN
               eafee_nino3_ts(j*12+i)=eafee_mmean_aavg(j);-eafee_clim_aavg
            END
            1 : BEGIN
               eafee_nino4_ts(j*12+i)=eafee_mmean_aavg(j);-eafee_clim_aavg
            END
            2 : BEGIN
               eafee_nino34_ts(j*12+i)=eafee_mmean_aavg(j);-eafee_clim_aavg
            END
         ENDCASE
      ENDFOR
   ENDFOR
ENDFOR

outfile='/home/ss901165/higem_qccce/es_2pctco2_eafee/higem_eafee.jan-dec_mmeans.k9-u7.nino_indices.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimid=NCDF_DIMDEF(id,'time',/UNLIMITED)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,'time',[dimid])
varids(1)=NCDF_VARDEF(id,'nino3',[dimid])
varids(2)=NCDF_VARDEF(id,'nino4',[dimid])
varids(3)=NCDF_VARDEF(id,'nino34',[dimid])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),findgen(eafee_nyears*12)+0.5
NCDF_VARPUT,id,varids(1),eafee_nino3_ts
NCDF_VARPUT,id,varids(2),eafee_nino4_ts
NCDF_VARPUT,id,varids(3),eafee_nino34_ts

NCDF_CLOSE,id

STOP

END



