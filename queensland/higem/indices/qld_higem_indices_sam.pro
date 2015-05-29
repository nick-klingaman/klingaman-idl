PRO qld_higem_indices_sam

; Construct a SAM index from HiGEM data

mslp_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.mslp.sam_domain.nc'
n_months=12
n_years=150

; Read MSLP at 40S and 65S
latitude=OPEN_AND_EXTRACT(mslp_infile,'latitude')
lat_40s=NEAREST(latitude,-40)
lat_65s=NEAREST(latitude,-65)
longitude=OPEN_AND_EXTRACT(mslp_infile,'longitude')
n_lon=N_ELEMENTS(longitude)

mslp_40s=REFORM(OPEN_AND_EXTRACT(mslp_infile,'p',$
                                 offset=[0,lat_40s,0,0],count=[n_lon,1,n_months,n_years]))
mslp_65s=REFORM(OPEN_AND_EXTRACT(mslp_infile,'p',$
                                 offset=[0,lat_65s,0,0],count=[n_lon,1,n_months,n_years]))

mslp_40s_lonavg=fltarr(n_months,n_years)
mslp_65s_lonavg=fltarr(n_months,n_years)

FOR i=0,n_months-1 DO BEGIN
   FOR j=0,n_years-1 DO BEGIN
      mslp_40s_lonavg(i,j)=MEAN(mslp_40s(*,i,j))
      mslp_65s_lonavg(i,j)=MEAN(mslp_65s(*,i,j))
   ENDFOR
ENDFOR

mslp_40s_lonavg=(mslp_40s_lonavg-MEAN(mslp_40s_lonavg))/STDDEV(mslp_40s_lonavg)
mslp_65s_lonavg=(mslp_65s_lonavg-MEAN(mslp_65s_lonavg))/STDDEV(mslp_65s_lonavg)

sam_index=mslp_40s_lonavg-mslp_65s_lonavg

sam_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.sam_index.nc'
id=NCDF_CREATE(sam_outfile,/CLOBBER)
dimids=intarr(2)
dimids(0)=NCDF_DIMDEF(id,'month',n_months)
dimids(1)=NCDF_DIMDEF(id,'year',n_years)
varids=intarr(3)
varids(0)=NCDF_VARDEF(id,'month',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'year',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'sam_index',[dimids(0),dimids(1)])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),indgen(n_months)
NCDF_VARPUT,id,varids(1),indgen(n_years)
NCDF_VARPUT,id,varids(2),sam_index
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
   sam_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_smeans.h9-w8.sam_index.nc'  
   smean_sam_index=fltarr(n_years)
   IF offset_stop gt offset_start THEN BEGIN
      FOR j=0,n_years-1 DO $
         smean_sam_index(j)=MEAN(sam_index(offset_start:offset_stop,j))
   ENDIF ELSE BEGIN
      FOR j=0,n_years-2 DO $
         smean_sam_index(j)=sam_index(offset_start:11,j)*FLOAT(12-offset_start)/FLOAT(offset_stop+13-offset_start)+$
         sam_index(0:offset_stop,j+1)*FLOAT(offset_stop+1)/FLOAT(offset_stop+13-offset_start)
      n_years=n_years-1
   ENDELSE

   id=NCDF_CREATE(sam_outfile,/CLOBBER)
   dimid=NCDF_DIMDEF(id,'season',n_years)
   varids=intarr(2)
   varids(0)=NCDF_VARDEF(id,'season',[dimid])
   varids(1)=NCDF_VARDEF(id,'sam_index',[dimid])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),indgen(n_years)
   NCDF_VARPUT,id,varids(1),smean_sam_index(0:n_years-1)
   NCDF_CLOSE,id
ENDFOR

STOP

END

