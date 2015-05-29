PRO qld_higem_indices_bom_blocking

; Construct the Bureau of Meteorology blocking index from HiGEM data

u500_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.1994-2128.u500.nc'
n_months=12
n_years=135

; Read U500 and select latitudes
latitude=OPEN_AND_EXTRACT(u500_infile,'latitude1')
longitude=OPEN_AND_EXTRACT(u500_infile,'longitude0')
box=[-65,0,-20,360]
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lat=N_ELEMENTS(latitude)
n_lon=N_ELEMENTS(longitude)
lat_25s=NEAREST(latitude,-25)
lat_30s=NEAREST(latitude,-30)
lat_40s=NEAREST(latitude,-40)
lat_45s=NEAREST(latitude,-45)
lat_50s=NEAREST(latitude,-50)
lat_55s=NEAREST(latitude,-55)
lat_60s=NEAREST(latitude,-60)

mmean_u500=REFORM(OPEN_AND_EXTRACT(u500_infile,'u_2',$
                                   offset=[box_tx(1),box_tx(0),0,0],$
                                   count=[n_lon,n_lat,1,n_months*n_years-1]))
mar_2053_offset=(2052-1994)*12+2
mmean_u500_nomiss=fltarr(n_lon,n_lat,n_months*n_years)

mmean_u500_nomiss(*,*,0:mar_2053_offset-1)=mmean_u500(*,*,0:mar_2053_offset-1)
mmean_u500_nomiss(*,*,mar_2053_offset)=(mmean_u500(*,*,mar_2053_offset-1)+mmean_u500(*,*,mar_2053_offset))/2.
mmean_u500_nomiss(*,*,mar_2053_offset+1:n_months*n_years-1)=mmean_u500(*,*,mar_2053_offset:n_months*n_years-2)

bom_index=fltarr(n_lon,n_months*n_years)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_years*n_months-1 DO BEGIN
      bom_index(i,j)=0.5*(mmean_u500_nomiss(i,lat_25s,j)+$
                          mmean_u500_nomiss(i,lat_30s,j)-$
                          mmean_u500_nomiss(i,lat_40s,j)-$
                          2*mmean_u500_nomiss(i,lat_45s,j)-$
                          mmean_u500_nomiss(i,lat_50s,j)+$
                          mmean_u500_nomiss(i,lat_55s,j)+$
                          mmean_u500_nomiss(i,lat_60s))
   ENDFOR
ENDFOR

bom_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.j4-w8.bom_blocking_index.nc'
id=NCDF_CREATE(bom_outfile,/CLOBBER)
dimids=intarr(2)
dimids(1)=NCDF_DIMDEF(id,'month',n_months*n_years)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
varids=intarr(3)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'month',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'bom_index',[dimids(0),dimids(1)])
NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),indgen(n_months*n_years)
NCDF_VARPUT,id,varids(2),bom_index
NCDF_CLOSE,id

FOR i=0,3 DO BEGIN
   CASE i OF
      0 : BEGIN
         season_name='mar-may'
         offset_start=2
         offset_stop=4
         n_years=135
      END
      1 : BEGIN
         season_name='jun-aug'
         offset_start=5
         offset_stop=7
         n_years=135
      END
      2 : BEGIN
         season_name='sep-nov'
         offset_start=8
         offset_stop=10
         n_years=135
      END
      3 : BEGIN
         season_name='dec-feb'
         offset_start=11
         offset_stop=1
         n_years=135
      END
   ENDCASE
   bom_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.'+season_name+'_smeans.j4-w8.bom_blocking_index.nc'  
   smean_bom_index=fltarr(n_lon,n_years)
   IF offset_stop gt offset_start THEN BEGIN
      FOR j=0,n_lon-1 DO BEGIN
         FOR k=0,n_years-1 DO $
            smean_bom_index(j,k)=MEAN(bom_index(j,offset_start+(k*12):offset_stop+(k*12)))
      ENDFOR
   ENDIF ELSE BEGIN
      FOR j=0,n_lon-1 DO BEGIN 
         FOR k=0,n_years-2 DO $
            smean_bom_index(j,k)=bom_index(j,offset_start+(k*12):11+(k*12))*FLOAT(12-offset_start)/FLOAT(offset_stop+13-offset_start)+$
            bom_index(j,((k+1)*12):offset_stop+((k+1)*12))*FLOAT(offset_stop+1)/FLOAT(offset_stop+13-offset_start)
      ENDFOR
      n_years=n_years-1
   ENDELSE

   id=NCDF_CREATE(bom_outfile,/CLOBBER)
   dimids=intarr(2)
   dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
   dimids(1)=NCDF_DIMDEF(id,'season',n_years)
   varids=intarr(3)
   varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'season',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'bom_index',[dimids(0),dimids(1)])
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),longitude
   NCDF_VARPUT,id,varids(1),indgen(n_years)
   NCDF_VARPUT,id,varids(2),smean_bom_index(*,0:n_years-1)
   NCDF_CLOSE,id
ENDFOR

STOP

END

