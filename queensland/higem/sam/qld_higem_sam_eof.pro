PRO qld_higem_sam_eof

mslp_mmeans_infile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.jan-dec_mmeans.h9-w8.mslp.global_domain.nc'

box=[-90,0,0,360]

longitude=OPEN_AND_EXTRACT(mslp_mmeans_infile,'longitude')
latitude=OPEN_AND_EXTRACT(mslp_mmeans_infile,'latitude')
DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_months=12
offset=0
n_years=40

mslp_mmeans=REFORM(OPEN_AND_EXTRACT(mslp_mmeans_infile,'p',$
                                    offset=[box_tx(1),box_tx(0),0,offset],$
                                    count=[n_lon,n_lat,n_months,n_years]),$
                   [n_lon,n_lat,n_years*n_months])/100.

EOFcalc,mslp_mmeans,latitude,mslp_mmeans_eofs,mslp_mmeans_pcs,$
        mslp_mmeans_svls,mslp_mmeans_fves,1

STOP

eof_outfile='/home/ss901165/higem_qccce/es_control_eafeb/higem_eafeb.sam_eofs.from_mmean_mslp.nc'
id=NCDF_CREATE(eof_outfile,/CLOBBER)
dimids=intarr(5)
varids=intarr(6)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'month',n_months)
dimids(3)=NCDF_DIMDEF(id,'year',n_years)
dimids(4)=NCDF_DIMDEF(id,'eof',1)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'month',dimids(2))
varids(3)=NCDF_VARDEF(id,'year',dimids(3))
varids(4)=NCDF_VARDEF(id,'eofs',[dimids(0),dimids(1),dimids(4)])
varids(5)=NCDF_VARDEF(id,'pcs',[dimids(2),dimids(3),dimids(4)])

NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),indgen(n_months)
NCDF_VARPUT,id,varids(3),indgen(n_years)
NCDF_VARPUT,id,varids(4),mslp_mmeans_eofs
NCDF_VARPUT,id,varids(5),REFORM(mslp_mmeans_pcs,[n_months,n_years,1])

NCDF_CLOSE,id

STOP
END
