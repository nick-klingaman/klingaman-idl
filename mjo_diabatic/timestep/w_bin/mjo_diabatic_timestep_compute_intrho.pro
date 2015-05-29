PRO mjo_diabatic_timestep_compute_intrho

metum='/home/ss901165/um_output6/mjodiab_2day/metum'
cancm4='/home/ss901165/um_output6/mjodiab_2day/cancm4'
mri='/home/ss901165/um_output6/mjodiab_2day/mri'
giss='/home/ss901165/um_output6/mjodiab_2day/giss'
cnrm='/home/ss901165/um_output6/mjodiab_2day/cnrm'
spcam='/home/ss901165/um_output6/mjodiab_2day/spcam'
nasa='/home/ss901165/um_output6/mjodiab_2day/nasa'

;rho_file=metum+'/MetUM.rho.20091020-20100110.lead_12-48hrs.nc'
;rho_file=cancm4+'/CanCM4.rho.20091020-20100110.lead_12-48hrs.nc'
;rho_file=mri+'/MRI-AGCM.rho.20091020-20100110.lead_12-48hrs.nc'
rho_file=giss+'/ModelE.rho.20091020-20100110.lead_12-48hrs.nc'
;rho_file=cnrm+'/CNRM.rho.20091020-20100110.lead_12-48hrs.nc'
;rho_file=spcam+'/SPCAM3.0.rho.20091020-20100110.lead_12-48hrs.nc'
;rho_file=nasa+'/GEOS5_AGCM.rho.20091020-20100110.lead_12-48hrs.nc'

box=[-10,60,10,160]
lat=OPEN_AND_EXTRACT(rho_file,'latitude')
lon=OPEN_AND_EXTRACT(rho_file,'longitude')
DEFINE_BOUNDARIES,box,lat,lon,box_tx,/LIMIT
nlat=N_ELEMENTS(lat)
nlon=N_ELEMENTS(lon)
z=OPEN_AND_EXTRACT(rho_file,'level')
;nz=50
;nt=180*44
nz=30
nt=72*44
offset_time=0

; Read rho
rho=OPEN_AND_EXTRACT(rho_file,'rho',offset=[box_tx(1),box_tx(0),0,offset_time],$
                     count=[nlon,nlat,nz,nt])

n_files=2
FOR i=0,n_files-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile=giss+'/ModelE.mcu.20091020-20100110.lead_12-48hrs.nc'
         varname='mcu'
         int_bot=11
         int_top=25
         outfile=giss+'/ModelE.mcu_intrho_lev11-25.20091020-20100110.lead_12-48hrs.nc'
      END
      ;1 : BEGIN
      ;   infile=mri+'/ModelE.mcu.20091020-20100110.lead_12-48hrs.nc'
      ;   varname='mcu'
      ;   int_bot=20
      ;   int_top=26
      ;   outfile=mri+'/ModelE.mcu_intrho_lev20-26.20091020-20100110.lead_12-48hrs.nc'
      ;END
      1 : BEGIN
         infile=giss+'/ModelE.mcu.20091020-20100110.lead_12-48hrs.nc'
         varname='mcu'
         int_bot=0
         int_top=25
         outfile=giss+'/ModelE.mcu_intrho_lev0-25.20091020-20100110.lead_12-48hrs.nc'
      END
   ENDCASE

   ; Read variable
   var_in=OPEN_AND_EXTRACT(infile,varname,offset=[box_tx(1),box_tx(0),0,offset_time],$
                           count=[nlon,nlat,nz,nt])
   var_out=fltarr(nlon,nlat,nt)
      
   FOR j=0,nlon-1 DO BEGIN
      FOR k=0,nlat-1 DO BEGIN
         FOR m=0,nt-1 DO BEGIN
            rhosum=TOTAL(rho(j,k,int_bot:int_top,m))
            var_out(j,k,m)=TOTAL(rho(j,k,int_bot:int_top,m)*var_in(j,k,int_bot:int_top,m))/rhosum
         ENDFOR
      ENDFOR
   ENDFOR

   id=NCDF_CREATE(outfile,/CLOBBER)
   dimids=intarr(3)
   dimids(0)=NCDF_DIMDEF(id,'longitude',nlon)
   dimids(1)=NCDF_DIMDEF(id,'latitude',nlat)
   dimids(2)=NCDF_DIMDEF(id,'t',nt)
   varids=intarr(4)
   varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
   varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
   varids(2)=NCDF_VARDEF(id,'t',[dimids(2)])
   varids(3)=NCDF_VARDEF(id,varname+'_intrho',[dimids(0),dimids(1),dimids(2)])
   
   NCDF_CONTROL,id,/ENDEF
   NCDF_VARPUT,id,varids(0),lon
   NCDF_VARPUT,id,varids(1),lat
   NCDF_VARPUT,id,varids(2),findgen(nt)
   NCDF_VARPUT,id,varids(3),var_out
   NCDF_CLOSE,id

ENDFOR

STOP
END
