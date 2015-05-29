PRO qld_iacorr_bypoint_silon144_makemask

; Give a SILO n144 file for latitude and longitude coorindates

qld_box=[-30,138,-10,154]
silo_infile='/home/ss901165/datasets_mango/SILO/n144/SILO_precip.may-apr_ameans.1900-2007.n144.nc'
silo_longitude=OPEN_AND_EXTRACT(silo_infile,'longitude')
silo_latitude=OPEN_AND_EXTRACT(silo_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,silo_latitude,silo_longitude,qld_box_tx,/LIMIT
silo_nlon=N_ELEMENTS(silo_longitude)
silo_nlat=N_ELEMENTS(silo_latitude)

mask_infile='/home/ss901165/um_output/mask_n144_higam.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,qld_box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)

mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[qld_box_tx(1),qld_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
mask_latrev=fltarr(mask_nlon,mask_nlat)
FOR i=0,mask_nlat-1 DO $
   mask_latrev(*,i)=mask(*,mask_nlat-i-1)

silo_precip=REFORM(OPEN_AND_EXTRACT(silo_infile,'rain',$
                             offset=[qld_box_tx(1),qld_box_tx(0),0],$
                             count=[silo_nlon,silo_nlat,1]))

n_regions=5
silo_region_masks=intarr(n_regions,silo_nlon,silo_nlat)
silo_regions=intarr(silo_nlon,silo_nlat)

FOR i=0,silo_nlon-1 DO BEGIN
   FOR j=0,silo_nlat-1 DO BEGIN
      IF silo_precip(i,j) lt 10000 and mask_latrev(i,j) eq 1.0 THEN BEGIN
         IF silo_latitude(j) gt -17.4 THEN BEGIN
            silo_regions(i,j)=1
            print,silo_latitude(j),' -> region 1'
         ENDIF ELSE IF silo_latitude(j) ge -20 and silo_longitude(i) lt 145 THEN BEGIN
            silo_regions(i,j)=2
         ENDIF ELSE IF silo_latitude(j) ge -20 and silo_longitude(i) ge 145 THEN BEGIN
            silo_regions(i,j)=3
         ENDIF ELSE IF silo_latitude(j) ge -22.5 and silo_longitude(i) lt 145 THEN BEGIN
            silo_regions(i,j)=2
         ENDIF ELSE IF silo_latitude(j) ge -22.5 and silo_longitude(i) ge 145 THEN BEGIN
            silo_regions(i,j)=3
         ENDIF ELSE IF silo_latitude(j) ge -25 and silo_longitude(i) le 140 THEN BEGIN
            silo_regions(i,j)=2
         ENDIF ELSE IF silo_latitude(j) ge -25 and silo_longitude(i) ge 140 and silo_longitude(i) lt 145 THEN BEGIN
            silo_regions(i,j)=4
         ENDIF ELSE IF silo_latitude(j) ge -25 and silo_longitude(i) ge 145 THEN BEGIN
            silo_regions(i,j)=3
         ENDIF ELSE IF silo_latitude(j) ge -30 and silo_longitude(i) le 147.5 THEN BEGIN
            silo_regions(i,j)=4
            print,silo_latitude(j),silo_longitude(i),' -> region 4'
         ENDIF ELSE IF silo_latitude(j) ge -30 and silo_latitude(j) le -27.5 and silo_longitude(i) ge 147.5 and silo_longitude(i) lt 150 THEN BEGIN
            silo_regions(i,j)=4
         ENDIF ELSE IF silo_latitude(j) ge -30 and silo_longitude(i) ge 150 THEN BEGIN
            silo_regions(i,j)=5
         ENDIF ELSE IF silo_latitude(j) ge -30 THEN BEGIN
            silo_regions(i,j)=5
         ENDIF
      ENDIF
   ENDFOR
ENDFOR

mylevs=['0.5','1.5','2.5','3.5','4.5','5.5']

psfile='/home/ss901165/idl/queensland/interannual_correlations/qld_iacorr_bypoint_silon144.region_map.may-apr.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=130,MARGIN=1000,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,/PORTRAIT,CB_WIDTH=112
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1,/REV
MAP,LONMIN=138,LONMAX=153.75,LATMIN=qld_box(0),LATMAX=qld_box(2),/hires
LEVS,MANUAL=mylevs
CON,FIELD=FLOAT(silo_regions),X=silo_longitude,Y=silo_latitude,$
    TITLE='Region map for SILO n144',/NOLINES,/BLOCK
PSCLOSE

mask_outfile='/home/ss901165/datasets_mango/SILO/n144/SILO_qld_region_mask.n144.may-apr.nc'
id=NCDF_CREATE(mask_outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'longitude',silo_nlon)
dimids(1)=NCDF_DIMDEF(id,'latitude',silo_nlat)
dimids(2)=NCDF_DIMDEF(id,'region',n_regions)
varids=intarr(4+n_regions)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'region',[dimids(2)],/SHORT)
varids(3)=NCDF_VARDEF(id,'silo_regions',[dimids(0),dimids(1)],/SHORT)
count=3
FOR i=0,n_regions-1 DO $
   varids(count+i+1)=NCDF_VARDEF(id,'mask_region'+STRTRIM(STRING(i+1),1),[dimids(0),dimids(1)],/SHORT)

NCDF_CONTROL,id,/ENDEF
 
NCDF_VARPUT,id,varids(0),silo_longitude
NCDF_VARPUT,id,varids(1),silo_latitude
NCDF_VARPUT,id,varids(2),indgen(n_regions)+1
NCDF_VARPUT,id,varids(3),silo_regions
FOR i=0,n_regions-1 DO BEGIN
   this_mask=fltarr(silo_nlon,silo_nlat)
   this_mask[where(silo_regions eq i+1)]=1
   this_mask[where(silo_regions ne i+1)]=0
   NCDF_VARPUT,id,varids(count+i+1),this_mask
ENDFOR

NCDF_CLOSE,id

STOP
END
