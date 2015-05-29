PRO hadgem3kpp_ocnTcorr_make_smooth_correction,infile,outfile,in_varname,in_zname,out_varname,n_days_per_year,mean_length

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
z=OPEN_AND_EXTRACT(infile,in_zname)
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_z=N_ELEMENTS(z)

sfcorr_in=OPEN_AND_EXTRACT(infile,in_varname)
n_days=N_ELEMENTS(REFORM(sfcorr_in(0,0,0,*)))
n_years=n_days/n_days_per_year
sfcorr_out=fltarr(n_lon,n_lat,n_z,n_days_per_year)

IF TOTAL(where(ABS(sfcorr_in) ge 1e10)) ge 0 THEN $
   sfcorr_in[where(ABS(sfcorr_in ge 1e10))]=!Values.F_NaN

FOR i=0,n_lon-1 DO BEGIN
   print,'Running for longitude band ',i,' of ',n_lon
   FOR j=0,n_lat-1 DO BEGIN
      IF sfcorr_in(i,j,0,0) ne 1e20 THEN BEGIN
    ;     IF i ge 1 and i le n_lon-2 and j ge 1 and j le n_lat-2 THEN BEGIN
    ;        FOR k=0,n_days-1 DO BEGIN
    ;           neighbours=REFORM([sfcorr_in(i-1,j,*,k),sfcorr_in(i+1,j,*,k),$
    ;                              sfcorr_in(i-1,j+1,*,k),sfcorr_in(i,j+1,*,k),sfcorr_in(i+1,j+1,*,k),$
    ;                              sfcorr_in(i-1,j-1,*,k),sfcorr_in(i,j-1,*,k),sfcorr_in(i,j-1,*,k)])
    ;           IF TOTAL(where(neighbours eq 1e20)) ge 0 THEN $
    ;              neighbours[where(neighbours eq 1e20)]=!Values.F_NaN
    ;           mean_neighbours=MEAN(neighbours(*,0),/NaN)
    ;           IF ABS(sfcorr_in(i,j,0,k)) gt ABS(5*mean_neighbours) and ABS(sfcorr_in(i,j,0,k)) ge 10 THEN BEGIN
    ;              print,'Triggered at i=',i,'j=',j,'time=',k,'because sfcorr_in =',sfcorr_in(i,j,0,k),$
    ;                    'and mean of neighbouring points is ',mean_neighbours
    ;              FOR m=0,n_z-1 DO $
    ;                 sfcorr_in(i,j,m,k)=MEAN(neighbours(*,m),/NaN)
    ;           ENDIF
    ;        ENDFOR               
    ;     ENDIF
         FOR k=0,n_z-1 DO BEGIN
            FOR m=0,n_days_per_year-1 DO $
               sfcorr_out(i,j,k,m)=MEAN(sfcorr_in(i,j,k,m:n_days-1:n_days_per_year),/NaN)
            temp=REFORM([REFORM(sfcorr_out(i,j,k,*)),REFORM(sfcorr_out(i,j,k,*))])
            temp=SMOOTH(temp,mean_length)
            sfcorr_out(i,j,k,*)=[temp(n_days_per_year:n_days_per_year+mean_length/2-1),$
                                 temp(mean_length/2:n_days_per_year-1)]
    ;	    print,sfcorr_out(i,j,k,14),temp(374),sfcorr_out(i,j,k,15)
         ENDFOR
      ENDIF ELSE $
         sfcorr_out(i,j,*,*)=1e20
   ENDFOR
ENDFOR

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(4)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'z',n_z)
dimids(3)=NCDF_DIMDEF(id,'t',n_days_per_year)
varids=intarr(5)
varids(0)=NCDF_VARDEF(id,'longitude',dimids(0))
varids(1)=NCDF_VARDEF(id,'latitude',dimids(1))
varids(2)=NCDF_VARDEF(id,'z',dimids(2))
varids(3)=NCDF_VARDEF(id,'t',dimids(3))
varids(4)=NCDF_VARDEF(id,out_varname,[dimids(0),dimids(1),dimids(2),dimids(3)])
;NCDF_CONTROL,id,/ENDEF
NCDF_ATTPUT,id,varids(4),'missing_value',1e20
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),z
NCDF_VARPUT,id,varids(3),indgen(n_days_per_year)+0.5
IF in_varname eq 'scorr' THEN BEGIN
   NCDF_VARPUT,id,varids(4),sfcorr_out
ENDIF ELSE IF in_varname eq 'fcorr_z' THEN BEGIN
   NCDF_VARPUT,id,varids(4),sfcorr_out ;/1024./4000.
ENDIF ELSE $
   NCDF_VARPUT,id,varids(4),sfcorr_out
NCDF_CLOSE,id

STOP
END

