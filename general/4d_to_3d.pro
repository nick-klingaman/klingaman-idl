PRO fourd_to_threed,infile,x_name,y_name,varname,outfile

x=OPEN_AND_EXTRACT(infile,x_name)
y=OPEN_AND_EXTRACT(infile,y_name)
nx=N_ELEMENTS(x)
ny=N_ELEMENTS(y)
var=OPEN_AND_EXTRACT(infile,varname)
nt=N_ELEMENTS(var(0,0,*,0))
nyr=N_ELEMENTS(var(0,0,0,*))
var_out=fltarr(nx,ny,nt*nyr)
IF nt lt nyr THEN BEGIN
   FOR i=0,nt-1 DO $
      var_out(*,*,i*nyr:(i+1)*nyr-1)=var(*,*,i,*)
ENDIF ELSE $
   FOR i=0,nyr-1 DO $
      var_out(*,*,i*nt:(i+1)*nt-1)=var(*,*,*,i)

id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,x_name,nx)
dimids(1)=NCDF_DIMDEF(id,y_name,ny)
dimids(2)=NCDF_DIMDEF(id,'t',nt*nyr)
varids=intarr(4)
varids(0)=NCDF_VARDEF(id,x_name,[dimids(0)])
varids(1)=NCDF_VARDEF(id,y_name,[dimids(1)])
varids(2)=NCDF_VARDEF(id,'t',[dimids(2)])
varids(3)=NCDF_VARDEF(id,varname,[dimids(0),dimids(1),dimids(2)])
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),x
NCDF_VARPUT,id,varids(1),y
NCDF_VARPUT,id,varids(2),findgen(nt*nyr)+0.5
NCDF_VARPUT,id,varids(3),var_out
NCDF_CLOSE,id

STOP
END
