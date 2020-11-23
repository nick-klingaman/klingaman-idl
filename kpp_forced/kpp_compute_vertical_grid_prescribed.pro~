PRO kpp_compute_vertical_grid_prescribed

hires_depth=300
max_depth=1000
dscale=3.5
hires_levels=72

dfac=1.0-exp(-1.0*dscale)
sk=-(findgen(hires_levels)-0.5)/float(hires_levels)
hm=hires_depth*dfac/float(hires_levels)/(-1.0*dscale)/(1.0+sk*dfac)

sumh=TOTAL(hm)
zm=fltarr(hires_levels)
dm=fltarr(hires_levels)
hsum=0
FOR i=0,hires_levels-1 DO BEGIN
   hm(i)=hm(i)*hires_depth/sumh
   zm(i)=0.0-(hsum+0.5*hm(i))
   hsum=hsum+hm(i)
   dm(i)=hsum
ENDFOR
depth=dm(hires_levels-1)
WHILE depth lt max_depth-hm(hires_levels-1) DO BEGIN
   hm=[hm,hm(hires_levels-1)]
   zm=[zm,0.0-(hsum+0.5*hm(i))]
   hsum=hsum+hm(i)
   dm=[dm,hsum]
   depth=dm(i)
   i=i+1
ENDWHILE

;set_plot,'x'
;plot,hm,-1*zm,psym=6,/xlog,/ylog,yrange=[1,max_depth],xrange=[1,150],ystyle=1,xstyle=1

outfile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/kpp_vertical_grid.1000m.nc'
id=NCDF_CREATE(outfile,/CLOBBER)
dimids=intarr(3)
dimids(0)=NCDF_DIMDEF(id,'d',N_ELEMENTS(dm))
dimids(1)=NCDF_DIMDEF(id,'z',N_ELEMENTS(zm))
dimids(2)=NCDF_DIMDEF(id,'h',N_ELEMENTS(hm))
varids=intarr(3)
varids(0)=NCDF_VARDEF(id,'d',dimids(0))
varids(1)=NCDF_VARDEF(id,'z',dimids(1))
varids(2)=NCDF_VARDEF(id,'h',dimids(2))
NCDF_CONTROL,id,/ENDEF
NCDF_VARPUT,id,varids(0),dm
NCDF_VARPUT,id,varids(1),zm
NCDF_VARPUT,id,varids(2),hm

NCDF_CLOSE,id

STOP
END
