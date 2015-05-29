PRO hadgem3kpp_cplwght_plot_overlays

dir='/home/ss901165/datasets/HADGEM3-KPP_ANCIL'
lsm_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'
files=[dir+'/cplwght_for_kpp.n96.5ptblend.50S-50N_0-360E.nc',$
       dir+'/cplwght_for_kpp.n96.5ptblend.extend_to_360E.nc',$
       dir+'/cplwght_for_kpp.n96.5ptblend.extend_to_200E.nc',$
       dir+'/cplwght_for_kpp.n96.5ptblend.extend_to_105E.nc',$
       dir+'/cplwght_for_kpp.n96.5ptblend.nrglobal.nc']
n_files=N_ELEMENTS(files)

mask=REFORM(OPEN_AND_EXTRACT(lsm_file,'lsm'))
FOR i=0,n_files-1 DO BEGIN
   IF i eq 0 THEN BEGIN
      longitude=OPEN_AND_EXTRACT(files(0),'longitude')
      latitude=OPEN_AND_EXTRACT(files(0),'latitude')
      n_lon=N_ELEMENTS(longitude)
      n_lat=N_ELEMENTS(latitude)
      total_cplwght=fltarr(n_lon,n_lat)
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/hadgem3kpp_cplwght_plot_overlays.ps'
      PSOPEN,file=psfile,FONT=6,CHARSIZE=150,TFONT=6,MARGIN=1500,XOFFSET=500
      MAP,LONMIN=0,LONMAX=360
   ENDIF
   this_cplwght=OPEN_AND_EXTRACT(files(i),'alpha')
   this_cplwght[where(mask eq 1)]=!Values.F_NaN
   this_cplwght[where(this_cplwght gt 0)]=1
   total_cplwght=total_cplwght+this_cplwght
ENDFOR
total_cplwght[where(this_cplwght eq 0)]=0
CS,SCALE=28,white=[2],NCOLS=6
LEVS,MANUAL=['1','2','3','4','5']
CON,X=longitude,Y=latitude,FIELD=FLOOR(total_cplwght),/BLOCK,/NOLINES,/NOLINELABELS,/NOCOLBAR
AXES
PSCLOSE

STOP
END
