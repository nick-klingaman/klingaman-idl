PRO kpp_scm_isothermal_reset_flag

;infile='/home/ss901165/um_output6/kpp_ocean/3D_kpp_r4b2/test_run_1000m/comp_flag.nc'
infile='/home/ss901165/um_output6/xihvh/flags.nc'

longitude=OPEN_AND_EXTRACT(infile,'longitude')
latitude=OPEN_AND_EXTRACT(infile,'latitude')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)

comp_flag=OPEN_AND_EXTRACT(infile,'comp_flag')
n_time=N_ELEMENTS(comp_flag(0,0,*))

n_reints=fltarr(n_lon,n_lat)
n_resets=fltarr(n_lon,n_lat)
reset_time=fltarr(n_lon,n_lat)
FOR i=0,n_lon-1 DO BEGIN
   FOR j=0,n_lat-1 DO BEGIN
      temp=REFORM(comp_flag(i,j,*))
      IF temp(0) ne 1e20 THEN BEGIN
         IF TOTAL(where(temp lt (23+999)/24. and temp gt 1)) ge 0 THEN $
            n_reints(i,j)=N_ELEMENTS(where(temp lt (23+999)/24. and temp gt 1))
         IF TOTAL(where(temp ge (23+999)/24.)) ge 0 THEN BEGIN
            n_resets(i,j)=N_ELEMENTS(where(temp ge (23+999)/24.))
            reset_time(i,j)=MIN(where(temp ge (23+999)/24.));/24.
         ENDIF ELSE $
            reset_time(i,j)=!Values.F_NaN
      ENDIF ELSE BEGIN
         n_reints(i,j)=!Values.F_NaN
         n_resets(i,j)=!Values.F_NaN
         reset_time(i,j)=!Values.F_NaN
      ENDELSE
   ENDFOR
ENDFOR
n_resetpts=fltarr(n_time)
FOR i=LONG(0),LONG(n_time)-1 DO $
   IF TOTAL(where(comp_flag(*,*,i) ge (23+999)/24. and comp_flag(*,*,i) lt 1e10)) ge 0 THEN $
      n_resetpts(i)=N_ELEMENTS(where(comp_flag(*,*,i) ge (23+999)/24. and comp_flag(*,*,i) lt 1e10))
n_reintpts=fltarr(n_time)
FOR i=LONG(0),LONG(n_time)-1 DO $
   IF TOTAL(where(comp_flag(*,*,i) gt 1 and comp_flag(*,*,i) lt (23+999)/24.)) gt 0 THEN $
      n_reintpts(i)=N_ELEMENTS(where(comp_flag(*,*,i) gt 1 and comp_flag(*,*,i) lt (23+999)/24.))

mylevs=['1','2','3','4','5','6','7','8','9','10','11','12']
psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_isothermal_reset_flag.xihvh.n_reints.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,MARGIN=2000
MAP,LONMIN=0,LONMAX=360,LATMIN=-50,LATMAX=50,/hires
CS,SCALE=28,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=n_reints,/BLOCK,/NOLINES,$
    TITLE='Number of timesteps in which the point was re-integrated successfully'
AXES
PSCLOSE

psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_isothermal_reset_flag.xihvh.n_resets.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,MARGIN=2000
MAP,LONMIN=0,LONMAX=360,LATMIN=-50,LATMAX=50,/hires
CS,SCALE=28,NCOLS=N_ELEMENTS(mylevs)+1,white=[2]
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=n_resets,/BLOCK,/NOLINES,$
    TITLE='Number of timesteps in which the gridpoint was reset to climatology'
AXES
PSCLOSE

mylevs=[100,400,700,1000,1300,1600,1900,2200,2500,2800,3100,3400,3700,4000,4300,4600,4900,5200,5500,5800,6100]
psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_isothermal_reset_flag.xihvh.reset_time.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,MARGIN=2000
MAP,LONMIN=0,LONMAX=360,LATMIN=-50,LATMAX=50,/hires
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs)+1
LEVS,MANUAL=mylevs
CON,X=longitude,Y=latitude,FIELD=reset_time,/BLOCK,/NOLINES,$
    TITLE='Day of first reset to climatology'
AXES
PSCLOSE

psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_isothermal_reset_flag.xihvh.n_resetpts.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,MARGIN=2000
GSET,XMIN=0,XMAX=n_time,YMIN=0,YMAX=6
GPLOT,X=findgen(n_time),Y=n_resetpts
AXES,XSTEP=500,YSTEP=1,YMINOR=0.5,XMINOR=250,$
     XTITLE='Time (days)',YTITLE='Number of points reset on this day'
PSCLOSE 

psfile='/home/ss901165/idl/kpp_forced/scm_tests/kpp_scm_isothermal_reint_flag.xihvh.n_reintpts.ps'
PSOPEN,file=psfile,FONT=2,TFONT=2,MARGIN=2000
GSET,XMIN=0,XMAX=n_time,YMIN=0,YMAX=4
GPLOT,X=findgen(n_time),Y=n_reintpts
AXES,XSTEP=500,YSTEP=1,YMINOR=0.5,XMINOR=250,$
     XTITLE='Time (days)',YTITLE='Number of points re-integrated on this day'
PSCLOSE 

STOP
END

