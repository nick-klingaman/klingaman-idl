PRO hadgem3kpp_fcorr_compare_ind_rerun_vinttemp

kpp_times=['0045','0075','0105','0135','0165','0195','0225','0255','0285','0315','0345','0375','0405']
kpp_runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
;months=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15','jul15-aug15',$
;'aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15','jan15-feb15']

n_times=N_ELEMENTS(kpp_times)
n_runs=N_ELEMENTS(kpp_runs)

ind_indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'
ensmean_indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind_rerun'

box=[-30,40,30,200]

mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))
n_lon=mask_nlon
n_lat=mask_nlat

vint_start=[0,20,40,60,80,100,120,140,160,180,0]
vint_stop=[20,40,60,80,100,120,140,160,180,200,200]
ymin=[24,24,24,24,20,20,20,16,16,16,20]
ymax=[28,28,28,28,24,24,24,20,20,20,24]
;vint_start=[100,120,140,160,180]
;vint_stop=[120,140,160,180,200]
;vint_start=[0]
;vint_stop=[200]
n_vint=N_ELEMENTS(vint_start)

ind_vint=fltarr(n_runs,n_times,n_vint)
ensmean_vint=fltarr(n_runs,n_times,n_vint)

FOR i=0,n_runs-1 DO BEGIN
   print,'---> '+kpp_runs(i)
   FOR j=0,n_times-1 DO BEGIN
      ind_infile=ind_indir+'/'+kpp_runs(i)+'/KPPocean_'+kpp_times(j)+'_means.nc'
      ensmean_infile=ensmean_indir+'/'+kpp_runs(i)+'/KPPocean_'+kpp_times(j)+'_means.nc'

      IF i eq 0 and j eq 0 THEN BEGIN
         z=OPEN_AND_EXTRACT(ind_infile,'z')
         n_z=N_ELEMENTS(z)
      ENDIF

      this_ind_T=REFORM(OPEN_AND_EXTRACT(ind_infile,'T'))
      this_ensmean_T=REFORM(OPEN_AND_EXTRACT(ensmean_infile,'T'))
      
      this_ind_T_mmean=fltarr(n_lon,n_lat,n_z)
      this_ensmean_T_mmean=fltarr(n_lon,n_lat,n_z)
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_lat-1 DO BEGIN
            FOR n=0,n_z-1 DO BEGIN
               this_ind_T_mmean(k,m,n)=this_ind_T(k,m,n,*)
               this_ensmean_T_mmean(k,m,n)=this_ensmean_T(k,m,n,*)
            ENDFOR
         ENDFOR
      ENDFOR

      FOR k=0,n_vint-1 DO BEGIN
         z_start=NEAREST(ABS(z),ABS(vint_start(k)))
         z_stop=NEAREST(ABS(z),ABS(vint_stop(k)))
       ;  print,z(z_start),z(z_stop)
         
         dz_total=0
         FOR m=0,n_lon-1 DO BEGIN
            FOR n=0,n_lat-1 DO BEGIN
               IF this_ind_T_mmean(m,n,0) le 1e10 THEN BEGIN
                  FOR p=z_start,z_stop DO BEGIN
                     IF p eq 0 THEN BEGIN
                        dz=2*ABS(z(p))
                     ENDIF ELSE $
                        dz=ABS(z(p))-ABS(z(p-1))
                     ind_vint(i,j,k)=this_ind_T_mmean(m,n,p)*dz+ind_vint(i,j,k)
                     ensmean_vint(i,j,k)=this_ensmean_T_mmean(m,n,p)*dz+ensmean_vint(i,j,k)
                     dz_total=dz_total+dz
                  ENDFOR
               ENDIF
            ENDFOR
         ENDFOR
         ind_vint(i,j,k)=ind_vint(i,j,k)/dz_total
         ensmean_vint(i,j,k)=ensmean_vint(i,j,k)/dz_total
      EndFOR
   ENDFOR
ENDFOR

FOR k=0,n_vint-1 DO BEGIN
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun_vinttemp.vint'+STRTRIM(STRING(vint_start(k)),1)+'-'+$
          STRTRIM(STRING(vint_stop(k)),1)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,SPACE3=500
   GSET,XMIN=0,XMAX=n_times,YMIN=ymin(k),YMAX=ymax(k)
   colors=['purple','blue','cyan','orange','red','pink','brown','black','green','dark green','steel blue']
   FOR i=0,n_runs-1 DO BEGIN
      GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_vint(i,*,k)),COL=FSC_COLOR(colors(i)),SYM=4,/NOLINES
      GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_vint(i,*,k)),COL=FSC_COLOR(colors(i)),SYM=5,/NOLINES  
   ENDFOR
   ind_vint_mean=fltarr(n_times)
   ensmean_vint_mean=fltarr(n_times)
   FOR i=0,n_times-1 DO BEGIN
      ind_vint_mean(i)=MEAN(ind_vint(*,i,k))
      ensmean_vint_mean(i)=MEAN(ensmean_vint(*,i,k))
   ENDFOR
   GPLOT,X=indgen(n_times)+0.5,Y=ind_vint_mean,COL=FSC_COLOR('violet red')
   GPLOT,X=indgen(n_times)+0.5,Y=ensmean_vint_mean,COL=FSC_COLOR('dark grey')
   AXES,XVALS=indgen(n_times)+0.5,XLABELS=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','jan'],$
        YSTEP=0.2,YMINOR=0.1,XTITLE='Month',YTITLE='Domain-mean Vertically averaged temperature (K)'
   PSCLOSE
ENDFOR

STOP
END
