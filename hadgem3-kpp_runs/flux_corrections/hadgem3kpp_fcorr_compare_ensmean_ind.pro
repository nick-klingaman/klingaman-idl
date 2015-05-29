PRO hadgem3kpp_fcorr_compare_ensmean_ind
  
; Compare heat corrections computed by the "ensemble-mean" and "individual" methods.

ensmean_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421.jan-dec_mmeans.flxcorr.n96.nc'
ind_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'

n_months=3
;month_names=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov']
;kpp_times=['0045','0075','0105','0135','0165','0195','0225','0255','0285','0315','0345']
;month_offsets=[0,1,2,3,4,5,6,7]

;month_names=['sep','oct','nov']
;kpp_times=['0285','0315','0345']
;month_offsets=[8,9,10]

n_days_per_file=30

month_names=['dec']
kpp_times=['0375']
month_offsets=[11]

runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
n_runs=N_ELEMENTS(runs)
ensmean_indir='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421'
ind_indir='/home/ss901165/kpp_ocean3/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'

longitude=OPEN_AND_EXTRACT(ensmean_fcorr_file,'longitude')
latitude=OPEN_AND_EXTRACT(ensmean_fcorr_file,'latitude')
z=OPEN_AND_EXTRACT(ensmean_fcorr_file,'z')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_z=N_ELEMENTS(z)

mylevs_fcorr_surface=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
mylevs_fcorr_vint=['-475','-425','-375','-325','-275','-225','-175','-125','-75','-25','25','75','125','175','225','275','325','375','425','475']
mylevs_fcorr_surface_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
mylevs_fcorr_vint_diff=['-38','-34','-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30','34','38']

mylevs_tinc_vintml=['-3.8','-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4','3.8']
mylevs_tinc_vintml_diff=['-0.95','-0.85','-0.75','-0.65','-0.55','-0.45','-0.35','-0.25','-0.15','-0.05','0.05','0.15','0.25','0.35','0.45','0.55','0.65','0.75','0.85','0.95']

FOR i=0,n_months-1 DO BEGIN
   print,month_names(i)
                                ; Read corrections
   ensmean_fcorr=REFORM(OPEN_AND_EXTRACT(ensmean_fcorr_file,'fcorr',$
                                         offset=[0,0,0,month_offsets(i)],$
                                         count=[n_lon,n_lat,n_z,1]))
   ensmean_expcorr=REFORM(OPEN_AND_EXTRACT(ensmean_fcorr_file,'expcorr',$
                                           offset=[0,0,0,month_offsets(i)],$
                                           count=[n_lon,n_lat,n_z,1]))
   ind_fcorr=REFORM(OPEN_AND_EXTRACT(ind_fcorr_file,'fcorr',$
                                     offset=[0,0,0,month_offsets(i)],$
                                     count=[n_lon,n_lat,n_z,1]))

   ensmean_fcorr_sfc=REFORM(ensmean_fcorr(*,*,0))
   ind_fcorr_sfc=REFORM(ind_fcorr(*,*,0))
   diff_fcorr_sfc=ind_fcorr_sfc-ensmean_fcorr_sfc

   ensmean_fcorr_vint=fltarr(n_lon,n_lat)
   ind_fcorr_vint=fltarr(n_lon,n_lat)
   FOR j=0,n_z-1 DO BEGIN
      IF j eq 0 THEN BEGIN
         dz=2*z(0)
      ENDIF ELSE $
         dz=z(j)-z(j-1)
      FOR k=0,n_lon-1 DO BEGIN
         FOR m=0,n_lat-1 DO BEGIN
            ensmean_fcorr_vint(k,m)=ensmean_fcorr_vint(k,m)+dz*ensmean_fcorr(k,m,j)
            ind_fcorr_vint(k,m)=ind_fcorr_vint(k,m)+dz*ind_fcorr(k,m,j)
         ENDFOR
      ENDFOR
   ENDFOR
   diff_fcorr_vint=ind_fcorr_vint-ensmean_fcorr_vint
   
   ensmean_expcorr_hmixint=fltarr(n_lon,n_lat)
   ind_expcorr_hmixint=fltarr(n_lon,n_lat)
   FOR j=0,n_runs-1 DO BEGIN

      ind_fcorr_indrun_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+runs(j)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
      ind_expcorr=REFORM(OPEN_AND_EXTRACT(ind_fcorr_indrun_file,'expcorr',$
                                       offset=[0,0,0,month_offsets(i)],$
                                       count=[n_lon,n_lat,n_z,1]))

      ensmean_infile=ensmean_indir+'/'+runs(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'
      ind_infile=ind_indir+'/'+runs(j)+'/KPPocean_'+kpp_times(i)+'_means.nc'

      ensmean_hmix=OPEN_AND_EXTRACT(ensmean_infile,'hmix',$
                                    offset=[0,0,0],count=[n_lon,n_lat,n_days_per_file])
      ind_hmix=OPEN_AND_EXTRACT(ind_infile,'hmix',$
                                offset=[0,0,0],count=[n_lon,n_lat,n_days_per_file])

      
      FOR k=0,n_days_per_file-1 DO BEGIN
         FOR m=0,n_lon-1 DO BEGIN
            FOR n=0,n_lat-1 DO BEGIN
               FOR p=0,n_z-1 DO BEGIN
                  IF ABS(z(p)) le ABS(ensmean_hmix(m,n,k)) THEN BEGIN
                     IF p eq 0 THEN BEGIN
                        dz=2*z(0)
                     ENDIF ELSE $
                        dz=z(p)-z(p-1)
                     ensmean_expcorr_hmixint(m,n)=ensmean_expcorr_hmixint(m,n)+$
                                                  ensmean_expcorr(m,n,p)*dz/15.
                  ENDIF
                  IF ABS(z(p)) le ABS(ind_hmix(m,n,k)) THEN BEGIN
                     IF p eq 0 THEN BEGIN
                        dz=2*z(0)
                     ENDIF ELSE $
                        dz=z(p)-z(p-1)
                     ind_expcorr_hmixint(m,n)=ind_expcorr_hmixint(m,n)+$
                                              ind_expcorr(m,n,p)*dz/15.
                  ENDIF
               ENDFOR
            ENDFOR
         ENDFOR
      ENDFOR
   ENDFOR
   ensmean_expcorr_hmixint=ensmean_expcorr_hmixint/FLOAT(n_runs*n_days_per_file)
   ind_expcorr_hmixint=ind_expcorr_hmixint/FLOAT(n_runs*n_days_per_file)
   diff_expcorr_hmixint=ind_expcorr_hmixint-ensmean_expcorr_hmixint

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ensmean_fcorr_sfc.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ensmean_fcorr_sfc,$
       TITLE='Ensmean sfc fcorr from runs using ensmean fcorr for '+month_names(i),CB_TITLE='W m!U-3!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind_fcorr_sfc.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ind_fcorr_sfc,$
       TITLE='Ensmean sfc fcorr from runs using individual fcorr for '+month_names(i),CB_TITLE='W m!U-3!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind-minus-ensmean_fcorr_sfc.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_surface_diff)+1,white=[10]
   LEVS,MANUAL=mylevs_fcorr_surface_diff
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=diff_fcorr_sfc,$
       TITLE='Diff in sfc fcorr from runs using ind and ensmean fcorr for '+month_names(i),CB_TITLE='W m!U-3!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ensmean_fcorr_vint.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_vint)+1,white=[12]
   LEVS,MANUAL=mylevs_fcorr_vint
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ensmean_fcorr_vint,$
       TITLE='Ensmean vint fcorr from runs using ensmean fcorr for '+month_names(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind_fcorr_vint.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_vint)+1,white=[12]
   LEVS,MANUAL=mylevs_fcorr_vint
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ind_fcorr_vint,$
       TITLE='Ensmean vint fcorr from runs using individual fcorr for '+month_names(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind-minus-ensmean_fcorr_vint.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_vint_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_fcorr_vint_diff
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=diff_fcorr_vint,$
       TITLE='Diff in vint fcorr from runs using ind and ensmean fcorr for '+month_names(i),CB_TITLE='W m!U-2!N',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ensmean_tinc_vintml.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_fcorr_vint)+1,white=[12]
   LEVS,MANUAL=mylevs_tinc_vintml
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ensmean_expcorr_hmixint,$
       TITLE='Ensmean Tinc from ensmean fcorr vint over mldepth for '+month_names(i),CB_TITLE='K/month',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind_tinc_vintml.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_tinc_vintml)+1,white=[12]
   LEVS,MANUAL=mylevs_tinc_vintml
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=ind_expcorr_hmixint,$
       TITLE='Ensmean Tinc from ind fcorr vint over mldepth for '+month_names(i),CB_TITLE='K/month',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ensmean_ind.ind-minus-ensmean_tinc_vintml.'+month_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_tinc_vintml_diff)+1,white=[12]
   LEVS,MANUAL=mylevs_tinc_vintml_diff
   MAP,LONMIN=MIN(longitude),LATMIN=MIN(latitude),LONMAX=MAX(longitude),LATMAX=MAX(latitude)
   CON,X=longitude,Y=latitude,FIELD=diff_expcorr_hmixint,$
       TITLE='Diff in ensmean Tinc vint over mldepth, ind minus ensmean for '+month_names(i),CB_TITLE='K/month',$
       /BLOCK,/NOLINES
   PSCLOSE,/NOVIEW

ENDFOR

STOP
END
