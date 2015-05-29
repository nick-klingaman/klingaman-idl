PRO hadgem3kpp_fcorr_compare_ind_rerun

kpp_times=['0045','0075','0105','0135','0165','0195','0225','0255','0285','0315','0345','0375','0405']
kpp_runs=['xgspj_i2','xgspj_i3','xgspj_i4','xgspj_i5','xgspj_i6','xgspj_i7','xgspj_i8','xgspj_i9','xgspj_j0','xgspj_j1']
months=['jan15-feb15','feb15-mar15','mar15-apr15','apr15-may15','may15-jun15','jun15-jul15','jul15-aug15',$
        'aug15-sep15','sep15-oct15','oct15-nov15','nov15-dec15','dec15-jan15','jan15-feb15']

ind_indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind'
ensmean_indir='/export/niagara/data-02/ss901165/kpp_ocean/runs/flxcorr_hadgem3_1.5xentrain_n96_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind_rerun'
obs_indir='/home/ss901165/datasets/METO_OCEAN_ANALYSIS/n96'

ensmean_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
;ind_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/hadgem3_1.5xentrain_ensmean_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'

n_times=N_ELEMENTS(kpp_times)
n_runs=N_ELEMENTS(kpp_runs)

box=[-30,40,30,200]
;levels_diff=['-1.7','-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
levels_diff=['-3.4','-3.0','-2.6','-2.2','-1.8','-1.4','-1.0','-0.6','-0.2','0.2','0.6','1.0','1.4','1.8','2.2','2.6','3.0','3.4']
hmix_levels_diff=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
;vintml_levels_diff=['-1.7','-1.5','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5','1.7']
vintml_levels_diff=['-68','-60','-52','-44','-36','-28','-20','-12','-4','4','12','20','28','36','44','52','60','68']
;vintml_levels_diff=['-34','-30','-26','-22','-18','-14','-10','-6','-2','2','6','10','14','18','22','26','30','34']

mask_infile='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/mask_n96_hadgem3-7.3.nc'
mask_longitude=OPEN_AND_EXTRACT(mask_infile,'longitude')
mask_latitude=OPEN_AND_EXTRACT(mask_infile,'latitude')
DEFINE_BOUNDARIES,box,mask_latitude,mask_longitude,mask_box_tx,/LIMIT
mask_nlon=N_ELEMENTS(mask_longitude)
mask_nlat=N_ELEMENTS(mask_latitude)
mask=REFORM(OPEN_AND_EXTRACT(mask_infile,'lsm',$
                             offset=[mask_box_tx(1),mask_box_tx(0),0,0],$
                             count=[mask_nlon,mask_nlat,1,1]))

obs_sst_aavg=fltarr(n_times)
FOR i=0,n_times-1 DO BEGIN
   obs_infile=obs_indir+'/meto_ocean_analysis.'+months(i)+'_mmean_clim.1980-2009.pot_temp.amip_type.n96.nc'
   IF i eq 0 THEN BEGIN
      obs_longitude=OPEN_AND_EXTRACT(obs_infile,'longitude')
      obs_latitude=OPEN_AND_EXTRACT(obs_infile,'latitude')
      DEFINE_BOUNDARIES,box,obs_latitude,obs_longitude,obs_box_tx,/LIMIT
      obs_nlon=N_ELEMENTS(obs_longitude)
      obs_nlat=N_ELEMENTS(obs_latitude)
      obs_ssts=fltarr(obs_nlon,obs_nlat,n_times)
   ENDIF

   this_obs_sst=REFORM(OPEN_AND_EXTRACT(obs_infile,'temp',$
                                        offset=[obs_box_tx(1),obs_box_tx(0),0],$
                                        count=[obs_nlon,obs_nlat,1]))+273.15
   this_obs_sst[where(mask eq 1)]=!Values.F_NaN
   obs_ssts(*,*,i)=this_obs_sst
   obs_sst_aavg(i)=MEAN(this_obs_sst,/NaN)   
ENDFOR

ind_sst_aavg=fltarr(n_runs,n_times)
ind_hmix_aavg=fltarr(n_runs,n_times)
ind_mlcorr_aavg=fltarr(n_runs,n_times)
ensmean_sst_aavg=fltarr(n_runs,n_times)
ensmean_hmix_aavg=fltarr(n_runs,n_times)
ensmean_mlcorr_aavg=fltarr(n_runs,n_times)
FOR i=0,n_runs-1 DO BEGIN
   print,'---> ',kpp_runs(i)
   ;ind_fcorr_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/heat_corrections/'+kpp_runs(i)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
   ind_fcorr_file='/export/niagara/data-02/ss901165/kpp_ocean/ancillaries/heat_corrections/'+kpp_runs(i)+'_flxcorr_1mtop_3hr_ukmo_sal60_amip_vn78_smooth_12421_ind.jan-dec_mmeans.flxcorr.n96.nc'
   FOR j=0,n_times-1 DO BEGIN
      print,'-------> ',kpp_times(j)
      ind_infile=ind_indir+'/'+kpp_runs(i)+'/KPPocean_'+kpp_times(j)+'_means.nc'
      ensmean_infile=ensmean_indir+'/'+kpp_runs(i)+'/KPPocean_'+kpp_times(j)+'_means.nc'

      IF j eq 0 THEN BEGIN
         ind_longitude=OPEN_AND_EXTRACT(ind_infile,'longitude')
         ind_latitude=OPEN_AND_EXTRACT(ind_infile,'latitude')
         ind_z=OPEN_AND_EXTRACT(ind_infile,'z')
         DEFINE_BOUNDARIES,box,ind_latitude,ind_longitude,ind_box_tx,/LIMIT
         ind_nlon=N_ELEMENTS(ind_longitude)
         ind_nlat=N_ELEMENTS(ind_latitude)
         ind_nz=N_ELEMENTS(ind_z)
         ind_sst=fltarr(ind_nlon,ind_nlat,n_times)
         ind_hmix=fltarr(ind_nlon,ind_nlat,n_times)
         ind_mlcorr=fltarr(ind_nlon,ind_nlat,n_times)
         ensmean_sst=fltarr(ind_nlon,ind_nlat,n_times)
         ensmean_hmix=fltarr(ind_nlon,ind_nlat,n_times)
         ensmean_mlcorr=fltarr(ind_nlon,ind_nlat,n_times)
      ENDIF
      
      this_ind_sst=REFORM(OPEN_AND_EXTRACT(ind_infile,'T',$
                                           offset=[ind_box_tx(1),ind_box_tx(0),0,0],$
                                           count=[ind_nlon,ind_nlat,1,30]))+273.15
      this_ind_hmix=REFORM(OPEN_AND_EXTRACT(ind_infile,'hmix',$
                                            offset=[ind_box_tx(1),ind_box_tx(0),0],$
                                            count=[ind_nlon,ind_nlat,30]))
      this_ind_fcorr=REFORM(OPEN_AND_EXTRACT(ind_fcorr_file,'fcorr',$
                                            offset=[ind_box_tx(1),ind_box_tx(0),0,j],$
                                            count=[ind_nlon,ind_nlat,ind_nz,1]))
      this_ensmean_sst=REFORM(OPEN_AND_EXTRACT(ensmean_infile,'T',$
                                           offset=[ind_box_tx(1),ind_box_tx(0),0,0],$
                                           count=[ind_nlon,ind_nlat,1,30]))+273.15
      this_ensmean_hmix=REFORM(OPEN_AND_EXTRACT(ensmean_infile,'hmix',$
                                            offset=[ind_box_tx(1),ind_box_tx(0),0],$
                                            count=[ind_nlon,ind_nlat,30]))      
      this_ensmean_fcorr=REFORM(OPEN_AND_EXTRACT(ensmean_fcorr_file,'fcorr',$
                                                offset=[ind_box_tx(1),ind_box_tx(0),0,j],$
                                                count=[ind_nlon,ind_nlat,ind_nz,1]))
      this_ensmean_mlcorr=fltarr(ind_nlon,ind_nlat,30)
      this_ind_mlcorr=fltarr(ind_nlon,ind_nlat,30)
      FOR k=0,ind_nlon-1 DO BEGIN
         FOR m=0,ind_nlat-1 DO BEGIN
            ind_sst(k,m,j)=MEAN(this_ind_sst(k,m,*))
            ind_hmix(k,m,j)=MEAN(this_ind_hmix(k,m,*))
            ensmean_sst(k,m,j)=MEAN(this_ensmean_sst(k,m,*))
            ensmean_hmix(k,m,j)=MEAN(this_ensmean_hmix(k,m,*))
            
            IF mask(k,m) ne 1 THEN BEGIN
               FOR n=0,29 DO BEGIN
                  ind_dz_total=0
                  ensmean_dz_total=0
                  FOR p=0,ind_nz-1 DO BEGIN
                     IF ABS(ind_z(p)) le ABS(this_ensmean_hmix(k,m,n)) THEN BEGIN
                        IF p eq 0 THEN BEGIN
                           dz=2*ABS(ind_z(0))
                        ENDIF ELSE $
                           dz=ABS(ind_z(p))-ABS(ind_z(p-1))
                        this_ensmean_mlcorr(k,m,n)=this_ensmean_mlcorr(k,m,n)+$
                                                   this_ensmean_fcorr(k,m,p)*dz
                        ensmean_dz_total=ensmean_dz_total+dz
                     ENDIF
                     IF ABS(ind_z(p)) le ABS(this_ind_hmix(k,m,n)) THEN BEGIN
                        IF p eq 0 THEN BEGIN
                           dz=2*ind_z(0)
                        ENDIF ELSE $
                           dz=ABS(ind_z(p))-ABS(ind_z(p-1))
                        this_ind_mlcorr(k,m,n)=this_ind_mlcorr(k,m,n)+$
                                               this_ind_fcorr(k,m,p)*dz
                        ind_dz_total=ind_dz_total+dz
                     ENDIF
                  ENDFOR
                  this_ensmean_mlcorr(k,m,n)=this_ensmean_mlcorr(k,m,n);/ensmean_dz_total
                  this_ind_mlcorr(k,m,n)=this_ind_mlcorr(k,m,n);/ind_dz_total
               ENDFOR
            ENDIF
            
            ensmean_mlcorr(k,m,j)=MEAN(this_ensmean_mlcorr(k,m,*))
            ind_mlcorr(k,m,j)=MEAN(this_ind_mlcorr(k,m,*))
            
         ENDFOR
      ENDFOR
     
      temp=REFORM(ind_sst(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ind_sst_aavg(i,j)=MEAN(temp,/NaN)
     
      temp=REFORM(ensmean_sst(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ensmean_sst_aavg(i,j)=MEAN(temp,/NaN)
      
      temp=REFORM(ind_hmix(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ind_hmix_aavg(i,j)=MEAN(temp,/NaN)
      
      temp=REFORM(ensmean_hmix(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ensmean_hmix_aavg(i,j)=MEAN(temp,/NaN)
      
      temp=REFORM(ind_mlcorr(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ind_mlcorr_aavg(i,j)=MEAN(temp,/NaN)
      
      temp=REFORM(ensmean_mlcorr(*,*,j))
      temp[where(mask eq 1)]=!Values.F_NaN
      ensmean_mlcorr_aavg(i,j)=MEAN(temp,/NaN)

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.ind_sst_bias.'+kpp_runs(i)+'_'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
      LEVS,MANUAL=levels_diff
      MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      CON,X=ind_longitude,Y=ind_latitude,FIELD=REFORM(ind_sst(*,*,j))-REFORM(obs_ssts(*,*,j)),$
          TITLE='SST bias from '+kpp_runs(i)+' in '+months(j)+' using ind fcorr',$
          CB_TITLE='Temperature (K)',/NOLINES,/BLOCK
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.ensmean_sst_bias.'+kpp_runs(i)+'_'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
      LEVS,MANUAL=levels_diff
      MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      CON,X=ind_longitude,Y=ind_latitude,FIELD=REFORM(ensmean_sst(*,*,j))-REFORM(obs_ssts(*,*,j)),$
          TITLE='SST bias from '+kpp_runs(i)+' in '+months(j)+' using ensmean fcorr',$
          CB_TITLE='Temperature (K)',/NOLINES,/BLOCK
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.ensmean-minus-ind_sst.'+kpp_runs(i)+'_'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(levels_diff)+1
      LEVS,MANUAL=levels_diff
      MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      CON,X=ind_longitude,Y=ind_latitude,FIELD=REFORM(ensmean_sst(*,*,j))-REFORM(ind_sst(*,*,j)),$
          TITLE='Diff in SSTs for '+kpp_runs(i)+' in '+months(j)+' - ensmean minus ind fcorr',$
          CB_TITLE='Temperature (K)',/NOLINES,/BLOCK
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.ensmean-minus-ind_hmix.'+kpp_runs(i)+'_'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(hmix_levels_diff)+1
      LEVS,MANUAL=hmix_levels_diff
      MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      CON,X=ind_longitude,Y=ind_latitude,FIELD=REFORM(ensmean_hmix(*,*,j))-REFORM(ind_hmix(*,*,j)),$
          TITLE='Diff in hmix for '+kpp_runs(i)+' in '+months(j)+' - ensmean minus ind fcorr',$
          CB_TITLE='Temperature (K)',/NOLINES,/BLOCK
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.ensmean-minus-ind_mlcorr.'+kpp_runs(i)+'_'+months(j)+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
      CS,SCALE=1,NCOLS=N_ELEMENTS(vintml_levels_diff)+1,white=[11]
      LEVS,MANUAL=vintml_levels_diff
      MAP,LATMIN=box(0),LATMAX=box(2),LONMIN=box(1),LONMAX=box(3)
      CON,X=ind_longitude,Y=ind_latitude,FIELD=REFORM(ensmean_mlcorr(*,*,j))-REFORM(ind_mlcorr(*,*,j)),$
          TITLE='Diff in vint_ml fcorr for '+kpp_runs(i)+' in '+months(j)+' - ensmean minus ind fcorr',$
          CB_TITLE='Vertical integral (over mixed layer) of artificial heat flux into ocean (W m!U-2!N)',/NOLINES,/BLOCK
      PSCLOSE,/NOVIEW

   ENDFOR
ENDFOR

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.sst_biases_domain_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
GSET,XMIN=0,XMAX=n_times,YMIN=-1.0,YMAX=1.0,TITLE='SST biases area-averaged across coupling domain'
colors=['purple','blue','cyan','orange','red','pink','brown','black','green','dark green','steel blue']
FOR i=0,n_runs-1 DO BEGIN
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_sst_aavg(i,*)-obs_sst_aavg(*)),SYM=4,COL=FSC_COLOR(colors(i)),/NOLINES
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_sst_aavg(i,*)-obs_sst_aavg(*)),SYM=5,COL=FSC_COLOR(colors(i)),/NOLINES
ENDFOR
ind_mean=fltarr(n_times)
ensmean_mean=fltarr(n_times)
FOR j=0,n_times-1 DO BEGIN
   ind_mean(j)=MEAN(ind_sst_aavg(*,j)-obs_sst_aavg(j))
   ensmean_mean(j)=MEAN(ensmean_sst_aavg(*,j)-obs_sst_aavg(j))
ENDFOR
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_mean),COL=FSC_COLOR('violet red')
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_mean),COL=FSC_COLOR('dark grey')
AXES,XVALS=indgen(n_times)+0.5,XLABELS=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','jan'],$
     YSTEP=0.2,YMINOR=0.1,NDECS=2,XTITLE='Month',YTITLE='Domain-mean SST bias (K)'
GPLOT,X=[0,n_times],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(['Corrected by flux corrections from individual run',$
                        'Corrected by ensemble-mean of individual flux corrections']),$
        LEGPOS=1,SYM=REVERSE([4,5]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('black')]),length=0
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.hmix_domain_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
GSET,XMIN=0,XMAX=n_times,YMIN=55,YMAX=20,TITLE='Hmix area-averaged across coupling domain'
colors=['purple','blue','cyan','orange','red','pink','brown','black','green','dark green','steel blue']
FOR i=0,n_runs-1 DO BEGIN
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_hmix_aavg(i,*)),SYM=4,COL=FSC_COLOR(colors(i)),/NOLINES
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_hmix_aavg(i,*)),SYM=5,COL=FSC_COLOR(colors(i)),/NOLINES
ENDFOR
FOR j=0,n_times-1 DO BEGIN
   ind_mean(j)=MEAN(ind_hmix_aavg(*,j))
   ensmean_mean(j)=MEAN(ensmean_hmix_aavg(*,j))
ENDFOR
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_mean),COL=FSC_COLOR('violet red')
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_mean),COL=FSC_COLOR('dark grey')
AXES,XVALS=indgen(n_times)+0.5,XLABELS=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','jan'],$
     YSTEP=-2.5,YMINOR=-1.25,NDECS=2,XTITLE='Month',YTITLE='Domain-mean hmix (m)'
;GPLOT,X=[0,n_times],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(['Corrected by flux corrections from individual run',$
                        'Corrected by ensemble-mean of individual flux corrections']),$
        LEGPOS=1,SYM=REVERSE([4,5]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('black')]),length=0
PSCLOSE

psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_compare_ind_rerun.vintml_fcorr_domain_mean.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=500
GSET,XMIN=0,XMAX=n_times,YMIN=-10,YMAX=60,TITLE='Fcorr vint_ml area-averaged across coupling domain'
colors=['purple','blue','cyan','orange','red','pink','brown','black','green','dark green','steel blue']
FOR i=0,n_runs-1 DO BEGIN
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_mlcorr_aavg(i,*)),SYM=4,COL=FSC_COLOR(colors(i)),/NOLINES
   GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_mlcorr_aavg(i,*)),SYM=5,COL=FSC_COLOR(colors(i)),/NOLINES
ENDFOR
FOR j=0,n_times-1 DO BEGIN
   ind_mean(j)=MEAN(ind_mlcorr_aavg(*,j))
   ensmean_mean(j)=MEAN(ensmean_mlcorr_aavg(*,j))
ENDFOR
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ind_mean),COL=FSC_COLOR('violet red')
GPLOT,X=indgen(n_times)+0.5,Y=REFORM(ensmean_mean),COL=FSC_COLOR('dark grey')

AXES,XVALS=indgen(n_times)+0.5,XLABELS=['jan','feb','mar','apr','may','jun','jul','aug','sep','oct','nov','dec','jan'],$
     YSTEP=3.,YMINOR=1.5,NDECS=1,XTITLE='Month',YTITLE='Domain-mean fcorr vint over mixed-layer (W m!U-2!N)'
GPLOT,X=[0,n_times],Y=[0,0],STYLE=2
GLEGEND,labels=REVERSE(['Corrected by flux corrections from individual run',$
                        'Corrected by ensemble-mean of individual flux corrections']),$
        LEGPOS=1,SYM=REVERSE([4,5]),COL=REVERSE([FSC_COLOR('black'),FSC_COLOR('black')]),length=0
GLEGEND,labels=REVERSE(['Corrected by flux corrections from individual run',$
                        'Corrected by ensemble-mean of individual flux corrections']),$
        LEGPOS=9,COL=REVERSE([FSC_COLOR('violet red'),FSC_COLOR('dark grey')])
PSCLOSE

STOP
END

