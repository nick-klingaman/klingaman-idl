PRO hadgem3kpp_mean_state_striped_fluxes_centreddiff

; Diagnose weird striped-flux behaviour in HadGEM3 runs

n_files=6
all_descriptions=strarr(n_files)
variables=['precip']
;cb_title='850 hPa meridional wind (m s!U-1!N)'
;cb_title='Surface temperature (K)'
cb_title='Precipitation rate (mm day!U-1!N)'
;cb_title='Latent heat flux (W m!U-2!N)'
n_vars=N_ELEMENTS(variables)

mylevs_raw=['0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5','8.5','9.5','10.5','11.5','12.5']
;mylevs_raw=['-11.5','-9.5','-7.5','-5.5','-3.5','-1.5','1.5','3.5','5.5','7.5','9.5','11.5']
;mylevs_raw=['0','20','40','60','80','100','120','140','160','180','200','220','240']
;mylevs_raw=['270','274','278','282','286','290','294','298','302','306','310','314','318','322']
mylevs=['-7.0','-5.6','-4.2','-3.0','-2.0','-1.2','-0.6','-0.2','0.2','0.6','1.2','2.0','3.0','4.2','5.6','7.0']
;mylevs=['-3.5','-2.8','-2.1','-1.5','-1.0','-0.6','-0.3','-0.1','0.1','0.3','0.6','1.0','1.5','2.1','2.8','3.5']
;mylevs=['-21','-16.8','-12.4','-9.0','-6.0','-3.6','-1.8','-0.6','0.6','1.8','3.6','6.0','9.0','12.4','16.8','21']
;mylevs_ratio=['0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1.1','1.3','1.6','2.0','2.5','3.1','3.8','4.6','5.4']

FOR i=0,n_files-1 DO BEGIN
   CASE i OF
      ;3 : BEGIN
      ;   infile='/home/ss901165/datasets/TRMM_3B42V6/n96/sep-nov_2010.nc'
      ;   runid='TRMM_n96'
      ;   description='TRMM at N96 (JJAS clim 1999-2008)'
      ;   multiplier=1
      ;END
      5 : BEGIN
         infile='/home/ss901165/datasets/GPCP/one_degree/n96/sep-nov_2008.nc'
         runid='GPCP_n96'
         description='GPCP at N96 (SON 2008)'
         multiplier=1
      END
      0 : BEGIN
         infile='/home/ss901165/um_output5/xfzbj/i1/xfzbja.psi1son.nc'
         runid='xfzbj'
         description='vn7.4, NPK science (1.5x entrainment), clim SST/ice'
         ;multiplier=86400.
         multiplier=86400.
      END
      3 : BEGIN
         infile='/home/ss901165/um_output5/xgspi/xgspia.psi1son.nc'
         runid='xgspi'
         description='7.8, GA3, AMIP2 SST/ice, monotone fix, no polar filter'
         multiplier=86400.
      END
      2 : BEGIN
         infile='/home/ss901165/um_output5/xfctu_withfixes/xfctua.psi1son.nc'
         runid='xfctu'
         description='vn7.8, GA3.0 science, AMIP2 SST/ice'
      END
      4 : BEGIN
         infile='/home/ss901165/um_output3/hadgem3_monwg/akkvi/test1.nc'
         runid='akkvi'
         description='Met Office akkvi'
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xfctu_monotonefix/xfctua.psi1son.nc'
         runid='xfctu_monotone'
         description='vn7.8, GA3.0 science, AMIP2 SST/ice, monotone fix'
      END
                                ;0 : BEGIN
                                ;   infile='/home/ss901165/datasets/HadCM3_CONTROL/dec_mmean.nc'
                                ;   runid='hadcm3_ctl'
                                ;   description='HadCM3 control (Dec mmean)'
                                ;END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   IF i eq 0 THEN $
      fft_mag=fltarr(n_files,n_lat*2)
   all_descriptions(i)=description
   
   FOR j=0,n_vars-1 DO BEGIN
      this_variable=REFORM(OPEN_AND_EXTRACT(infile,variables(j)))*multiplier(j)
      ;this_variable=REFORM(this_variable(*,*,2))

      ns_centred_diff=fltarr(n_lon,n_lat)
      ns_centred_diff2=fltarr(n_lon,n_lat)
      ew_centred_diff=fltarr(n_lon,n_lat)
      FOR k=0,n_lon-1 DO BEGIN
         ns_centred_diff(k,0)=!Values.F_NaN
         ns_centred_diff(k,n_lat-1)=!Values.F_NaN
         ns_centred_diff2(k,0:1)=!Values.F_NaN
         ns_centred_diff2(k,n_lat-2:n_lat-1)=!Values.F_NaN
         FOR m=1,n_lat-2 DO $
            ns_centred_diff(k,m)=(2*this_variable(k,m)-this_variable(k,m-1)-this_variable(k,m+1))    
         FOR m=2,n_lat-3 DO $
            ns_centred_diff2(k,m)=(2*ns_centred_diff(k,m)-ns_centred_diff(k,m-1)-ns_centred_diff(k,m+1))
      ENDFOR
      FOR m=0,n_lat-1 DO BEGIN
         ew_centred_diff(0,m)=(2*this_variable(0,m)-this_variable(n_lon-1,m)-this_variable(1,m))
         ew_centred_diff(n_lon-1,m)=(2*this_variable(n_lon-1,m)-this_variable(0,m)-this_variable(n_lon-2,m))
         FOR k=1,n_lon-2 DO $
            ew_centred_diff(k,m)=(2*this_variable(k,m)-this_variable(k-1,m)-this_variable(k+1,m))
      ENDFOR

      fft_input=[REFORM(this_variable(0,*)),REVERSE(REFORM(this_variable(n_lon/2,*)))]
      fft_output=FFT(fft_input)
      fft_mag(i,*)=abs(fft_output)^2

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.'+runid+'_'+variables(j)+'_raw.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1,/REV
      LEVS,MANUAL=mylevs_raw
      MAP,/hires
      CON,X=longitude,Y=latitude,FIELD=this_variable,/NOLINES,TITLE='Seasonal mean (i1sep) '+variables(j)+' from run '+description,$
          CB_TITLE=cb_title
      AXES
      PSCLOSE,/NOVIEW
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.'+runid+'_'+variables(j)+'_nsdiff.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      MAP,/hires
      CON,X=longitude,Y=latitude,FIELD=ns_centred_diff,/NOLINES,TITLE='First derivative N-S direction for '+variables(j)+' from run '+description,$
          CB_TITLE=cb_title
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.'+runid+'_'+variables(j)+'_ewdiff.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      MAP,/hires
      CON,X=longitude,Y=latitude,FIELD=ew_centred_diff,/NOLINES,TITLE='First derivative E-W direction for '+variables(j)+' from run '+description,$
          CB_TITLE=cb_title
      AXES
      PSCLOSE,/NOVIEW

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.'+runid+'_'+variables(j)+'_nsdiff2.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=400
      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
      LEVS,MANUAL=mylevs
      MAP,/hires
      CON,X=longitude,Y=latitude,FIELD=ns_centred_diff2,/NOLINES,TITLE='Second derivative N-S direction for '+variables(j)+' from run '+description,$
          CB_TITLE=cb_title
      AXES
      PSCLOSE,/NOVIEW

;      psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.'+runid+'_'+variables(j)+'_nsdiff-ratio-ewdiff.ps'
;      PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=1500,SPACE2=500,XOFFSET=500,YOFFSET=1000,TFONT=2,TCHARSIZE=90,SPACE3=200
;      CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs)+1
;      LEVS,MANUAL=mylevs_ratio
;      MAP,/hires
;      CON,X=longitude,Y=latitude,FIELD=ns_centred_diff/ew_centred_diff,/NOLINES
;      AXES
;      PSCLOSE
   ENDFOR
ENDFOR

f=findgen(n_lat+1)
psfile='/home/ss901165/idl/hadgem3-kpp_runs/mean_state/striped_fluxes/hadgem3kpp_mean_state_striped_fluxes_centreddiff.fft'+variables(0)+'_allruns.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=105,MARGIN=2500,SPACE2=500,XOFFSET=1500,YOFFSET=500,TFONT=2,TCHARSIZE=90,SPACE3=400
;GSET,XMIN=10.,XMAX=n_lat,YMIN=0.0000005,YMAX=0.1,/YLOG
GSET,XMIN=10,XMAX=n_lat,YMIN=0.0000001,YMAX=0.1,/YLOG
colors=['red','blue','cyan','brown','purple','black']
FOR i=0,n_files-1 DO $
   GPLOT,X=f(10:n_lat),Y=REFORM(fft_mag(i,10:n_lat)),COL=FSC_COLOR(colors(i))
AXES,XSTEP=10,XTITLE='Wavenumber (290 points in total)',YTITLE='Power',YVALS=[0.0000005,0.000001,0.000002,0.000004,0.000007,0.00001,0.00002,0.00004,$
                                                                              0.00007,0.0001,0.0002,0.0004,0.0007,0.001,0.002,0.004,0.007,$
                                                                              0.01,0.02,0.04,0.07,0.1],NDECS=4
;AXES,XSTEP=10,XTITLE='Wavenumber (290 points in total)',YTITLE='Power',YVALS=[0.0001,0.0002,0.0004,0.0007,0.001,0.002,0.004,0.007,0.01,0.02,0.04,0.07,0.1,$
;                                                                              0.2,0.4,0.7,1.0,2.0,4.0,7.0,10.0],NDECS=4
GLEGEND,labels=REVERSE(all_descriptions),COL=REVERSE(FSC_COLOR(colors)),LEGPOS=3
PSCLOSE   

STOP
END
