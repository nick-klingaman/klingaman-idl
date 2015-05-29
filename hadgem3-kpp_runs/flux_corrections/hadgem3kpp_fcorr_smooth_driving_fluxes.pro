PRO hadgem3kpp_fcorr_smooth_driving_fluxes

; Run a filter over the input fluxes to KPP, in order to reduce
; gridpoint variability

basedir='/export/mango/data-10/ss901165/kpp_ocean/ancillaries/flux_data'
input_file=basedir+'/xgspj.fluxes_for_kpp.jan-dec_dmeans.i5.3hr_n96_tgrid.nc'
output_file=basedir+'/xgspj.fluxes_for_kpp.jan-dec_dmeans.i5.3hr_n96_tgrid_smooth_121xy.nc'
runid='xgspj_i5'

mask_file='/home/ss901165/datasets/HADGEM3-KPP_ANCIL/landfrac_n96_hadgem3-7.8.nc'

longitude=OPEN_AND_EXTRACT(input_file,'longitude')
latitude=OPEN_AND_EXTRACT(input_file,'latitude')
time=OPEN_AND_EXTRACT(input_file,'time')
n_lon=N_ELEMENTS(longitude)
n_lat=N_ELEMENTS(latitude)
n_time=N_ELEMENTS(time)

mask=REFORM(OPEN_AND_EXTRACT(mask_file,'lsm'))
mask_periodic=fltarr(n_lon+2,n_lat)
mask_periodic(1:n_lon,*)=mask
mask_periodic(0,*)=mask(n_lon-1,*)
mask_periodic(n_lon+1,*)=mask(0,*)

variables=['lhf','shf','lwf','precip','swf','taux','tauy']
;variables=['lhf']
n_variables=N_ELEMENTS(variables)

id=NCDF_CREATE(output_file,/CLOBBER)
dimids=intarr(3)
varids=intarr(n_variables+3)
dimids(0)=NCDF_DIMDEF(id,'longitude',n_lon)
dimids(1)=NCDF_DIMDEF(id,'latitude',n_lat)
dimids(2)=NCDF_DIMDEF(id,'time',n_time)
varids(0)=NCDF_VARDEF(id,'longitude',[dimids(0)])
varids(1)=NCDF_VARDEF(id,'latitude',[dimids(1)])
varids(2)=NCDF_VARDEF(id,'time',[dimids(2)])
FOR i=0,n_variables-1 DO $
   varids(3+i)=NCDF_VARDEF(id,variables(i),[dimids(0),dimids(1),dimids(2)])

NCDF_CONTROL,id,/ENDEF

NCDF_VARPUT,id,varids(0),longitude
NCDF_VARPUT,id,varids(1),latitude
NCDF_VARPUT,id,varids(2),time

FOR i=0,n_variables-1 DO BEGIN
   
   CASE i OF 
      0 : BEGIN
         mylevs_raw=['-250','-235','-220','-205','-190','-175','-160','-145','-130','-115','-90','-75','-60']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         multiplier=1.
         cb_title='W m!U-2!N'
      END
      1 : BEGIN
         mylevs_raw=['-42','-39','-36','-33','-30','-27','-24','-21','-18','-15','-12','-9','-6','-3','0']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         multiplier=1.
         cb_title='W m!U-2!N'
      END
      4 : BEGIN
         mylevs_raw=['150','160','170','180','190','200','210','220','230','240','250','260','270','280','290','300']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         multiplier=1.
         cb_title='W m!U-2!N'
      END
      2 : BEGIN
         mylevs_raw=['-105','-100','-95','-90','-85','-80','-75','-70','-65','-60','-55','-50','-45','-40']
         mylevs_diff=['-7.5','-6.5','-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5','6.5','7.5']
         multiplier=1.
         cb_title='W m!U-2!N'
      END
      3 : BEGIN
         mylevs_raw=['1','2','3','4','5','6','7','8','9','10','11','12','13','14','15']
         mylevs_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         multiplier=86400.
         cb_title='mm day!U-1!N'
      END
      5 : BEGIN
         mylevs_raw=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
         mylevs_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         multiplier=100.
         cb_title='N m!U-2!N * 100'
      END
      6 : BEGIN
         mylevs_raw=['-15','-13','-11','-9','-7','-5','-3','-1','1','3','5','7','9','11','13','15']
         mylevs_diff=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         multiplier=100.
         cb_title='N m!U-2!N * 100'
      END
   ENDCASE

   print,'Now reading variable '+variables(i)+' ...'
   input_variable=REFORM(OPEN_AND_EXTRACT(input_file,variables(i)))
   input_variable_save=input_variable

   input_variable_periodic=fltarr(n_lon+2,n_lat,n_time)
   input_variable_periodic(1:n_lon,*,*)=input_variable
   input_variable_periodic(0,*,*)=input_variable(n_lon-1,*,*)
   input_variable_periodic(n_lon+1,*,*)=input_variable(0,*,*)   

   output_variable=fltarr(n_lon,n_lat,n_time)
   print,'Now smoothing variable '+variables(i)+' ...'
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=1,n_lat-2 DO BEGIN         
         IF mask_periodic(j+1,k) lt 0.1 THEN BEGIN
            weights=intarr(3)
            weights(*)=0
            weights(1)=2
            IF mask_periodic(j,k) lt 0.1 THEN $
               weights(0)=1
            IF mask_periodic(j+2,k) lt 0.1 THEN $
               weights(2)=1
            output_variable(j,k,*)=(input_variable_periodic(j,k,*)*weights(0)+$
                                    input_variable_periodic(j+1,k,*)*weights(1)+$
                                    input_variable_periodic(j+2,k,*)*weights(2))/FLOAT(TOTAL(weights))
         ENDIF ELSE $
            output_variable(j,k,*)=input_variable_periodic(j+1,k,*)
      ENDFOR
      output_variable(j,0,*)=input_variable(j,0,*)
      output_variable(j,n_lat-1,*)=input_variable(j,n_lat-1,*)
   ENDFOR   
   
   input_variable=output_variable
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=1,n_lat-2 DO BEGIN         
         IF mask_periodic(j+1,k) lt 0.1 THEN BEGIN
            weights=intarr(3)
            weights(*)=0
            weights(1)=2
            IF mask_periodic(j,k-1) lt 0.1 THEN $
               weights(0)=1
            IF mask_periodic(j,k+1) lt 0.1 THEN $
               weights(2)=1
            output_variable(j,k,*)=(input_variable(j,k-1,*)*weights(0)+$
                                    input_variable(j,k,*)*weights(1)+$
                                    input_variable(j,k+1,*)*weights(2))/FLOAT(TOTAL(weights))
         ENDIF ELSE $
            output_variable(j,k,*)=input_variable_periodic(j+1,k,*)
      ENDFOR
      output_variable(j,0,*)=input_variable(j,0,*)
      output_variable(j,n_lat-1,*)=input_variable(j,n_lat-1,*)
   ENDFOR
   
   print,'Now writing variable '+variables(i)+' ...'
;   FOR i=0,n_variables-1 DO $
   NCDF_VARPUT,id,varids(3+i),output_variable

   input_variable_timemean=fltarr(n_lon,n_lat)
   output_variable_timemean=fltarr(n_lon,n_lat)
   FOR j=0,n_lon-1 DO BEGIN
      FOR k=0,n_lat-1 DO BEGIN
         input_variable_timemean(j,k)=MEAN(input_variable_save(j,k,*))*multiplier
         output_variable_timemean(j,k)=MEAN(output_variable(j,k,*))*multiplier
      ENDFOR
   ENDFOR   
   input_variable_timemean[where(mask eq 1)]=!Values.F_NaN
   output_variable_timemean[where(mask eq 1)]=!Values.F_NaN
;   input_variable_timemean[where(input_variable_timemean) eq 0.)]=!Values.F_NaN
;   output_variable_timemean[where(ABS(output_variable_timemean) eq 0.)]=!Values.F_NaN

   ; Plot time-mean of input and output variables
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_smooth_driving_fluxes.'+variables(i)+$
          '_'+runid+'_nosmooth.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=-40,LATMAX=40,LONMIN=30,LONMAX=210
   CON,X=longitude,Y=latitude,FIELD=(input_variable_timemean),/NOLINES,/BLOCK,$
       TITLE='Time-mean of '+variables(i)+' in '+runid+' without smoothing',CB_TITLE=cb_title
   PSCLOSE,/NOVIEW

   ; Plot time-mean of input and output variables
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_smooth_driving_fluxes.'+variables(i)+$
          '_'+runid+'_smooth.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_raw)+1
   LEVS,MANUAL=mylevs_raw
   MAP,LATMIN=-40,LATMAX=40,LONMIN=30,LONMAX=210
   CON,X=longitude,Y=latitude,FIELD=(output_variable_timemean),/NOLINES,/BLOCK,$
       TITLE='Time-mean of '+variables(i)+' in '+runid+' with 1-2-1 smoothing in x and y directions',CB_TITLE=cb_title
   PSCLOSE,/NOVIEW
   
   ; Plot time-mean of input and output variables
   psfile='/home/ss901165/idl/hadgem3-kpp_runs/flux_corrections/hadgem3kpp_fcorr_smooth_driving_fluxes.'+variables(i)+$
          '_'+runid+'_smooth-minus-nosmooth.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=110,MARGIN=1500,SPACE2=1300,XOFFSET=500,YOFFSET=1500,TFONT=2,TCHARSIZE=100,$
          YSIZE=10000,SPACE3=500
   CS,SCALE=1,NCOLS=N_ELEMENTS(mylevs_diff)+1
   LEVS,MANUAL=mylevs_diff
   MAP,LATMIN=-40,LATMAX=40,LONMIN=30,LONMAX=210
   CON,X=longitude,Y=latitude,FIELD=(output_variable_timemean)-(input_variable_timemean),/NOLINES,/BLOCK,$
       TITLE='Diff in time-mean of '+variables(i)+' in '+runid+' for smoothed (1-2-1 x and y) minus un-smoothed',$
       CB_TITLE=cb_title
   PSCLOSE,/NOVIEW

ENDFOR

NCDF_CLOSE,id

STOP
END
