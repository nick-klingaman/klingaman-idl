PRO hadgem3_monwg_aquaplanet_daily_preciparea

percent_bins=[5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,99]
n_bins=N_ELEMENTS(percent_bins)
n_runs=4

                     ;H 3H 6H D 2D 4D 8D 16D 32D
meaning_lengths=LONG([3, 3, 2,4, 2, 2, 2,  2,  2]) ; Each as a multiple of the last
n_meaning_lengths=N_ELEMENTS(meaning_lengths)

pdfs=fltarr(n_runs,n_meaning_lengths+2,n_bins+1,5)
colors=strarr(n_runs)
styles=strarr(n_runs)
model_names=strarr(n_runs)
FOR i=0,n_runs-1 DO BEGIN
   ; CASE i OF
   ;   3 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_ctlent'
   ;       colors(i)='black'
   ;       styles(i)=0
   ;    END
   ;    2 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_1.5xent'
   ;       colors(i)='red'
   ;       styles(i)=0
   ;    END
   ;    4 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_2.5xent'
   ;       colors(i)='orange'
   ;       styles(i)=0
   ;    END
   ;    0 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhccw/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_2xcape'
   ;       colors(i)='blue'
   ;       styles(i)=0
   ;    END
   ;    1 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhccx/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_0.5xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_0.5xcape'
   ;       colors(i)='brown'
   ;       styles(i)=0
   ;    END
   ;    5 : BEGIN
   ;       infile='/home/ss901165/um_output5/xhccy/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent_2xcape.jan-dec_dmeans.years1-3.tot_precip.nc'
   ;       model_names(i)='fixnhsol5N_sin2sin4_1.5xent_2xcape'
   ;       colors(i)='purple'
   ;       styles(i)=0
   ;    END
   ; ENDCASE

   CASE i OF 
   
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
;   DEFINE_BOUNDARIES,[-10,60,10,80],latitude,longitude,box_tx,/LIMIT
   n_lon=N_ELEMENTS(longitude)
   n_lat=N_ELEMENTS(latitude)
   
   print,'Reading timestep precipitation data ...'
   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip') ; Convert to mm/day
;   timestep_precip=OPEN_AND_EXTRACT(infile,'tot_precip',offset=[box_tx(1),box_tx(0),0],count=[n_lon,n_lat,LONG(72*1080)])*72.
   
   our_timestep_precip=timestep_precip
   our_ntime=N_ELEMENTS(our_timestep_precip(0,0,*))
   
   FOR j=0,n_meaning_lengths DO BEGIN
      print,' ---> '+STRTRIM(STRING(j),1)+' of '+STRTRIM(STRING(n_meaning_lengths),1)
      print,'Computing areas ...'
      areas=fltarr(n_bins,our_ntime-1)
      FOR k=LONG(1),LONG(our_ntime)-1 DO BEGIN
         temp_precip=REFORM(our_timestep_precip(*,*,k),[n_lon*n_lat])
         temp_precip_sorted=SORT(temp_precip)
         temp_precip_sorted=REVERSE(temp_precip_sorted)
         running_fraction=fltarr(n_lon*n_lat)
         total_precip=TOTAL(temp_precip)
         FOR m=0,n_lon*n_lat-1 DO $
            running_fraction(m)=TOTAL(temp_precip[temp_precip_sorted(0:m)])/total_precip
         FOR m=0,n_bins-1 DO $
            areas(m,k-1)=(NEAREST(running_fraction,percent_bins(m)/100.)+1)/FLOAT(n_lon*n_lat)
      ENDFOR
      FOR k=0,n_bins-1 DO BEGIN
         temp_areas=REFORM(areas(k,*))
         sorted=SORT(temp_areas)
         pdfs(i,j,k,0)=temp_areas[sorted(0)]
         pdfs(i,j,k,1)=temp_areas[sorted(our_ntime/4)]
         pdfs(i,j,k,2)=temp_areas[sorted(our_ntime/2)]
         pdfs(i,j,k,3)=temp_areas[sorted(our_ntime*3./4.)]
         pdfs(i,j,k,4)=temp_areas[sorted(our_ntime-2)]
      ENDFOR
      ; Do meaning for next pass
      IF j ne n_meaning_lengths THEN BEGIN
         temp=our_timestep_precip
         our_ntime=our_ntime/meaning_lengths(j)
         our_timestep_precip=fltarr(n_lon,n_lat,our_ntime)
         print,'Meaning into new timeseries of '+STRTRIM(STRING(our_ntime),1)+' points ...'
         FOR k=0,n_lon-1 DO BEGIN
            print,'-----> Longitude '+STRTRIM(STRING(k+1),1)+' ...'
            FOR m=0,n_lat-1 DO $
               FOR n=LONG(0),LONG(our_ntime)-1 DO $
                  our_timestep_precip(k,m,n)=MEAN(temp(k,m,n*meaning_lengths(j):(n+1)*meaning_lengths(j)-1))      
         ENDFOR
      ENDIF
   ENDFOR

   temp=our_timestep_precip
   our_timestep_precip=fltarr(n_lon,n_lat)
   areas=fltarr(n_bins,our_ntime-1)
   temp_precip_sorted=SORT(our_timestep_precip)
   temp_precip_sorted=REVERSE(temp_precip_sorted)
   running_fraction=fltarr(n_lon*n_lat)
   total_precip=TOTAL(temp_precip)
   FOR m=0,n_lon*n_lat-1 DO $
      running_fraction(m)=TOTAL(temp_precip[temp_precip_sorted(0:m)])/total_precip
   FOR m=0,n_bins-1 DO $
      pdfs(i,n_meaning_lengths+1,m,0)=(NEAREST(running_fraction,percent_bins(m)/100.)+1)/FLOAT(n_lon*n_lat)

ENDFOR

FOR j=0,n_meaning_lengths DO BEGIN
   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/daily/hadgem3_monwg_aquaplanet_daily_preciparea.eqx_dcyc.meaning_length'+STRTRIM(STRING(j),1)+'.ps'
   PSOPEN,file=psfile,TFONT=2,FONT=2,CHARSIZE=120
   total_mean=1
   FOR k=0,j-1 DO $
      total_mean=total_mean*meaning_lengths(k)
   GSET,XMIN=0,XMAX=n_bins,YMIN=0.2,YMAX=80,/YLOG,TITLE='Wettest [y_value]% of domain (15S-15N) '+$
        'accounts for [x_value]% of domain-total precip (meaning '+STRTRIM(STRING(total_mean),1)+' timesteps)'
   FOR k=0,n_runs-1 DO BEGIN
      FOR m=0,n_bins-1 DO $
         EBAR,X=m+0.3+0.2*ODD(k),BOX=pdfs(k,j,m,*)*100.,COL=FSC_COLOR(colors(k))
                                ;GPLOT,X=indgen(n_bins+1)+0.5,Y=REFORM(pdfs(k,j,*)),COL=FSC_COLOR(colors(k)),STYLE=styles(k),SYM=3,SIZE=50
                                ;GPLOT,X=[0.5,n_bins-0.5],Y=[percent_bins(0),percent_bins(n_bins-1)],STYLE=1
      GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(pdfs(k,n_meaning_lengths+1,0:n_bins-1,0)*100.),COL=FSC_COLOR(colors(k)),STYLE=1
   ENDFOR
   AXES,XVALS=indgen(n_bins)+0.5,XLABELS=[STRMID(STRTRIM(STRING(percent_bins),1),0,4)],$                                     
        YVALS=['0.2','0.3','0.4','0.6','0.8','1.0',$
               '1.5','2.0','3.0','4.0','6.0','8.0','10.0','15.0','20.0','30.0','40.0','60.0','80.0'],$
        YTITLE='Percentage of wettest gridpoints',XTITLE='Percentage of domain-total precipitation'   
   GLEGEND,labels=REVERSE(model_names),COL=REVERSE(FSC_COLOR(colors)),STYLE=REVERSE(styles),LEGPOS=11
   PSCLOSE,/NOVIEW
ENDFOR

STOP
END

      
