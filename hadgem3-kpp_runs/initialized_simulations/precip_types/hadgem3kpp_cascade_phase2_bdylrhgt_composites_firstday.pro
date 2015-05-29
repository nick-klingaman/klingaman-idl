PRO hadgem3kpp_cascade_phase2_bdylrhgt_composites_firstday

um3='/home/ss901165/um_output3'
dates=['k9c40','k89q0','k8170','k6a70','k54i0','k4130','k2bt0','k2750','k25q0','k12g0','k0cd0','k0as0']

box=[-15,40,15,200]
time_offset=0
n_times=1

n_sets=4
n_variables=1

n_bins=20
precip_bins=fltarr(n_bins)
binned_bdylrhgt=fltarr(n_sets,n_bins)

FOR j=0,n_variables-1 DO BEGIN
   CASE j OF
      0 : BEGIN
         varname='bdylrhgt'
         ncvarname='field1534'
         longitude_var='longitude'
         latitude_var='latitude'
         multiplier=1.
         levels=['250','300','350','400','450','500','550','600','650','700','750','800','850','900']
         diff_levels=['-130','-110','-90','-70','-50','-30','-10','10','30','50','70','90','110','130']
      END
   ENDCASE
   print,'---> '+varname
   FOR i=0,n_sets-1 DO BEGIN
      CASE i OF
         0 : BEGIN
            runids=['xfrla','xfrle','xfrli','xfrlm','xfrlq','xfrlu','xfsea','xfsee','xfsei','xfsem','xfseq','xfseu']
            set_name='control'       
         END
         1 : BEGIN
            runids=['xfrlb','xfrlf','xfrlj','xfrln','xfrlr','xfrlv','xfseb','xfsef','xfsej','xfsen','xfser','xfsev']
            set_name='15xentrain'
         END
         2 : BEGIN
            runids=['xfrlc','xfrlg','xfrlk','xfrlo','xfrls','xfrlw','xfsec','xfseg','xfsek','xfseo','xfses','xfsew']
            set_name='nocmt'
         END
         3 : BEGIN
            runids=['xfrld','xfrlh','xfrll','xfrlp','xfrlt','xfrlx','xfsed','xfseh','xfsel','xfsem','xfset','xfsex']
            set_name='15xentrain_nocmt'
         END
      ENDCASE
      n_cases=N_ELEMENTS(runids)
      print,'-------> '+set_name            
      FOR k=0,n_cases-1 DO BEGIN
         infile=um3+'/'+runids(k)+'/'+runids(k)+'a.pa'+dates(k)+'.nc'
         longitude=OPEN_AND_EXTRACT(infile,longitude_var)
         latitude=OPEN_AND_EXTRACT(infile,latitude_var)
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
         
         IF k eq 0 THEN BEGIN
            composite_var=fltarr(n_lon,n_lat)
            allruns_precip=fltarr(n_cases,n_lon*n_lat)
            allruns_bdylrhgt=fltarr(n_cases,n_lon*n_lat)
         ENDIF
         
         var=REFORM(OPEN_AND_EXTRACT(infile,ncvarname,offset=[box_tx(1),box_tx(0),0,0],$
                                     count=[n_lon,n_lat,1,n_times]))*multiplier

         precip=REFORM(OPEN_AND_EXTRACT(infile,'precip',offset=[box_tx(1),box_tx(0),0,0],$
                                        count=[n_lon,n_lat,1,n_times]))*86400.
         

         FOR m=0,n_lon-1 DO $
            FOR n=0,n_lat-1 DO $
               composite_var(m,n)=composite_var(m,n)+MEAN(var(m,n,*))*1./FLOAT(n_cases)               
               
         allruns_precip(k,*)=REFORM(precip,[n_lon*n_lat])
         allruns_bdylrhgt(k,*)=REFORM(var,[n_lon*n_lat])
      ENDFOR      
      IF i eq 0 THEN BEGIN
         control_composite_var=composite_var
         allruns_nonzero_precip=allruns_precip[where(allruns_precip gt 0.024)]
         allruns_nonzero_precip_sorted=SORT(allruns_nonzero_precip)      
         FOR n=0,n_bins-1 DO $
            precip_bins(n)=allruns_nonzero_precip(allruns_nonzero_precip_sorted(FLOOR(N_ELEMENTS(allruns_nonzero_precip)*n/FLOAT(n_bins))))
         print,precip_bins
      ENDIF

      allruns_precip_indices=VALUE_LOCATE(precip_bins(*),REFORM(allruns_precip))
      FOR n=0,n_bins-1 DO $
         binned_bdylrhgt(i,n)=MEAN(allruns_bdylrhgt[where(allruns_precip_indices eq n)],/NaN)

      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_bdylrhgt_composites_firstday.'+varname+'.hours0-23.'+$
             set_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700
      CS,SCALE=2,NCOLS=N_ELEMENTS(levels)+1,white=[2]
      LEVS,MANUAL=levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=longitude,Y=latitude,FIELD=composite_var,/NOLINES,/BLOCK,$
          TITLE='Composite of '+varname+' from '+set_name+' for first day of phase 2 cases',CB_TITLE='Boundary-layer height (m)'
      AXES
      PSCLOSE,/NOVIEW

      IF i gt 0 THEN BEGIN
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_bdylrhgt_composites_firstday.'+varname+'.hours0-23.'+$
                set_name+'-minus-control.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
                SPACE2=700   
         CS,SCALE=1,NCOLS=N_ELEMENTS(diff_levels)+1
         LEVS,MANUAL=diff_levels
         MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
         CON,X=longitude,Y=latitude,FIELD=composite_var-control_composite_var,/NOLINES,/BLOCK,$
             TITLE='Difference in composite of '+varname+' for '+set_name+' minus control for first day of phase 2 cases',CB_TITLE='Boundary-layer height (m)'
         AXES
         PSCLOSE,/NOVIEW
      ENDIF      

   ENDFOR

   psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_bdylrhgt_composites_firstday.binned_by_precip.hours0-23.ps'
   PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
          SPACE2=400
   precip_bins=precip_bins/24.
   GSET,XMIN=0,XMAX=n_bins,YMIN=400.,YMAX=900.,TITLE='Comparison of mean boundary-layer height binned by precipitation - 5S-5N, 40-200E - first day of simulations'
   blue=FSC_COLOR('blue',31)
   red=FSC_COLOR('red',30)
   brown=FSC_COLOR('brown',32)
   purple=FSC_COLOR('purple',33)
   FOR i=0,n_sets-1 DO $
      GPLOT,X=indgen(n_bins)+0.5,Y=REFORM(binned_bdylrhgt(i,*)),COL=30+i,STYLE=2
   AXES,XVALS=indgen(n_bins+1),XLABELS=[STRMID(STRTRIM(STRING(precip_bins),1),0,5),' >'+STRMID(STRTRIM(STRING(precip_bins(n_bins-1)),1),0,5)],$
        XTITLE='Precipitation rate (mm hr!U-1!N)',YTITLE='Boundary-layer height (m)'
   PSCLOSE

ENDFOR

STOP
END
   
