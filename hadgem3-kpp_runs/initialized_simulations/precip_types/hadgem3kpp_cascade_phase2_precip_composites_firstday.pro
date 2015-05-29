PRO hadgem3kpp_cascade_phase2_precip_composites_firstday

um3='/home/ss901165/um_output3'
date_ranges=['04nov09-06nov09','26aug08-28aug08','07dec07-09dec07','07sep06-09sep06','18mar05-20mar05','03dec03-05dec03',$
             '29oct02-01nov02','05jun02-07jun02','26apr02-28apr02','16jan01-18jan01','13nov00-15nov00','28sep00-30sep00']

box=[-15,40,15,200]
time_offset=0
n_times=24
hour_range=STRTRIM(STRING(time_offset*3),1)+'-'+STRTRIM(STRING((time_offset+n_times)*3-1),1)

shallow_levels=['0.005','0.010','0.015','0.020','0.025','0.030','0.035','0.040','0.045','0.050','0.055']
shallow_diff_levels=['-0.045','-0.039','-0.033','-0.027','-0.021','-0.015','-0.009','-0.003','0.003','0.009','0.015','0.021','0.027','0.033','0.039','0.045']
mid_levels=['0.03','0.06','0.09','0.12','0.15','0.18','0.21','0.24','0.27','0.30','0.33','0.36','0.39']
mid_diff_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
deep_levels=['0.80','0.82','0.84','0.86','0.88','0.90','0.92','0.94','0.95','0.96','0.97','0.98','0.99']
deep_diff_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
lsrain_levels=['0.04','0.08','0.12','0.16','0.20','0.24','0.28','0.32','0.36','0.40','0.44','0.48','0.52']
lsrain_diff_levels=['-0.30','-0.26','-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22','0.26','0.30']

n_sets=4
n_variables=8

FOR j=0,n_variables-1 DO BEGIN
   CASE j OF
      1 : BEGIN
         varname='rain'
         ncvarname='rain'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=86400.
         levels=['0.5','1.0','1.5','2.0','3.0','4.0','5.0','6.0','8.0','10.0','12.0','14.0','16.0','18.0','21.0']
         diff_levels=['-5.5','-4.5','-3.5','-2.75','-2.0','-1.25','-0.75','-0.25','0.25','0.75','1.25','2.0','2.75','3.5','4.5','5.5']         
;         scatter_xrange=[0,35]
;         scatter_yrange=[-12,12]
         scatter_xrange=[0,25]
         scatter_yrange=[-10,10]         
         cb_title='Precipitation (mm day!U-1!N)'
      END
      2 : BEGIN
         varname='precip_shallowconv'
         ncvarname='precip_1'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=86400.
         levels=['0.03','0.06','0.09','0.12','0.15','0.18','0.21','0.24','0.27','0.30','0.33','0.36','0.39']
         diff_levels=['-0.22','-0.18','-0.14','-0.10','-0.06','-0.02','0.02','0.06','0.10','0.14','0.18','0.22']
         cb_title='Precipitation (mm day!U-1!N)'
;         scatter_xrange=[0,1]
;         scatter_yrange=[-0.5,0.5]
         scatter_xrange=[0,0.5]
         scatter_yrange=[-0.4,0.4]
      END
      3 : BEGIN
         varname='precip_midconv'
         ncvarname='precip_2'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=86400.
         levels=['0.1','0.2','0.3','0.4','0.6','0.8','1.0','1.2','1.5','1.8','2.1','2.4','2.8','3.2','3.6']
         diff_levels=['-1.5','-1.3','-1.1','-0.9','-0.7','-0.5','-0.3','-0.1','0.1','0.3','0.5','0.7','0.9','1.1','1.3','1.5']
         cb_title='Precipitation (mm day!U-1!N)'
;         scatter_xrange=[0,5]
;         scatter_yrange=[-3,3]
         scatter_xrange=[0,4]
         scatter_yrange=[-2,2]
      END
      4 : BEGIN
         varname='precip_deepconv'
         ncvarname='precip'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=86400.
         levels=['1.0','2.0','3.0','4.0','5.0','6.0','7.0','8.0','10.0','12.0','14.0','16.0','18.0']
         diff_levels=['-5.5','-4.5','-3.5','-2.75','-2.0','-1.25','-0.75','-0.25','0.25','0.75','1.25','2.0','2.75','3.5','4.5','5.5']
         cb_title='Precipitation (mm day!U-1!N)'
;         scatter_xrange=[0,35]
;         scatter_yrange=[-12,6]
         scatter_xrange=[0,25]
         scatter_yrange=[-10,10]
      END
      5 : BEGIN
         varname='lsrain'
         ncvarname='lsrain'
         longitude_var='longitude'
         latitude_var='latitude_1'
         multiplier=86400.
         levels=['0.2','0.4','0.6','0.8','1.0','1.2','1.4','1.6','1.8','2.0','2.2','2.4','2.6','2.8','3.0']
         diff_levels=['-1.95','-1.65','-1.35','-1.05','-0.75','-0.45','-0.15','0.15','0.45','0.75','1.05','1.35','1.65','1.95']
         cb_title='Precipitation (mm day!U-1!N)'
;         scatter_xrange=[0,4]
;         scatter_yrange=[-1,3]
         scatter_xrange=[0,3]
         scatter_yrange=[-1,2.5]
      END
      6 : BEGIN
         varname='indic_shallowconv'
         ncvarname='field1598'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=1.
         levels=['0.05','0.10','0.15','0.20','0.25','0.30','0.35','0.40','0.45','0.50','0.55','0.60','0.65','0.70']
         diff_levels=['-0.15','-0.13','-0.11','-0.09','-0.07','-0.05','-0.03','-0.01','0.01','0.03','0.05','0.07','0.09','0.11','0.13','0.15']
         cb_title='Fraction of timesteps'
;         scatter_xrange=[0,1.0]
;         scatter_yrange=[-0.5,0.5]
         scatter_xrange=[0,1.0]
         scatter_yrange=[-0.4,0.4]
      END
      7 : BEGIN
         varname='indic_midconv'
         ncvarname='field1934'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=1.
         levels=['0.02','0.04','0.06','0.08','0.10','0.12','0.14','0.16','0.18','0.20','0.22','0.24','0.26','0.28']
         diff_levels=['-0.075','-0.065','-0.055','-0.045','-0.035','-0.025','-0.015','-0.005','0.005','0.015','0.025','0.035','0.045','0.055','0.065','0.075']
         cb_title='Fraction of timesteps'
;         scatter_xrange=[0,0.4]
;         scatter_yrange=[-0.2,0.2]
         scatter_xrange=[0,0.25]
         scatter_yrange=[-0.15,0.15]
      END
      0 : BEGIN
         varname='cape_tscale_deep'
         ncvarname='unspecified_10'
         longitude_var='longitude_1'
         latitude_var='latitude'
         multiplier=1./(5400.)
         levels=['0.04','0.08','0.12','0.16','0.20','0.24','0.28','0.32','0.36','0.40','0.44','0.48','0.52','0.56']
         diff_levels=['-0.225','-0.195','-0.165','-0.135','-0.105','-0.075','-0.045','-0.015',$
                      '0.015','0.045','0.075','0.105','0.135','0.165','0.195','0.225']
         cb_title='Fraction of timesteps'
;         scatter_xrange=[0,0.7]
;         scatter_yrange=[-0.3,0.4]
         scatter_xrange=[0,0.5]
         scatter_yrange=[-0.4,0.4]
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
;   n_cases=2
      print,'-------> '+set_name
      
      FOR k=0,n_cases-1 DO BEGIN
         infile=um3+'/'+runids(k)+'.new_stash/'+runids(k)+'.'+date_ranges(k)+'_3hrmeans.'+varname+'.nc'
         longitude=OPEN_AND_EXTRACT(infile,longitude_var)
         latitude=OPEN_AND_EXTRACT(infile,latitude_var)
         DEFINE_BOUNDARIES,box,latitude,longitude,box_tx,/LIMIT
         n_lon=N_ELEMENTS(longitude)
         n_lat=N_ELEMENTS(latitude)
         
         IF k eq 0 THEN BEGIN
            composite_var=fltarr(n_lon,n_lat)
         ENDIF
         
         var=REFORM(OPEN_AND_EXTRACT(infile,ncvarname,offset=[box_tx(1),box_tx(0),0,time_offset],$
                                     count=[n_lon,n_lat,1,n_times]))*multiplier
         FOR m=0,n_lon-1 DO $
            FOR n=0,n_lat-1 DO $
               composite_var(m,n)=composite_var(m,n)+MEAN(var(m,n,*))*1./FLOAT(n_cases)
      ENDFOR      
      IF i eq 0 THEN $
         control_composite_var=composite_var
      
      psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_precip_composites_firstday.'+varname+'.hours'+$
             hour_range+'.'+set_name+'.ps'
      PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
             SPACE2=700   
      CS,SCALE=2,NCOLS=N_ELEMENTS(levels)+1,white=[2],/REV
      LEVS,MANUAL=levels
      MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
      CON,X=longitude,Y=latitude,FIELD=composite_var,/NOLINES,/BLOCK,$
          TITLE='Composite of '+varname+' from '+set_name+' for hours '+hour_range+' of phase 2 cases',CB_TITLE=cb_title
      AXES
      PSCLOSE,/NOVIEW

      IF i gt 0 THEN BEGIN
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_precip_composites_firstday.'+varname+'.hours'+$
                hour_range+'.'+set_name+'-minus-control.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1200,SPACE3=500,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
                SPACE2=700   
         CS,SCALE=1,NCOLS=N_ELEMENTS(diff_levels)+1,/REV
         LEVS,MANUAL=diff_levels
         MAP,LONMIN=box(1),LONMAX=box(3),LATMIN=box(0),LATMAX=box(2),/hires
         CON,X=longitude,Y=latitude,FIELD=composite_var-control_composite_var,/NOLINES,/BLOCK,$
             TITLE='Difference in composite of '+varname+' for '+set_name+' minus control for hours '+hour_range+' of phase 2 cases',$
             CB_TITLE=cb_title
         AXES
         PSCLOSE,/NOVIEW
         
         psfile='/home/ss901165/idl/hadgem3-kpp_runs/initialized_simulations/precip_types/hadgem3kpp_cascade_phase2_precip_composites_firstday.'+varname+'.hours'+$
                hour_range+'.'+set_name+'-minus-control_scatter.ps'
         PSOPEN,file=psfile,FONT=2,CHARSIZE=100,MARGIN=1700,SPACE3=400,XOFFSET=1000,YOFFSET=200,TFONT=2,TCHARSIZE=100,$
                SPACE2=500
         GSET,XMIN=scatter_xrange(0),XMAX=scatter_xrange(1),YMIN=scatter_yrange(0),YMAX=scatter_yrange(1),$
              TITLE=varname+' in control simulation versus difference for '+set_name+' minus control for hours '+hour_range+' of phase 2 cases'
         GPLOT,X=REFORM(control_composite_var,[n_lon*n_lat]),Y=REFORM(composite_var-control_composite_var,[n_lon*n_lat]),SYM=3,/NOLINES,SIZE=30
         aspect=(ABS(scatter_yrange(1))+ABS(scatter_yrange(0)))/(ABS(scatter_xrange(1))+ABS(scatter_xrange(0)))
         GPLOT,X=[scatter_xrange(0),scatter_xrange(1)],Y=[0,0],STYLE=0
         ;GPLOT,X=[0,MIN([scatter_xrange(1)*aspect,scatter_yrange(1)])],Y=[0,scatter_yrange(1)],STYLE=2
         ;GPLOT,X=[0,MIN([scatter_xrange(1)*aspect,scatter_yrange(1)])],Y=[0,scatter_yrange(0)],STYLE=2
         GPLOT,X=[0,MIN([scatter_xrange(1),scatter_yrange(1)*2])],Y=[0,MIN([scatter_xrange(1),scatter_yrange(1)*2])*0.5],STYLE=2
         GPLOT,X=[0,MIN([scatter_xrange(1),scatter_yrange(0)*(-2)])],Y=[0,MIN([scatter_xrange(1),scatter_yrange(0)*2])*0.5],STYLE=2
         GPLOT,X=[0,MIN([scatter_xrange(1),scatter_yrange(1)*4])],Y=[0,MIN([scatter_xrange(1),scatter_yrange(1)*2])*0.25],STYLE=2
         GPLOT,X=[0,MIN([scatter_xrange(1),scatter_yrange(0)*(-4)])],Y=[0,MIN([scatter_xrange(1),scatter_yrange(0)*2])*0.25],STYLE=2
         AXES,XSTEP=(scatter_xrange(1)-scatter_xrange(0))/10.,YSTEP=(scatter_yrange(1)-scatter_yrange(0))/10.,$
              XMINOR=(scatter_xrange(1)-scatter_xrange(0))/20.,YMINOR=(scatter_yrange(1)-scatter_yrange(0))/20.,$
              YTITLE='Difference in '+varname+': '+set_name+' minus control',XTITLE=varname+' from control simulation',NDECS=2
         PSCLOSE,/NOVIEW
      ENDIF
   ENDFOR
ENDFOR

STOP
END
