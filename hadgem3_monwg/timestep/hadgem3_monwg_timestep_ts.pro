PRO hadgem3_monwg_timestep_ts

pt=[-5,60]
amzgg='/home/ss901165/um_output3/hadgem3_monwg/amzgg'
anbba='/home/ss901165/um_output3/hadgem3_monwg/anbba'
cascade='/home/ss901165/um_output6/cascade'

n_sets=3
FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.nc'
         varname='precip'
         plot_title='anbba (GA5.0, N512) total precip tstep'
         psfile_title='anbba_totprecip'
         start_read=0
         n_time=720
         n_plots=3
         multiplier=86400.
         n_times_per_day=144
      END
      1 : BEGIN
         infile=anbba+'/anbbaa.jun-sep_tsmeans.1982.precip.nc'
         varname='precip'
         plot_title='anbba (GA5.0, N512) total precip tstep'
         psfile_title='anbba_totprecip_equaltime'
         start_read=0
         n_time=1440
         n_plots=3
         multiplier=86400.
         n_times_per_day=144
      END
      2 : BEGIN
         infile=amzgg+'/amzgga.jun-sep_tsmeans.1982.precip.nc'
         varname='precip'
         plot_title='amzgg (GA5.0, N96) total precip tstep'
         psfile_title='amzgg_totprecip'
         start_read=0
         n_time=720
         n_plots=3
         multiplier=86400.
         n_times_per_day=72
      END
   ENDCASE

   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   lonpt=NEAREST(longitude,pt(1))
   latpt=NEAREST(latitude,pt(0))
   
   precip_ts=REFORM(OPEN_AND_EXTRACT(infile,varname,$
                                     offset=[lonpt,latpt,start_read],$
                                     count=[1,1,n_time*n_plots]))*multiplier

   psfile='/home/ss901165/idl/hadgem3_monwg/timestep/hadgem3_monwg_timestep_ts.'+psfile_title+'_5S60E.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=170,SPACE3=50,MARGIN=1500,XOFFSET=1500,SPACE2=50,YOFFSET=1000,YPLOTS=n_plots,/PORTRAIT,YSPACING=1000

   precip_ts[where(precip_ts lt 0.1)]=0.1
   FOR j=0,n_plots-1 DO BEGIN
      POS,ypos=n_plots-j
      IF j eq 0 THEN BEGIN
         GSET,XMIN=j*n_time,XMAX=(j+1)*n_time,YMIN=0.1,YMAX=400,/YLOG,TITLE=plot_title+' (5S,60E)'
      ENDIF ELSE $
         GSET,XMIN=j*n_time,XMAX=(j+1)*n_time,YMIN=0.1,YMAX=400,/YLOG
      black=FSC_COLOR('black',30)
      red=FSC_COLOR('red',31)
      HIST,X=indgen(n_time)+j*n_time+0.5,Y=precip_ts(j*n_time:(j+1)*n_time-1),FILLCOL=30,WIDTH=5
      AXES,XSTEP=n_times_per_day,XMINOR=n_times_per_day/2,$
           YVALS=['0.1','0.2','0.4','0.6','1.0',$
                  '2.0','4.0','6.0','10',$
                  '20','40','60','100','200','400'],$
           YTITLE='Precipitation (mm day!U-1!N)',$
           XTITLE='Timestep',/NOUPPER,/NORIGHT
   ENDFOR
   PSCLOSE

ENDFOR

STOP
END
