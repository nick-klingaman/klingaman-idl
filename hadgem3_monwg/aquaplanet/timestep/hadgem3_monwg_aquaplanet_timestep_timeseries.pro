PRO hadgem3_monwg_aquaplanet_timestep_timeseries

pt=[2.5,75]
timestart=1000
n_times=144
n_sets=3
model_names=strarr(n_sets)
colors=strarr(n_sets)
styles=intarr(n_sets)

FOR i=0,n_sets-1 DO BEGIN
   CASE i OF
      0 : BEGIN
         infile='/home/ss901165/um_output5/xhccr/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_ctlent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='nhsol5N_ctlent'
         colors(i)='black'
         styles(i)=0
         a=2
      END
      1 : BEGIN
         infile='/home/ss901165/um_output5/xhccs/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_1.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='nhsol5N_1.5xent'
         colors(i)='red'
         styles(i)=0
         a=1
      END
      2 : BEGIN
         infile='/home/ss901165/um_output5/xhcct/daily/hadgem3a_ga30_aqua_fixnhsol5N_sin2sin4_2.5xent.jan-dec_dmeans.years1-3.tot_precip.nc'
         model_names(i)='nhsol5N_2.5xent'
         colors(i)='orange'
         styles(i)=0
      END
   ENDCASE
   
   longitude=OPEN_AND_EXTRACT(infile,'longitude')
   latitude=OPEN_AND_EXTRACT(infile,'latitude')
   lonpt=NEAREST(longitude,pt(1))
   latpt=NEAREST(latitude,pt(0))
   
   precip_in=REFORM(OPEN_AND_EXTRACT(infile,'tot_precip',$
                                     offset=[lonpt,latpt,timestart],count=[1,1,n_times]))*a

   psfile='/home/ss901165/idl/hadgem3_monwg/aquaplanet/timestep/hadgem3_monwg_aquaplanet_timestep_timeseries.'+model_names(i)+'.ps'
   PSOPEN,file=psfile,FONT=6,TFONT=6,CHARSIZE=150,MARGIN=2500,YOFFSET=1000,XOFFSET=1000
   GSET,XMIN=1,XMAX=n_times,YMIN=0.,YMAX=2.5
;   GPLOT,X=indgen(n_times)+0.5,Y=precip_in
   HIST,X=indgen(n_times)+1,Y=precip_in,WIDTH=20,FILLCOL=FSC_COLOR('black')
   GPLOT,X=indgen(n_times)+1,Y=precip_in,/NOLINES,SYM=3,COL=FSC_COLOR('red'),SIZE=70
   AXES,XSTEP=10,XMINOR=2,YSTEP=0.25,YMINOR=0.05,$
        YTITLE='Precipitation (mm timestep!U-1!N)',XTITLE='Timestep (20 minutes)',NDECS=2
   PSCLOSE
  
ENDFOR

STOP
END
