PRO qld_eraint_precip_oscillation_amean_strides

; Plot the difference between taking annual means of rainfall from
; ERA-Interim as 0,[year_end],4 and 3,[year_end],4.  Basically, the
; difference in annual-mean rainfall for a 3 hour forecast against a
; 12 hour forecast.

; File containing differences
stride_diff_file='/home/ss901165/datasets_mango/ERA-INTERIM/TP/ERA_interim.jan-dec_amean.1989.TP_3hfcst-minus-12hfcst.nc'

; Get longitude, longitude
eraint_longitude=OPEN_AND_EXTRACT(stride_diff_file,'longitude')
eraint_latitude=OPEN_AND_EXTRACT(stride_diff_file,'latitude')
eraint_nlon=N_ELEMENTS(eraint_longitude)
eraint_nlat=N_ELEMENTS(eraint_latitude)

; Get rainfall differences
eraint_precip_stride_diff=REFORM(OPEN_AND_EXTRACT(stride_diff_file,'TP',$
                                                offset=[0,0,0],count=[eraint_nlon,eraint_nlat,1]))*86400.

; Make plot
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_oscillation/qld_eraint_precip_oscillation_amean_strides.1891.ps'

mylevs_diff=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']

PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[8];white=[9,10],/REV
MAP,/HIRES
LEVS,MANUAL=mylevs_diff
CON,FIELD=eraint_precip_stride_diff,X=eraint_longitude,Y=eraint_latitude,TITLE='Difference between taking annual means from [0,2919,4] and [3,2919,4] for 1989 (ERA-Interim; mm/day)',/NOLINELABELS,/NOLINES
PSCLOSE

STOP
END
