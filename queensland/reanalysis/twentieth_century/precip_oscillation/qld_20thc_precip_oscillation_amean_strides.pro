PRO qld_20thc_precip_oscillation_amean_strides
  
; Plot the difference between taking annual means of rainfall from the
; 20th century reanalysis as 0,[year_end],2 and 1,[year_end],2.

; File containing differences
stride_diff_file='/home/ss901165/datasets_mango/20THC_REANALYSIS/diff_first_second_1891.nc'

; Get longitude, longitude
acre_longitude=OPEN_AND_EXTRACT(stride_diff_file,'longitude')
acre_latitude=OPEN_AND_EXTRACT(stride_diff_file,'latitude')
acre_nlon=N_ELEMENTS(acre_longitude)
acre_nlat=N_ELEMENTS(acre_latitude)

; Get rainfall differences
acre_precip_stride_diff=REFORM(OPEN_AND_EXTRACT(stride_diff_file,'PRATE',$
                                                offset=[0,0,0,0],count=[acre_nlon,acre_nlat,1,1]))*86400.

; Make plot
psfile='/home/ss901165/idl/queensland/reanalysis/twentieth_century/precip_oscillation/qld_20thc_precip_oscillation_amean_strides.1891.ps'

mylevs_diff=['-5.5','-4.5','-3.5','-2.5','-1.5','-0.5','0.5','1.5','2.5','3.5','4.5','5.5']

PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1500,SPACE2=300,XOFFSET=500,YOFFSET=500,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
CS,SCALE=26,NCOLS=N_ELEMENTS(mylevs_diff)+1,/REV,white=[8];white=[9,10],/REV
MAP,/HIRES
LEVS,MANUAL=mylevs_diff
CON,FIELD=acre_precip_stride_diff,X=acre_longitude,Y=acre_latitude,TITLE='Difference between taking annual means from [0,2919,2] and [1,2919,2] for 1891 (20th Cent.; mm/day)',/NOLINELABELS,/NOLINES
PSCLOSE

STOP
END
