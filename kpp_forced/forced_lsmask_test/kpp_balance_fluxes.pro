PRO kpp_balance_fluxes

basedir='/home/ss901165/um_output2/xccbh_kpp/'
n_fluxes=4
fluxes=strarr(n_fluxes)
fluxes=['lh','sh','solar','longwave']
point=[21.666,143.75]

FOR i=0,n_fluxes-1 DO BEGIN
    infile=basedir+fluxes(i)+'_for_kpp.nc'
    id=NCDF_OPEN(infile)
    varid=NCDF_VARID(id,fluxes(i))
    IF i eq 0 THEN BEGIN
        latid=NCDF_VARID(id,'latitude')
        lonid=NCDF_VARID(id,'longitude')
        NCDF_VARGET,id,latid,latitudes
        NCDF_VARGET,id,lonid,longitudes
        latpoint=NEAREST(latitudes,point(0))
        lonpoint=NEAREST(longitudes,point(1))
    ENDIF
    CASE i OF
        0 : NCDF_VARGET,id,varid,lh,offset=[lonpoint,latpoint,0,0],count=[1,1,1,1200]
        1 : NCDF_VARGET,id,varid,sh,offset=[lonpoint,latpoint,0,0],count=[1,1,1,1200]
        2 : NCDF_VARGET,id,varid,shortwave,offset=[lonpoint,latpoint,0,0],count=[1,1,1,1200]
        3 : NCDF_VARGET,id,varid,longwave,offset=[lonpoint,latpoint,0,0],count=[1,1,1,1200]
    ENDCASE
    NCDF_CLOSE,id
ENDFOR

lh = -lh
sh = -sh

id=NCDF_OPEN('/home/ss901165/kpp_ocean/forced_lsmask_test/KPPocean.T.nc')
varid=NCDF_VARID(id,'T')
latid=NCDF_VARID(id,'latitude')
lonid=NCDF_VARID(id,'longitude')
NCDF_VARGET,id,latid,latitudes
NCDF_VARGET,id,lonid,longitudes
latpoint=NEAREST(latitudes,point(0))
lonpoint=NEAREST(longitudes,point(1))
NCDF_VARGET,id,varid,temperature,offset=[lonpoint,latpoint,0,0],count=[1,1,1,1200]
NCDF_CLOSE,id

balance=lh+sh+shortwave+longwave

FOR i=0,4 DO BEGIN

    set_plot,'ps'
    device,file='/home/ss901165/kpp_ocean/forced_lsmask_test/kpp_balance_fluxes.'+STRTRIM(STRING(i),1)+'.ps',color=1,bits_per_pixel=24
    LoadCT,33,ncolors=16,bottom=2
    time=findgen(240)/8+i*30
    plot,time,balance(i*240:(i+1)*240-1),xtitle='Time (days; 0 = 1 May)',ytitle='Fluxes (W/m^2)',yrange=[-700,1000],ystyle=9,xrange=[i*30,(i+1)*30],xstyle=1,title='For point (21.66N,143.75E)'
    oplot,time,lh(i*240:(i+1)*240-1),color=6,linestyle=4
    oplot,time,sh(i*240:(i+1)*240-1),color=9,linestyle=2
    oplot,time,shortwave(i*240:(i+1)*240-1),color=14,linestyle=3
    oplot,time,longwave(i*240:(i+1)*240-1),color=16,linestyle=5
    oplot,time,REPLICATE(MEAN(balance(i*240:(i+1)*240-1)),N_ELEMENTS(time))
    AXIS,YAXIS=1,YRANGE=[25,40],YTITLE='Temperature (degC)', /SAVE
    oplot,time,temperature(i*240:(i+1)*240-1),color=17,linestyle=6
    items=['BALANCE','LHF','SHF','SWF','LWF','TEMP']
    legend,items,linestyle=[0,4,2,3,5,6],color=[0,6,9,12,15,17],/NORMAL,/BOTTOM,/RIGHT
    device,/close

ENDFOR

STOP

END
