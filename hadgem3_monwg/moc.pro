PRO MOC

;NEED FIELD704

;v1='snowfall'
;v2='snowfall_amount'
;v3='snowdepth'
v4='field704'
;v5='sea_ice_area_fraction'
;v6='iceconc'

;need to integrate down to k=12 = 996.55m and across atlantic @26.25N (j=46)
; first point is -91.875 (i=71) last point is -24.375 (i=89)

;DX = cos(25N)*3.75*111km
;need to weigh the DZs

;MOC_index = SUM_depth ( SUM_lon ( V-VELOCITY))

;For Drake PAssage do three boxes to lev 17 and then only two at lev 18

;Units: need to go from cm/s to m/s
;1 SV is 10^6M3/s

; MOC EXPECT 16 SV
; for Drake Passage ACC EXPECT 140 SV

prefix=['d.','db.','dc.','od.','odb.','odc.']
 
nlat=37
nlon=48
ntime_ctl=500
ntime_exp=900
onlat=73
onlon=96
ndepth=20

;d=NCREAD('/home/ss901165/um_output5/xhhba/xhfada.py.1851-2050.nc',/SILENT,/VALID,VARS=[v1])
;db=NCREAD('/home/ss901165/um_output5/xhhba/xhhbaa.py.1851-2250.nc',/SILENT,/VALID,VARS=[v2])
;dc=NCREAD('xhemc/xhemca_py.nc',/SILENT,/VALID,VARS=[v3])
od=NCREAD('/home/ss901165/um_output5/xhfad/xhfado.py.1851-2350.nc',/SILENT,/NOMOD,VARS=[v4])
odb=NCREAD('/home/ss901165/um_output5/xhhba/xhhbao.py.1851-2750.nc',/SILENT,/NOMOD,VARS=[v4])
;odc=NCREAD('xhemc/xhemco_py.nc',/SILENT,/VALID,VARS=[v6])

nvarsa=3
nvarso=3
nvars=6
;vars=[v1,v2,v3,v4,v5,v6]

;a=FLTARR(nlon,nlat,ntime,nvarsa)
;o=FLTARR(onlon,onlat,ndepth,ntime,nvarso)
MOC=FLTARR(ntime_ctl)
MOC3=FLTARR(ntime_exp)

ACC=FLTARR(ntime_ctl)
ACC3=FLTARR(ntime_exp)

;var_mean=FLTARR(ntime,nvars)
;count=INTARR(nvars)

odelZ = FLTARR(ndepth)
odelZ = [10,10,10,10.2,15.3,23,34.5,51.8,77.8,116.8,175.3,263.2,395.3,615,615,615,615,615,616,616]

;com="a[*,*,*,0]="+prefix[0]+vars[0]+"(*,*,*)"
;res=execute(com)
;com="a[*,*,*,1]="+prefix[1]+vars[1]+"(*,*,*)"
;print,com
;res=execute(com)
;com="a[*,*,*,2]="+prefix[2]+vars[2]+"(*,*,0,*)"
;res=execute(com)
;com="o[*,*,*,0]="+prefix[3]+vars[3]+"(*,*,*)"
;res=execute(com)
;com="o[*,*,*,1]="+prefix[4]+vars[4]+"(*,*,*)"
;res=execute(com)
;com="o[*,*,*,2]="+prefix[5]+vars[5]+"(*,*,*,*)"
;res=execute(com)

;odc=NCREAD('xhemc/xhemco_py.nc',/SILENT,/NOMOD,VARS='field704')
;od=NCREAD('xhfad/xhfado.py.1851-2050.nc',/SILENT,/NOMOD,VARS='northward_sea_water_velocity')
;odb=NCREAD('xhemb/xhembo_py.nc',/SILENT,/NOMOD,VARS='northward_sea_water_velocity')

;odc.field704(WHERE (odc.field704 EQ 2.E20)) = 0.
od.field704(WHERE (od.field704 EQ 2.E20)) = 0.
odb.field704(WHERE (odb.field704 EQ 2.E20)) = 0.

MOC(*)=0.
;MOC2(*)=0.
MOC3(*)=0.

for it=0,ntime_ctl-1 DO BEGIN
for k = 0,12 DO BEGIN
for i = 71,89 DO BEGIN
;for k = 0,19 DO BEGIN
;MOC2(it) = MOC2(it) + 1.E-6 * 1.E-2*odc.field704(i,46,k,it) * cos(26.25*3.1415/180) * 3.75* 1.11E5 * odelZ(k)
MOC(it) = MOC(it) + 1.E-6 * 1.E-2*od.field704(i,46,k,it) * cos(26.25*3.1415/180) * 3.75* 1.11E5 * odelZ(k)
;MOC3(it) = MOC3(it) + 1.E-6 * 1.E-2*odb.field704(i,46,k,it) * cos(26.25*3.1415/180) * 3.75* 1.11E5 * odelZ(k)
ENDFOR
;print,MOC, MOC2
ENDFOR
ENDFOR

FOR it=0,ntime_exp-1 DO $
FOR k=0,12 DO $
FOR i=71,89 DO $
MOC3(it) = MOC3(it) + 1.E-6 * 1.E-2*odb.field704(i,46,k,it)*cos(26.25*3.1415/180)*3.75*1.11E5*odelZ(k)

ytime=1850.5+indgen(ntime_exp)

PSOPEN, THICK=200, CHARSIZE=140, FILE="FAMOUS_AMOC.ps"
CS, SCALE=1
GSET, XMIN=1850, XMAX=MAX(ytime), YMIN=0, YMAX=40, TITLE='FAMOUS AMOC at 25N'
;labels=['Minimum', 'Maximum']

AXES, XVALS=INDGEN(ntime_exp)*1+1850, XSTEP=25, ORIENTATION=90, XTITLE='Year', YSTEP=2.5, YTITLE='MOC (SV)',/NORIGHT, NDECS=1

GPLOT, X=ytime(0:ntime_ctl-1), Y=MOC(0:ntime_ctl-1), COL=5 
;GPLOT, X=ytime(0:ntime-1), Y=MOC2(0:ntime-1), COL=9 
GPLOT, X=ytime(0:ntime_exp-1), Y=MOC3(0:ntime_exp-1), COL=2 

;GLEGEND, SIZE=30, LENGTH=60, LEGPOS=1, COL=[19,14,17,5,9,2], LABELS=['Land T, CTL','Land T, 5*S_RESP','Land T, 0.2*S_RESP','Ocean T, CTL','Ocean T, 5*RESP_S','Ocean T, 0.2*RESP_S']
GLEGEND, SIZE=30, LENGTH=60, LEGPOS=1, COL=[5,2], LABELS=['AMOC CTL','AMOC OCNMIX*25']

PSCLOSE,/NOVIEW

;For Drake PAssage do three boxes to lev 17 and then only two at lev 18
;Use longitude 65.625W(78)
;Use latitudes -58.75(12) -61.25(11)

;odc=NCREAD('xhemc/xhemco_py.nc',/SILENT,/NOMOD,VARS='field703')
od=NCREAD('/home/ss901165/um_output5/xhfad/xhfado.py.1851-2350.nc',/SILENT,/NOMOD,VARS='field703')
odb=NCREAD('/home/ss901165/um_output5/xhhba/xhhbao.py.1851-2750.nc',/SILENT,/NOMOD,VARS='field703')

;odc.field703(WHERE (odc.field703 EQ 2.E20)) = 0.
;od.field703(WHERE (od.eastward_sea_water_velocity LT -1.E9)) = 0.
;odb.eastward_sea_water_velocity(WHERE (odb.eastward_sea_water_velocity LT -1.E9)) = 0.

ACC(*)=0.
;ACC2(*)=0.
ACC3(*)=0.

od.field703[where(od.field703 EQ 2E20)]=0.
odb.field703[where(odb.field703 EQ 2E20)]=0.

for it=0,ntime_ctl-1 DO BEGIN
for k = 0,19 DO BEGIN
for j = 11,12 DO BEGIN
;ACC2(it) = ACC2(it) + 1.E-6 * 1.E-2*odc.field703(78,j,k,it) * 2.5 * 1.11E5 * odelZ(k)
ACC(it) = ACC(it) + 1.E-6 * 1.E-2*od.field703(78,j,k,it) * 2.5 * 1.11E5 * odelZ(k)
;ACC3(it) = ACC3(it) + 1.E-6 * 1.E-2*odb.field703(78,j,k,it) * 2.5 * 1.11E5 * odelZ(k)
ENDFOR
;print,ACC, ACC2, ACC3
ENDFOR
ENDFOR

FOR it=0,ntime_exp-1 DO $
FOR k=0,19 DO $
FOR j=11,12 DO $
ACC3(it)=ACC3(it)+1.E-6*1.E-2*odb.field703(78,j,k,it)*2.5*1.11E5*odelZ(k)

PSOPEN, THICK=200, CHARSIZE=140, FILE="FAMOUS_ACC.ps"
CS, SCALE=1
GSET, XMIN=1850, XMAX=MAX(ytime), YMIN=15, YMAX=140, TITLE='FAMOUS ACC'
;labels=['Minimum', 'Maximum']

AXES, XVALS=INDGEN(ntime_exp)*1+1850.5, XSTEP=50, XMINOR=25, ORIENTATION=90, XTITLE='Year', YSTEP=20, YTITLE='ACC (SV)',/NORIGHT, NDECS=1

print,ACC,ACC3
GPLOT, X=ytime(0:ntime_ctl-1), Y=ACC(0:ntime_ctl-1), COL=5 
GPLOT, X=ytime(0:ntime_exp-1), Y=ACC3(0:ntime_exp-1), COL=2 

GLEGEND, SIZE=30, LENGTH=60, LEGPOS=1, COL=[5,2], LABELS=['ACC CTL','ACC OCNMIX*25']

PSCLOSE,/NOVIEW

STOP
END

