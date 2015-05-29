PRO blank_australia_map

; For climate
;box_plot=[-60,70,40,290]

; For synoptic
;box_plot=[-55,110,-5,180]

; For synoptic (smaller)
;box_plot=[-50,130,-5,165]

; For climate (smaller)
box_plot=[-50,70,40,300]
n_lon=box_plot(3)-box_plot(1)+1
n_lat=box_plot(2)-box_plot(0)+1

psfile='/home/ss901165/idl/queensland/blank_australia_map_climate_smaller.ps'
PSOPEN,file=psfile,FONT=2,CHARSIZE=150,MARGIN=1000,SPACE2=300,XOFFSET=1000,YOFFSET=5000,TFONT=2,TCHARSIZE=100,CB_WIDTH=110
MAP,LONMIN=box_plot(1),LONMAX=box_plot(3),LATMIN=box_plot(0),LATMAX=box_plot(2),/CYLINDRICAL

dummy=fltarr(n_lon,n_lat)
dummy(*,*)=0.
LEVS,MANUAL=[0,1]
white=FSC_COLOR("white",2)
white=FSC_COLOR("white",3)
white=FSC_COLOR("white",4)
white=FSC_COLOR("white",5)
CON,FIELD=dummy,X=indgen(n_lon)+box_plot(1),Y=indgen(n_lat)+box_plot(0),/NOLINES,/NOCOLBAR,/NOAXES
AXES,XSTEP=30,YSTEP=10
GPLOT,X=153+2./60.,Y=-27-25./60,SYM=3,SIZE=50
GPLOT,X=154,Y=-27-25./60,TEXT="Brisbane",CHARSIZE=50,ALIGN=0.0
GPLOT,X=130+51./60.,Y=-12-25./60,SYM=3,SIZE=50
GPLOT,X=131.5,Y=-11-25./60,TEXT="Darwin",CHARSIZE=50,ALIGN=0.0
GPLOT,X=151+10./60.,Y=-33-53./60.,SYM=3,SIZE=50
GPLOT,X=152+50./60.,Y=-33-10./60.,TEXT="Sydney",CHARSIZE=50,ALIGN=0.0
GPLOT,X=145,Y=-37-50./60.,SYM=3,SIZE=50
GPLOT,X=145,Y=-39-50./60.,TEXT="Melbourne",CHARSIZE=50,ALIGN=0.0
GPLOT,X=115+52./60.,Y=-31-57./60.,SYM=3,SIZE=50
GPLOT,X=116+52./60.,Y=-31-57./60.,TEXT="Perth",CHARSIZE=50,ALIGN=0.0
GPLOT,X=145+45./60.,Y=-16-57./60.,SYM=3,SIZE=50
GPLOT,X=146+46./60.,Y=-16-57./60.,TEXT="Cairns",CHARSIZE=50,ALIGN=0.0
GPLOT,X=145+16./60.,Y=-15-30./60.,SYM=3,SIZE=50
GPLOT,X=146+16./60.,Y=-15-30./60.,TEXT="Cooktown",CHARSIZE=50,ALIGN=0.0
GPLOT,X=138+30./60.,Y=-34-52./60.,SYM=3,SIZE=50
GPLOT,X=139+30./60.,Y=-34-52./60.,TEXT="Adelaide",CHARSIZE=50,ALIGN=0.0
GPLOT,X=133+50./60.,Y=-23-40./60.,SYM=3,SIZE=50
GPLOT,X=134+50./60.,Y=-23-40./60.,TEXT="Alice Springs",CHARSIZE=50,ALIGN=0.0

PSCLOSE

STOP

END
