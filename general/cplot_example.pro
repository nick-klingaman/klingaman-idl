pro cplot
	
	device, decomposed=0
	r=[255,0,0,0,0,0,66,184,245,255,255,255,230,191,148,105,161,184]
	g=[255,0,84,199,255,255,255,255,255,209,135,28,0,0,0,0,0,0]
	b=[255,0,255,255,255,178,0,0,0,0,0,0,0,0,0,20,135,186]
	tvlct, r,g,b
	mylevs=indgen(15)+20
	
	id=ncdf_open('/home/ss901165/datasets/temperature.cdf')
	tempid=ncdf_varid(id,'temp')
	ncdf_varget,id,tempid,temp

	map_set,-14.75,75.25,limit=[-29.75,30.25,-0.25,119.75],/cylindrical
	map_grid

	mylabs=intarr(8)+1
	mycolors=indgen(15)+3
	print,mycolors
	lats=-29.75+findgen(60)*0.5
	longs=30.25+findgen(180)*0.5
	
	contour,temp(*,*,1,1),longs,lats,levels=mylevs,$
		c_labels=mylabs,/overplot,/cell_fill, $
		xstyle=1,ystyle=1,color=1,c_colors=mycolors

	map_continents

end
