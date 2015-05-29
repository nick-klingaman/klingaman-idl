pro mapp_set3,latmin,latmax,lonmin,lonmax,p0lon,p0lat,rot,$
	iproj,limit,t3d=t3d,border=border,title=title,sat_p=sat_p,$
	xmargin=xmargin,ymargin=ymargin,position=position,save=save

!x.s=[0,1]		;normal coverage for uv plane
!y.s=[0,1]


if iproj eq 3 then begin		;Conic?
  !map.phioc=rot		;Parameter order is diff
  !map.p0lat=(p0lat + p0lon)/2.	;Center latitude
  if !map.p0lat lt 0.0 then !map.sino=-1 else !map.sino=1
  chi1=(90. - !map.sino * p0lat) * !dtor
  if p0lat eq p0lon then begin
    !map.coso=cos(chi1)
  endif else begin
    chi2=(90. - !map.sino * p0lon) * !dtor
    !map.coso=alog(sin(chi1)/sin(chi2))/ $
       alog(tan(0.5*chi1)/tan(0.5*chi2))
  endelse
  !map.out(8)=p0lat * !dtor	;Ucen
  !map.out(9)=p0lon * !dtor	;Vcen
  goto,get_bounds
endif

;		Not conic, all other projections

x1=rot * !dtor
x2=p0lat * !dtor

if (iproj ge 8) then  BEGIN
 !map.out(8)=x1		;ucen
 !map.out(11)=x2		;vrng
ENDIF ELSE !map.out(8)=x2

!map.out(9)=p0lon * !dtor		;vcen
sinr=sin(x1)			;Stash cos / sin of map center lat and rotation
cosr=cos(x1)
sino=sin(x2)
coso=cos(x2)
del1=.0001
!map.sat=0.0		;Clear satellite proj params

if iproj ge 8 and iproj ne 14 then begin   ;Cylindrical projections
  if (abs(p0lat) ge del1) or $	;Simple case?
     ((abs(rot) ge del1) and (abs(abs(rot)-180.) ge del1))  THEN BEGIN
    sint=coso * cosr          ;NO...  complicated case
    cost=sqrt(1.-sint*sint)
    tmp1= sinr / cost
    tmp2= sino / cost
    !map.phioc=p0lon - atan(tmp1, - cosr * tmp2) * !radeg
    sinr=tmp1 * coso
    cosr=-tmp2
    sino=sint
    coso=cost
  endif else begin		;Simple cases
    if abs(rot) lt del1 then sino=1.0 else begin 
      sino=-1.
      !map.phioc=p0lon + 180.
    endelse
    coso=0.0
    sinr=0.0
    cosr=1.0
    !map.projection=iproj+3	;Make a simple projection
  endelse
endif			;Cylindrical projections

if iproj eq 7 then begin ;Special params for satellite projection.
  !map.sat(0)=sat_p(0) ;Salt=sat_p(0)
                         ;Save em.  sat(1)=TRUE for Vertical perspective
  !map.sat(1)=(sat_p(1) eq 0.0) and (sat_p(2) eq 0.0)
  !map.sat(2)=sin(!dtor * sat_p(1)) ;Salpha=sat_p(1)
  !map.sat(3)=cos(!dtor * sat_p(1)) ;calpha
  !map.sat(4)=sin(!dtor * sat_p(2)) ;Sbeta=sat_p(2)
  !map.sat(5)=cos(!dtor * sat_p(2)) ;cbeta
endif	

!map.coso= coso
!map.sinr=sinr
!map.sino=sino
!map.cosr=cosr
if (iproj eq 2) then !map.sat(0)=0;


GET_BOUNDS:

if n_elements(limit) ne 8 then begin	;Explicit limits?
  ;	  stereo	ortho	     conic
  bounds=[[-2,-2,2,2], [-1,-1,1,1], [-180,-1,180,1]]
  ;			lamb	gnomic		azimuthal
  bounds=[[bounds], [-2,-2,2,2],[-2,-2,2,2],[-!pi,-!pi,!pi,!pi]]
  ;			satell	cylindrical	mercator
  bounds=[[bounds], [-1,-1,1,1],[-180,-90,180,90],[-!pi,-!pi,!pi,!pi]]
  ;			mollweide  Sinusoidal            
  bounds=[[bounds], [-2,-1,2,1], [-!pi,-!pi/2,!pi,!pi/2]]
  ;                       aitoff
  bounds=[[bounds], [-2*sqrt(2), -sqrt(2), 2*sqrt(2), sqrt(2)]]
  
  iproj1=(iproj < 11) - 1
  plimit=bounds(*,iproj1)
  umin=plimit(2)      ;MAXIMUM USEFUL AREA, xmax
  vmin=plimit(3)	;ymax
  umax=plimit(0)	;xmin
  vmax=plimit(1)	;ymin
  
  latdel=(latmax-latmin)/10.
  if latdel gt 10. THEN BEGIN  ;We dont want stepsizes too large
    del=float(fix(latmax-latmin)/10)
    latdel=(latmax-latmin)/del
  ENDIF
   
  londel=(lonmax - lonmin)/10.
  if londel gt 10. THEN BEGIN
    del=float(fix(lonmax - lonmin)/10)
    londel=(lonmax - lonmin)/del
  ENDIF
  
  eps=1.e-5 
  latlim=latmax + (latmax-latmin)*eps
  lonlim=lonmax + (lonmax-lonmin)*eps
  
  ;Find max u and v coordinates
  
  for lat=float(latmin), latlim, latdel  do $  
  for lon=float(lonmin), lonlim, londel do begin
    p00=convert_coord(lon,lat, /to_norm)
    p0=!map.out(0:1)         ; (p,q in uv coordinates)
;	print, lon, lat, p0
    if (p0(0) ge plimit(0) and p0(0) le plimit(2) and $
        p0(1) ge plimit(1) and p0(1) le plimit(3)) THEN BEGIN
      umin=umin < p0(0)	;Smallest area
      umax=umax > p0(0)
      vmin=vmin < p0(1)
      vmax=vmax > p0(1)
    ENDIF
  ENDFOR


ENDIF ELSE BEGIN  			;4 point (8 element) limit specified

; limit =  [ lat0, lon0, lat1, lon1, lat2, lon2, lat3, lon3] specify
; four points on the map which give, respectively,
; the leftmost, topmost, rightmost, and bottom
; most points to be shown.

; print,'Limit = ', limit
; print, latmin, latmax, lonmin, lonmax
  p0=fltarr(2,4)		;for the 4 corners
  for i=0,3 do begin
    lat=limit(2*i)
    lon=limit(2*i+1)
    p1=convert_coord(lon, lat, /to_norm)
    str=strtrim(lat,2) + ',' + strtrim(lon,2)
    if max(!map.out(0:1)) ge 1e3 then message, $
       'Map_set3, limit point not mappable, lat,lon='+str
    p0(0,i)=!map.out(0:1)     ;Get uv coordinates
;		print, i, p0(*,i)
  endfor
  umin=p0(0,0)
  umax=p0(0,2)
  vmin=p0(1,1)
  vmax=p0(1,3)
ENDELSE



if iproj eq 3 then  BEGIN 
  ueps=135.                   ;For conical
  umin= -1 & umax=1
endif  else ueps=0.75 * ABS(umax-umin)
veps=0.75 * ABS(vmax-vmin)
  
!map.out(6)=ueps
!map.out(7)=veps

if n_elements(xmargin) ne 2 THEN xmargin=[1,1]
if n_elements(ymargin) ne 2 THEN ymargin=[1,2]
if not N_Elements(title) THEN title=" "
;
; This establishes the plot scaling between uv coordinates and the 
;  plot range.

if keyword_set(position) then  begin
  if n_elements(position) ne 4 then begin
    xsz=!x.window(1)-!x.window(0)
    ysz=!y.window(1)-!y.window(0)
    pos=[!x.window(0),!y.window(0),!x.window(0)+xsz,!y.window(0)+ysz]
  endif else begin
    pos=position
  endelse          
  plot, [umin,umax], [vmin,vmax], xsty=5, ysty=5, /nodata,    $
     xmargin= xmargin, ymargin= ymargin, title=title, font=0,/noerase,$
     position=pos
endif else begin
  plot, [umin,umax], [vmin,vmax], xsty=5, ysty=5, /nodata,    $
     xmargin= xmargin, ymargin= ymargin, title=title, font=0, /noerase
endelse

!x.type=2			;Reset mapping type
if keyword_set(border) then  $
   plots ,!x.window([0,1,1,0,0]),!y.window([0,0,1,1,0]),/norm,/noclip,t3d=t3d

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Put a grid on a previously established map projection.
pro map_grid3, label=label,  latdel = latdel,$
       londel=londel, glinestyle = glinestyle, glinethick = glinethick,   $
       color = color, lonlab = lonlab, latlab = latlab, lonalign = lonalign,$
       latalign = latalign, charsize = charsize, t3d=t3d

if (!x.type NE 2) THEN $  ; make sure we have mapping coordinates
   message, 'map_grid---Current ploting device must have mapping coordinates'


lonmin = !map.out(2)		;Get lat/lon ranges from !MAP
lonmax = !map.out(3)

latmin = !map.out(4)
latmax = !map.out(5)

if n_elements(t3d) le 0 then t3d = 0
if abs(latmax - latmin) gt 4. then begin	;Make range into integers
  latmin = floor(latmin)
  latmax = ceil(latmax)
endif

if abs(lonmax - lonmin) gt 4 then begin
  lonmin = floor(lonmin)
  lonmax = ceil(lonmax)
endif

			;Default grid spacings...
if n_elements(latdel) eq 0 then latdel = (latmax - latmin)/10. < 15
if n_elements(londel) eq 0 then londel = (lonmax-lonmin)/10. < 15 

if N_Elements(glinestyle) EQ 0 THEN glinestyle =1
if N_Elements(glinethick) EQ 0 THEN glinethick =1

if n_elements(color) le 0 then begin	;Default color?
  if (!d.flags and 512) ne 0 then color = 0 else color = !d.n_colors-1
endif


if N_Elements(label) NE 0 OR (N_ELEMENTS(Latlab) ne 0) $
   OR (N_Elements(LonLab) NE 0) THEN BEGIN
  printno = 1  
  printno2 = 1
  if N_Elements(Latlab) eq 0 THEN Latlab = (lonmin + lonmax)/2
  if N_ELements(LonLab) eq 0 THEN LonLab = (latmin +latmax)/2
endif ELSE BEGIN
  printno = -1
  printno2 = -1
ENDELSE
                                    ; of grid numbers
if n_elements(latalign) eq 0 THEN latalign = .5	;Text alignment of lat labels
if n_elements(lonalign) eq 0 THEN lonalign = .5 ;Text alignment of lon labels
if n_elements(charsize) eq 0 THEN charsize =  1

step = 4 < (latmax - latmin)/10.
len = long((latmax-latmin) / step + 1)
lati = (float(latmax-latmin)/(len-1))*findgen(len)+latmin   ;Array of lats

First = 1

nlon=1+float(lonmax-lonmin)/londel
for i=0,nlon-1 do begin
  lon=lonmin+i*londel
  if (lon lt -180) then lon2 =lon +360  $
    	else if (lon gt 180) then lon2 = -360 +lon $
    	else lon2 = lon
  pres = convert_coord(lon,latmin,/to_norm)
  pres = !map.out(0:1)
  pres1 = convert_coord(lon,latmax,/to_norm)
  pres1 = !map.out(0:1)
  lon1 = lon
  if First eq 1  THEN First = 0 else begin
    if abs(pres(0) - past(0)) GE !map.out(6)   OR  $
       abs(pres(1) - past(1)) GE !map.out(7)   OR  $
       abs(pres1(0) - past1(0)) GE !map.out(6) OR  $
       abs(pres1(1) - past1(1)) GE !map.out(7) THEN BEGIN
      if(lon ge 0) then dd = .0001 else dd = -.0001
      lon1 = lon - dd
    ENDIF  
  endelse
  past = pres
  past1 = pres1
  loni = Replicate(lon1,len)  
  plots,loni,lati,NOCLIP=0,linestyle =glinestyle,thick = glinethick, $
     color = color, t3d=t3d
  if lon2 ne long(lon2) then fmt = '(f7.2)' else fmt = '(i4)'
  if printno eq 1 then xyouts,lon, LonLab, ali=lonalign, t3d=t3d, $
     strtrim(string(lon2,format=fmt),2), charsize = charsize
  printno  = 1 - printno

endfor

!map.out(3) = lonmax 


step = 4 < (lonmax - lonmin)/10.
len = (lonmax-lonmin)/step + 1
loni = findgen(len)*step + lonmin


if (loni(len-1) NE lonmax) THEN  BEGIN
  loni = [loni, lonmax]
  len = len + 1
ENDIF

for lat = float(latmin), latmax, latdel do begin
  if lat ne long(lat) then fmt = '(f7.2)' else fmt = '(i4)'
  if printno2 eq 1 then xyouts,latlab,lat, ali=latalign, t3d=t3d, $
     strtrim(string(lat, format=fmt),2), charsize = charsize
  printno2 = 1 - printno2
  lati = Replicate(lat,len)
  plots,loni,lati,NOCLIP=0,linestyle=glinestyle,color = color, $
     thick=glinethick, t3d=t3d
endfor
!map.out(5) = latmax

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro plot_map3,mapfile,mlinestyle=mlinestyle,mlinethick=mlinethick, $
          t3d=t3d,color=color,save=save

; inputs:  mapfile         map database file
;          mlinestyle      line style used to draw map outline
;          mlinethick      line thickness used to draw map outline
;          t3d             type of 3d transformation
;          color           color used to draw map outline
;          save            1  => save map coordinates to "coastline"
;

mapdir='/home/paul/idl/esrg/maps/'

if not keyword_set(save) then save=0
if not keyword_set(color) then color=!p.color

if (!x.type NE 2) THEN $  ; make sure we have mapping coordinates
   message, 'map_grid---Current ploting device must have mapping coordinates'


latmin=!map.out(4)
latmax=!map.out(5)
lonmin=!map.out(2)
lonmax=!map.out(3)

openr,lu,/get_lun,mapdir+mapfile,/xdr,/stream
nseg=0L & readu,lu,nseg
numpnts=intarr(nseg)
datapntr=lonarr(nseg)
maxlat=fltarr(nseg)
minlat=fltarr(nseg)
maxlon=fltarr(nseg)
minlon=fltarr(nseg)
readu,lu,numpnts,datapntr,minlon,maxlon,minlat,maxlat

segrng=.5*(maxlon-minlon+lonmax-lonmin)
segdst=.5*abs(maxlon+minlon-lonmax-lonmin) mod 360
segdst=segdst < (360.-segdst)
ii=where(((maxlat < latmax) ge (minlat > latmin)) and $
         (segdst le segrng), ncount)
         
if not keyword_set(save) then save=0

if save eq 1 then begin
  openw,lun,/get_lun,'coastline',/xdr,/stream
  writeu,lun,latmin,latmax,lonmin,lonmax
endif

for i=0,ncount-1 do begin
  point_lun, lu, datapntr(ii(i))
  xy=fltarr(2,numpnts(ii(i)))
  READU, lu, xy
  plots,xy(0,*),xy(1,*),NOCLIP=0, t3d=t3d, $
             THICK=mlinethick,linestyle=mlinestyle, color=color
  if save then begin
     writeu,lun,numpnts(ii(i))
     writeu,lun,xy
  endif
endfor

free_lun,lu

if save then free_lun,lun

return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro plot_map2, mapfile,nseg,mlinestyle=mlinestyle, mlinethick=mlinethick, $
          t3d=t3d, color=color, c_color=c_color , save=save
	        
; inputs:  mapfile         map database file
;          mlinestyle      line style used to draw map outline
;          mlinethick      line thickness used to draw map outline
;          t3d             type of 3d transformation
;          color           color used to draw map outline
;          c_color         color for coastlines or political boundaries
;          save            1  => save map coordinates to "coastline"
;
if (!x.type NE 2) THEN message,'Map transform not established.'
latmin=!map.out(4)
latmax=!map.out(5)
lonmin=!map.out(2)
lonmax=!map.out(3)

maxlat=0. & minlat=0. & maxlon=0. & minlon=0.

openr, lun, /get, FILEPATH(mapfile,subdir="maps"),/xdr,/stream

pos=0L

if not keyword_set(save) then save=0

if save eq 1 then begin
  openw,lu,/get_lun,'coastline',/xdr,/stream
  writeu,lu,latmin,latmax,lonmin,lonmax
endif

npts=0
for i=1,nseg do begin
  READU, lun, npts,maxlat,minlat,maxlon,minlon
  point_lun, -lun, pos
  xy=fltarr(2,npts)
  
  inside=1
  if (maxlat lt latmin) or (minlat gt latmax) then inside=0
  segcen=(maxlon+minlon)/2
  limcen=(lonmax+lonmin)/2
  segrng=(maxlon-minlon)/2
  limrng=(lonmax-lonmin)/2
  segdst=abs(segcen-limcen) mod 360
  segdst=segdst < (360.-segdst)
  if segdst gt segrng+limrng then inside=0
  
  if inside then begin
    READU, lun, xy
    plots, xy(0,*), xy(1,*), NOCLIP=0, t3d=t3d, $
       THICK=mlinethick,linestyle=mlinestyle, color=c_color
    if save eq 1 then begin
      if xy(0,0) eq xy(0,npts-1) and xy(1,0) eq xy(1,npts-1) then begin
        writeu,lu,npts
        writeu,lu,xy
      endif else begin
        writeu,lu,npts
        writeu,lu,xy
      endelse
    endif
  endif else begin
    point_lun, lun, pos+npts*8L
  endelse
endfor

if save eq 1 then free_lun,lu
FREE_LUN, lun

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro map_continents3,mlinestyle=mlinestyle,mlinethick=mlinethick,  $
                    color=color, t3d=t3d, save=save, lores=lores

if N_Elements(mlinestyle) EQ 0 THEN mlinestyle=0
if N_Elements(mlinethick) EQ 0 THEN mlinethick=1
if not keyword_set(save) then save=0
if keyword_set(lores) then file='coasts_lores' else file='coasts'

if n_elements(t3d) le 0 then t3d=0
if n_elements(color) le 0 then color=!p.color

plot_map3,file,mlinestyle=mlinestyle,mlinethick=mlinethick,$
              t3d=t3d,color=color,save=save

end	        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro map_rivers3,mlinestyle=mlinestyle,mlinethick=mlinethick,color=color,t3d=t3d
	

if N_Elements(mlinestyle) EQ 0 THEN mlinestyle=0
if N_Elements(mlinethick) EQ 0 THEN mlinethick=1
if n_elements(t3d) le 0 then t3d=0
if n_elements(color) le 0 then color=!p.color

plot_map3,'rivers',mlinestyle=mlinestyle,mlinethick=mlinethick, $
   t3d=t3d,color=color

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro map_usa3,mlinestyle=mlinestyle,mlinethick=mlinethick,color=color,t3d=t3d

if N_Elements(mlinestyle) EQ 0 THEN mlinestyle=0
if N_Elements(mlinethick) EQ 0 THEN mlinethick=1
if n_elements(t3d) le 0 then t3d=0
if n_elements(color) le 0 then color=!p.color

plot_map3,'states',mlinestyle=mlinestyle,mlinethick=mlinethick, $
              t3d=t3d, color=color
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro map_boundaries3,mlinestyle=mlinestyle,mlinethick=mlinethick, $
                    color=color,t3d=t3d
	

if N_Elements(mlinestyle) EQ 0 THEN mlinestyle=0
if N_Elements(mlinethick) EQ 0 THEN mlinethick=1
if n_elements(t3d) le 0 then t3d=0
if n_elements(color) le 0 then color=!p.color

plot_map3,'political',mlinestyle=mlinestyle,mlinethick=mlinethick, $
           t3d=t3d,color=color

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro checkparam3, latmin, latmax, lonmin, lonmax, p0lon,p0lat, limit ,  $
                iproj, Rot

NOLIMIT=0
if (N_Elements(limit) NE 4) and (N_Elements(limit) ne 8) then BEGIN
  lonmin=-180.0 + p0lon
  lonmax=180.0 + p0lon
  if (iproj NE 3) THEN BEGIN
    latmax=90 
    latmin=-90 
  ENDIF
  NOLIMIT=1
ENDIF

; If entire globe, center about P0lon
if (lonmin EQ -180 AND lonmax EQ 180) THEN BEGIN  
  if iproj ne 3 THEN BEGIN
    lonmin=-180 + P0lon
    lonmax=180 + P0lon
  ENDIF ELSE BEGIN
    lonmin=-180 + Rot
    lonmax=180 + rot
  ENDELSE
ENDIF

case 1 of
    iproj eq 1 or iproj eq 2 or iproj eq 4 :BEGIN ;stereo, ortho, lambert
        
        if (NOLIMIT eq 1) and p0lat eq 0.0 then BEGIN
            lonmax=p0lon + 90   ; center on equator
            lonmin=p0lon - 90
        ENDIF
        
        if (NOLIMIT eq 1) and p0lat eq 90.0   then  begin ;center at a pole
            latmin=0.0 
            latmax=90.0
        ENDIF else if (NOLIMIT eq 1) and p0lat eq -90.0 then BEGIN
            latmin=-90.0
            latmax=0.0
        ENDIF
    END                       ;Iproj eq 1 or 2 or 4
    iproj eq 5: BEGIN		;gnomic
        if (NOLIMIT eq 1) and p0lat eq 0.0 then BEGIN ; center on equator
            lonmax=p0lon + 60
            lonmin=p0lon - 60
            latmin=p0lat - 60
            latmax=p0lat + 60
        ENDIF
        if (NOLIMIT eq 1) and p0lat eq 90.0  then  begin
            latmin=30
            latmax=90.0
        ENDIF else if (NOLIMIT eq 1) and p0lat eq -90.0 then BEGIN
            latmin=-90.0
            latmax=-30.0
        ENDIF
    END

 iproj eq 3: BEGIN		;Conic
   if NOLIMIT eq 1 THEN BEGIN
     sgn=(p0lat ge 0) * 2 - 1	; + 1 or -1
     t0=sgn * 15.
     t1=sgn * 75.
     latmin=t0 < t1
     latmax=t0 > t1
     lonmin=-180 + Rot
     lonmax=180  + Rot
   ENDIF                        ;Nolimit
 ENDCASE

 iproj eq 9 or iproj eq 12: if NOLIMIT eq 1 THEN BEGIN ;mercator, merc/simple
   latmin=-80
   latmax=80
 ENDIF
 
 ELSE:
ENDCASE
RETURN
END

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro map_set3, p0lat, p0lon, rot,                   proj=proj,             $
     query=interg,          cylindrical=cyl,       mercator=merc,         $
     mollweide=moll,        stereographic=stereo,  orthographic=ortho,    $
     conic=cone,            lambert=lamb,          gnomic=gnom,           $
     azimuthal=azim,        satellite=satel,       sinusoidal=sinu,       $
     aitoff=aitoff,         latdel=latdel,         londel=londel,         $
     limit=limit,           sat_p=sat_p,           title=title,           $
     noborder=noborder,     noerase=noerase,       label=label,           $
     glinestyle=glinestyle, glinethick=glinethick, mlinestyle=mlinestyle, $
     mlinethick=mlinethick, p_color=p_color,       color=color,           $
     c_color=c_color,       r_color=r_color,       s_color=s_color,       $
     continents=continent,  grid=grid,             xmargin=xmargin,       $
     ymargin=ymargin,       lonlab=lonlab,         latlab=latlab,         $
     lonalign=lonalign,     latalign=latalign,     charsize=charsize,     $
     advance=advance,       usa=usa,               t3d=t3d,               $
     position=position,     rivers=rivers,         political=political,   $  
     save=save,             lores=lores

;+
; NAME:
;      map_set3
; PURPOSE:
;        The procedure map_set3 establishes the axis type and
;        coordinate conversion mechanism for mapping points on the
;        earth's surface, expressed in latitude and longitude, to
;        points on a plane, according to one of ten possible map
;        projections. The user may may select the map projection, the
;        map center, polar rotation and geographical limits.
;
;        The user may request map_set3 to plot the graticule and
;        continental boundaries by setting the keywords Grid and
;        Continent.
;
; CATEGORY:
;           Mapping
; CALLING SEQUENCE:
;   map_set3, p0lat, p0lon, rot,                      PROJ=proj,             $
;       QUERY=interg,          CYLINDRICAL=cyl,       MERCATOR=merc,         $
;       MOLLWEIDE=moll,        STEREOGRAPHIC=stereo,  ORTHOGRAPHIC=ortho,    $
;       CONIC=cone,            LAMBERT=lamb,          GNOMIC=gnom,           $
;       AZIMUTHAL=azim,        SATELLITE=satel,       SINUSOIDAL=sinu,       $
;       AITOFF=aitoff,         LATDEL=latdel,         LONDEL=londel,         $
;       LIMIT=limit,           SAT_P=sat_p,           TITLE=title,           $
;       NOBORDER=noborder,     NOERASE=noerase,       LABEL=label,           $
;       GLINESTYLE=glinestyle, GLINETHICK=glinethick, MLINESTYLE=mlinestyle, $
;       MLINETHICK=mlinethick, P_COLOR=p_color,       COLOR=color,           $
;       C_COLOR=c_color,       R_COLOR=r_color,       S_COLOR=s_color,       $
;       CONTINENTS=continent,  GRID=grid,             XMARGIN=xmargin,       $
;       YMARGIN=ymargin,       LONLAB=lonlab,         LATLAB=latlab,         $
;       LONALIGN=lonalign,     LATALIGN=latalign,     CHARSIZE=charsize,     $
;       ADVANCE=advance,       USA=usa,               T3D=t3d,               $
;       POSITION=position,     RIVERS=rivers,         POLITICAL=political,   $ 
;       SAVE=save,             LORES=lores
;
; OPTIONAL INPUT:
;   p0lat
;     For all but Lambert's conformal conic projection with two
;     standard parallels, P0lat should be set to the latitude of the
;     point on the earth's surface to be mapped to the center of the
;     projection plane.  If the projection type is sinusoidal, P0lat
;     must be 0. -90 <= P0lat <= 90. If P0lat is not set, the default
;     value is 0. If the user has selected Lambert's conformal conic
;     projection with two standard parallels, P0lat should be set to
;     the latitude in degrees of one of the parallels.  If not both
;     P0lat and P0lon are defined, P0lat is 20 by default.
;
;   p0lon
;     For all but Lambert's conformal conic projection with two
;     standard parallels, P0lon should be set by the user to the
;     longitude of the point on the earth's surface to be mapped to
;     the center of the map projection. -180 <= P0lon <= 180. If P0lon
;     is not set, the default value is zero.
;
;   rot
;     The user should set the argument Rot to the angle through which
;     the North direction should be rotated around the line L between
;     the earth's center and the point (P0lat, P0lon). Rot is measured
;     in degrees with the positive direction being clockwise rotation
;     around L. Default value is 0.
;
; KEYWORDS:
;
;   advance         if set, advance to the next frame when there
;                   are multiple plots on the screen.
;
;   aitoff          aitoff's projection
;
;   azimuthal       azimuthal equidistant projection
;
;   conic           conic projection
;
;   cylindrical     cylindrcal equidistant projection
;
;   gnomic          gnomonic projection
;
;   lambert         lambert's equal area projection
;
;   mercator        mercator's projection
;
;   mollweide       mollweide type projection
;
;   orthographic    orthographic projection
;
;   satellite       satellite (general perspective) projection
;
;   sinusoidal      sinsoidal projection
;
;   stereographic   stereographic projection
;
;   charsize        size of characters in labels.
;
;   color           color of the map frame
;
;   c_color         color of continental boundaries.
;
;   r_color         color of rivers.
;
;   p_color         color of political boundaries.
;
;   continents      if set, draw continental boundaries
;
;   glinestyle      linestyle of gridlines. (default = 1 = dotted)
;
;   glinethick      thickness of gridlines. (default = 1 = normal)
;
;   grid            if set, draw gridlines 
;
;   label           if set, label parallels and meridians
;
;   latalign        the aligment of the text baseline for
;                   latitude labels. a value of 0.0 left justifies
;                   the label, 1.0 right justifies it and
;                   0.5 centers. the default value is .5.
;
;   latdel          if set, spacing of parallels drawn on grid. 
;                   default is 10 <  (latmin - latmax) / 10).
;
;   latlab          if set, longitude at which to place latitude
;                   labels. default is longitude of center.
;
;   limit           a four or eight element vector.  if a four element
;                   vector, [latmin, lonmin, latmax, lonmax]
;                   specifying the boundaries of the region to be
;                   mapped. (latmin, lonmin) and (latmax, lonmax) are
;                   the latitudes and longitudes of two diagonal
;                   points on the boundary with latmin < latmax and
;                   lonmin <lonmax.  if an eight element vector: [
;                   lat0, lon0, lat1, lon1, lat2, lon2, lat3, lon3]
;                   specify four points on the map which give,
;                   respectively, the location of a point on the left
;                   edge, top edge, right edge, and bottom edge of the
;                   map extent.
;
;   lonalign        the aligment of the text baseline for longitude
;                   labels. a value of 0.0 left justifies the label,
;                   1.0 right justifies it and 0.5 centers it. default
;                   value is .5.
;
;   londel          if set, spacing of meridians drawn on grid.
;                   y axis, and the screen border
;
;   lonlab          if set, latitude at which to place longitude
;                   labels. default is longitude of center.
;
;   mlinethick      thickness of continental boundaries. default is 1.
;   mlinestyle      linestyle of continental boundaries. default is 0 = normal.
;
;   noborder        if set, no border is drawn around the map.
;
;   noerase         flag to signal that map_set3 should not erase the
;                   current plot window. default is to erase.
;
;   sat_p           an three element array of additional parameters
;                   required by the satellite projection.  the
;                   parameters are: [ p, omega, gamma], where: p =
;                   distance of viewpoint from center of globe, in
;                   units of the globe's radius, gamma = rotation of
;                   projection plane clockwise from north, omega =
;                   downward tilt of the viewing plane in degrees.
;                   the projection plane is first rotated gamma
;                   degrees clockwise from north, and then tilted down
;                   omega degrees.  if both gamma and omega are zero,
;                   a vertical perspective projection results.
;
;   save            save=1 => continental lat-lon points are saved to
;                   file "coastline" for postprocessing by procedure
;                   map_fill.
;
;   lores           if set, use low resolution map data when lores=1
;                   and the high resolution data set when lores=0 if
;                   lores is not set, then the low resolution data set
;                   is used when the map dimensions exceed about 3000 km
;                   otherwise the high resolution data set is used.
;
;                   NOTE: the high resolution database may take a factor
;                   ten times longer to plot than the lo-res version.
;
;   t3d             1 to use the existing 3d transformation, 0 or
;                   omitted for normal.
;
;   title           title of plot to be centered over plot window.
;
;   xmargin         if set, a two element vector that specifies in
;                   character units the vertical margin between the
;                   map and the screen border.
;
;   ymargin         if set, a two element vector that specifies in
;                   character units the horizontal margin between the
;                   map and the screen border.
;
;   position        a one or four element array.  if pos has four elements
;                   they specify the xmin,ymin,xmax,ymax, normalized
;                   coordinates of the map frame.  If pos has just one
;                   element then the map frame will match the last specified
;                   plot window, i.e., pos=[!x.window(0),!y.window(0),
;                   !x.window(1),!y.window(1)]
;                   
;
;   usa             if set draw the state boundries
;
;   rivers          if set draw rivers
;
;   political       if set draw political boundaries
;
;
; SIDE EFFECTS:
;       writes to file "coastline" in current working directory when
;       keyword parameter SAVE is set.
;
;       changes the some of the values in structures !map, !x and !y
;
; EXAMPLE
;
;;
;; display data defined on a regular LAT-LON grid onto a given map 
;; projection.  USE MAP_SET3 and MAP_IMAGE to create the map projection
;; and to warp the image.  Then use BOXPOS to position the TVIM frame
;; to correctly register the map and image
;
; IMAGE = sin(!pi*findrng(0,24,200))#sin(!pi*findrng(0,12,200))
; !p.multi=[0,1,2]
; map_set3,45,0,/ortho,/advance,pos=boxpos(/aspect)
; newimage=map_image(image,/bilin,latmin=-90,latmax=90,lonmin=-180,lonmax=180)
; tvim,newimage,title='Warped data',pos=boxpos(/get),/scale
; map_set3,45,0,/ortho,pos=boxpos(/get),/grid,/cont,/noerase ; draw map
; tvim,image,xrange=[-180,180],yrange=[-90,90],/scale, $
;     title='Unwarped data',xtitle='Longitude',ytitle='Latitude'
; map_set3,0,0,/cyl,pos=boxpos(/get),/grid,/cont,/noerase ; draw map
;
;; In the next example MAP_FILL is used to mask out land areas. Note,
;; however, that MAP_FILL doesn't always fill land areas so neatly.
;; MAP_SET3 is used with /SAVE to create a file containing the continental 
;; boundary lat-lon coordinates in the current working directory.  Then 
;; MAP_FILL is called to color-fill the land areas.
;
; image=randata(32,32,s=2.5)
; tvim,image,/interp
; map_set3,-64,-64,limit=[-65.1,-64.5,-64,-62],/ortho,$
;           pos=boxpos(/get),/noerase,/grid,/save,/cont
; map_fill,c_color=200,f_color=100
;;
;; map of channel islands with continental regions filled in
;
; map_set3,35,-120,/cont,limit=[33,-120,35,-118],/ortho,/save,c_color=100
; map_fill,f_color=100
;;
;; NOTE: In this example the continental boundaries don't form nice
;; closed regions which POLYFILL can fill.  Use MAP_FILL in interactive
;; mode (no arguments) to fix up the map segments to allow proper
;; POLYFILLing.
;
;
; Modification history:
;			Written, Ann Bateson, 1991
;	SMR, 10-'91	Added a check for additional satelite parameters
;			   when /SATELITE is set.
;       UCSB, ESRG,5-92 Added USA flag to check and map state boundries
;	DMS,  6-92	Fixed bug that incorrectly printed grid values with
;				small lat/lon ranges.
;	DMS, 8-92	Added 8 element lat/lon keyword, fixed problems
;				with Satellite, conics with 1 std parallel,
;				smaller and/or wierd maps
;       PJR, 5-95       put in SAVE keyword and cleaned some code
;       PJR, 9-95       installed new data format for map databases, new
;                       format allows much more rapid execution for small
;                       area maps.
;       PJR, 2-96       installed lores flag 
;-
; !Map.out coorespondence:
;     0 p, 1 q, 2 umin, 3 umax, 4 vmin, 5 vmax, 6 ueps, 7 veps,
;      8 ucen, 9 vcen, 10 urng, 11 vrng;



if (N_ELements(interg) eq 0) THEN BEGIN                ; DONT INTERROGATE 

  if n_elements(proj) ne 0    then iproj =proj          $        
  else if keyword_set(stereo) then iproj =1		$
  else if keyword_set(ortho ) then iproj =2		$
  else if keyword_set(cone  ) then iproj =3		$
  else if keyword_set(lamb  ) then iproj =4		$
  else if keyword_set(gnom  ) then iproj =5		$
  else if keyword_set(azim  ) then iproj =6		$
  else if keyword_set(satel ) then iproj =7		$
  else if keyword_set(merc  ) then iproj =9		$
  else if keyword_set(moll  ) then iproj =10		$
  else if keyword_set(sinu  ) then iproj =14            $
  else if keyword_set(aitoff) then iproj =15            $
  else iproj=8                  ;Assume cylindrical

  IF (iproj eq 7) and (NOT(KEYWORD_SET(sat_p))) THEN $
     MESSAGE, "Satellite parameters must be set for satellite projection."

  if keyword_set(noborder) eq 0 then border =1
  if n_elements(limit) eq 4 then BEGIN
    latmin=limit(0)
    latmax=limit(2)
    lonmin=limit(1)
    lonmax=limit(3)                        
  ENDIF ELSE if n_elements(limit) eq 8 then BEGIN
    latmin=min(limit([0,2,4,6]))
    latmax=max(limit([0,2,4,6]))
    lonmin=min(limit([1,3,5,7]))
    lonmax=max(limit([1,3,5,7]))
  ENDIF

  if !P.multi(0) eq 0 and keyword_set(advance) then erase

  if n_elements(rot) eq 0 then rot=0. ;Default rotation
  if n_elements(t3d) le 0 then t3d=0

  if n_elements(p0lat) eq 0 then  $ ;Defaults values for center of proj
     if iproj NE 3 THEN p0lat=0.0 $
  else  BEGIN 
    p0lat=20
    p0lon=60
  ENDELSE

  if n_elements(p0lon) eq 0 then   $
     if iproj NE 3 THEN p0lon=0.0 $
  ELSE BEGIN
    p0lon=60
    p0lat=20
  ENDELSE
ENDIF

if (iproj eq 14) THEN BEGIN     ;Sinusoidal
  if N_Elements(P0lat) NE 0 THEN $
     if P0lat NE 0 THEN  $
     message,'map_set3--- Sinusoidal projection must have P0lat=0'
  if N_Elements(Rot) NE 0 THEN  $
     if Rot NE 0 THEN   $
     message,'map_set3--- Sinusoidal projection must have Rot=0'
  p0lat=0
  rot  =0
endif

if N_Elements(interg) ne 0 THEN BEGIN
  iproj=0
  read,'Projection number ',iproj
  read,'p0lat, p0lon, rot:  ',p0lat,p0lon,rot
  read,'latmin, latmax: ', latmin, latmax
  read,'lonmin, lonmax: ', lonmin, lonmax
ENDIF

if KEYWORD_SET(xmargin) THEN BEGIN
  csize=!d.X_SIZE/!d.X_CH_SIZE	;1/Char width in normalized units
  if total(xmargin) gt csize THEN BEGIN
    message,'map_set3--- xmargin too large'
    return
  ENDIF
ENDIF

if KEYWORD_SET(ymargin) THEN BEGIN
  csize=!d.Y_SIZE/!d.Y_CH_SIZE
  if total(ymargin) gt csize THEN BEGIN
    message,'map_set3--- ymargin too large'
    return
  ENDIF
ENDIF



!map.projection=iproj		;Set mapping parameters
!x.type=2
help,!map,/struct
!map.p0lon=p0lon		;Save Center longitude /lat for internals
!map.p0lat=p0lat

if not N_Elements(noerase) and not KEYWORD_SET(advance) THEN erase

checkparam3, latmin, latmax, lonmin, lonmax, p0lon, p0lat, limit, iproj, Rot
; print, latmin, latmax, lonmin, lonmax

!map.out(2)=lonmin		;Limits
!map.out(3)=lonmax
!map.out(4)=latmin
!map.out(5)=latmax

if n_elements(lores) eq 0 then begin
  if (latmax-latmin) >  cos(!dtor*.5*(latmax*latmin))*(lonmax-lonmin) gt 30 $
    then lores=1 else lores=0
endif

mapp_set3, latmin, latmax, lonmin, lonmax, p0lon, p0lat,rot,iproj, limit,  $
      border=border, title =title, sat_p=sat_p, xmargin=xmargin, $
      ymargin=ymargin, t3d=t3d, position=position

if keyword_set(grid) then $		;Grid?
   map_grid3, label=label, latdel=latdel,$
   londel=londel , glinestyle=glinestyle, $
   glinethick=glinethick, color=color, latlab=latlab,$
   lonlab=lonlab, lonalign=lonalign, latalign=latalign, $
   charsize=charsize, t3d=t3d

if keyword_set(continent) then $ ;Continents?
   map_continents3,color=c_color,t3d=t3d, $
   mlinestyle=mlinestyle,mlinethick=mlinethick, save=save, lores=lores

if keyword_set(usa) ne 0 then $ ;US State outlines?
   map_usa3, color=s_color, t3d=t3d, $
   mlinestyle=mlinestyle, mlinethick=mlinethick

if keyword_set(rivers) ne 0 then $ ;Rivers?
   map_rivers3, color=r_color, t3d=t3d, $
   mlinestyle=mlinestyle, mlinethick=mlinethick

if keyword_set(political) ne 0 then $ ;Boundaries?
   map_boundaries3, color=p_color, t3d=t3d, $
   mlinestyle=mlinestyle, mlinethick=mlinethick

if KEYWORD_SET(ADVANCE) and !P.Multi(0) gt 0 THEN $
   !P.Multi(0)=!P.Multi(0) - 1 $
else !p.multi(0)=!p.multi(1) * !p.multi(2) - 1
       
RETURN
END
