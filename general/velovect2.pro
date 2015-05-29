PRO VELOVECT2,U,V,X,Y, Missing = Missing, Length = length, Dots = dots,$
  noaxis=noaxis,color=color,levels,charsize=charsize,unit=unit,$
  max_arrow=max_arrow,draw_one=draw_one,mag_min=mag_min,$
  x_min=x_min,y_min=y_min,x_reverse=x_reverse,y_reverse=y_reverse,$
  arrow_head=arrow_head,onescale=onescale,twoscales=twoscales,$
  dataangle=dataangle,deviceangle=deviceangle,noscale=noscale,xmap=xmap,ymap=ymap
;
;+
; NAME:
;	VELOVECT2
; PURPOSE:
;	Produce a two dimensional velocity field plot.
;		Draw a directed arrow at each point showing
;		the direction and magnitude of the field.
;               The arrows will have a scaling which depends on the
;		magnitude of the max x and y components.
;		
; CATEGORY:
;	Graphics
; CALLING SEQUENCE:
;	VELOVECT2,U,V[,X,Y]
; INPUTS:
;	U -- The X component of the two dimensional field.  Both
;         U and V must be two dimensional and have the same dimensions.
;	V -- The Y component of the two dimensional field.  The
;         vector at point (i,j) has the magnitude of
;         (U(i,j)^2 + V(i,j)^2)^0.5 and a direction of
;         ATAN2(V(i,j),U(i,j)).
; OPTIONAL INPUT PARAMETERS:
;	X -- Optional abcissae values, Size of X must = 1st dimension
;         of U and V.  Must be a vector.
;       Y -- Optional ordinate values.  Same rules as X.
; KEYWORD INPUT PARAMETERS:
;	Missing -- missing data value; values with this value are ignored.
;	Length -- length factor relative to the default.
;	/dots -- Place a dot at each missing point; 0 or omitted, draw nothing
;         for missing points.  Has effect only if Missing is specified.
;	/Noaxis -- don't draw the axes
;       /onescale,/draw_one (synonymous) -- Use the same scaling for x- and
;         y-components of the vector. Ignored if max_arrow is specified.
;       /twoscales -- Use separate scalings for the x- and y-components.
;         Ignored if max_arrow is specified.
;       /noscale -- Do not draw a key arrow
;       /dataangle -- Draw the arrows such that equal displacements in the x-
;         and y-components are represented by equal distances in map coordinates
;         on their respective axes. This keyword is ignored if separate scaling
;         for the components is being used. "map" coordinates are identical to
;         "data" coordinates, unless xmap or ymap is supplied.
;       xmap -- An array of x-scaling values for /dataangle; it must have the
;         same dimensions as U. An element of xmap indicates that one unit of U
;         (one unit of map x-coordinates) should be represented by xmap units of
;         data coordinates of the x-axis.
;       ymap -- An array of y-scaling values for /dataangle; it must have the
;         same dimensions as V. An element of xmap indicates that one unit of V
;         (one unit of map y-coordinates) should be represented by ymap units of
;         data coordinates of the y-axis.
;       /deviceangle -- Draw the arrows such that equal displacements in the x-
;         and y-components are represented by equal distances in device
;         coordinates i.e. physical lengths on the plot (we assume). This keyword
;         is ignored if separate scaling for the components is being used. If a
;         common scaling is being used, it is the default. (It is currently an
;         unnecessary keyword.)
;       charsize -- character size for the key
;       color -- colour index or indices for arrows
;         If a scalar, specifies the colour for all arrows
;         If a 2d array, it must have the same dimensions as u and v and
;           it specifies the colour point by point
;	max_arrow -- Specifies the value for the maximum arrow which can
;         comfortably be drawn, and for the arrow key. By default, this is
;         derived from largest size the vector takes. max_arrow can be
;         given as a 2-element vector to specify different scalings for
;         the x- and y-components. If max_arrow has one element, it
;         implies /onescale; if two elements, /twoscales.
;       unit -- units string for the arrow key. If it has two elements,
;         two arrow keys are drawn, even if the scaling is the same for
;         both components.
;       /x_reverse -- reverse sense of arrows in x
;       /y_reverse -- reverse sense of arrows in y
;       mag_min  -- minimum value of vector magnitude for arrow to be drawn.
;         Default is 0.
;       x_min  -- minimum value of abs(U) for arrow to be drawn; default 0.
;       y_min  -- minimum value of abs(V) for arrow to be drawn; default 0.
;       arrow_head -- size of arrow head, relative to the default. Value of zero
;         to give just lines, larger values for larger arrow heads.
; OUTPUTS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	Plotting on the selected device is performed.  System
;	variables concerning Plotting are changed.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  The system variables !Xtitle, !Ytitle and
;	!Mtitle may be set to title the axes.
;       Different scalings are used for x- and y-components if specifically
;       requested (with a two-element max_arrow or /twoscales) or by default if
;       the maximum sizes of the components differ by more than an order of
;       magnitude. In this latter case, a common scale is used if specifically
;       requested (with a one-element max_arrow or /onescale).
; MODIFICATION HISTORY:
;	Dms, Rsi, Oct, 1983.
;	For Sun, DMS, RSI, April, 1989.
;       UKMO local modified version of velovect, named velovect2
;-
; Year 2000 compliant: Yes
; Checked by Roger Milton on 23rd August 1998

author_name='$Author: pvwave $'
date_name='$Date: 1998/08/23 12:14:25 $'
version_name='$Revision: 7.2 $'
@comm_error
;============================================================
; set up default values
;============================================================

s = size(u)
t = size(v)
if s(0) ne 2 then begin 
baduv:	 message, 'U and V parameters must be 2D and same size.'
endif
if total(abs(s(0:2)-t(0:2))) ne 0 then goto,baduv
;
if n_params(0) lt 3 then x = findgen(s(1)) else $
  if n_elements(x) ne s(1) then begin
badxy:	message, 'X and Y arrays have incorrect size.'
  endif
if n_params(1) lt 4 then y = findgen(s(2)) else $
  if n_elements(y) ne s(2) then goto,badxy
;

if n_elements(missing) le 0 then missing = !pp.rmdi
if n_elements(length) le 0 then length = 1.0
if n_elements(color) eq 0 then color=!p.color
if (n_elements(mag_min) eq 0) then mag_min=0.0
if (n_elements(x_min) eq 0) then x_min=0.0
if (n_elements(y_min) eq 0) then y_min=0.0
if not keyword_set(arrow_head) then arrow_head=1
if keyword_set(draw_one)+keyword_set(onescale)+keyword_set(twoscales)+$
  keyword_set(noscale) gt 1 $
  then message,'/DRAW_ONE, /ONESCALE, /TWOSCALES, /NOSCALE are mutually exclusive'
if keyword_set(dataangle) and keyword_set(deviceangle) $
  then message,'/DATAANGLE and /DEVICEANGLE are mutually exclusive'
if keyword_set(draw_one) then onescale=1

; make up colors from input
sz=size(color)
case  sz(0) of
  0:begin
    colors=bytarr(s(1),s(2))
    colors(*,*)=color
    xyoutscolor =color
  end

  2:begin
    colors=color
    xyoutscolor = !p.color      ;ensure XYOUTS is supplied with a scalar colour
  end

  else: message,'Color arguments must have 2 dimensions or be a scalar value'
endcase

;============================================================
;find subscripts of non-missing elements
;============================================================
mag = sqrt(u^2+v^2)             ; magnitude
nbad = 0			;# of missing points
if n_elements(missing) gt 0 then begin
  good = where(u ne missing and abs(u) ge x_min and $
    v  ne missing and abs(v) ge y_min and abs(mag) ge mag_min,good_count) 
  if keyword_set(dots) then bad = where(u eq missing or v eq missing or $
   abs(u) le x_min or abs(v) lt y_min or abs(mag) lt mag_min, nbad)
endif else begin
  good_count=N_ELEMENTS(u)
  good = LINDGEN(good_count)
ENDELSE

IF (good_count eq 0) THEN BEGIN
  message, /INFO, "No arrows to plot"
  RETURN
ENDIF

IF (good_count NE 0) THEN  BEGIN ;; got some data to plot

  ugood = u(good)
  vgood = v(good)               ; ugood and vgood are non missing data 

; arrow locations
  x0 = x(good MOD s(1))
  y0 = y(good / s(1))

  x0=con_coords(x0,/to_raw)
  y0=con_coords(y0,/to_raw,/y)

; do we need different scaling for each axis ?
; we do if max(abs(u)) is more than a magnitude different from
;        max(abs(v)) or max_arrow has two cpts

  uscale = MAX(ABS(ugood))
  vscale = MAX(ABS(vgood))
; ratio of component mags
  if uscale eq 0 or vscale eq 0 then cpt_ratio=1 else cpt_ratio = (uscale/vscale)
  mag = SQRT(ugood^2+vgood^2)	;magnitude.
  biggest=MAX(mag)

  case 1 of
    n_elements(max_arrow) ne 0: maxmag=max_arrow
    ((cpt_ratio LE 0.1 OR cpt_ratio GE 10.0) AND NOT KEYWORD_SET(onescale)) $
      or keyword_set(twoscales): maxmag = [uscale, vscale]
    else: maxmag=biggest
  endcase
  diff_scale=n_elements(maxmag) eq 2
  if not diff_scale then maxmag=replicate(maxmag,2)

; maxmag is now the scaling term due to the velocity in each dirn.
; what do we do if we have a max magnitude in one dirn of 0.0 ?
; set it to 1 me thinks.
  ex = WHERE(maxmag EQ 0.0, nzero)
  IF (nzero GT 0) THEN maxmag(ex) = 1.0

;============================================================
; work out scaling terms
;============================================================
  xcrange = !X.crange(1)-!X.crange(0)
  IF (N_ELEMENTS(x) LE 1) $
    THEN delta_x = xcrange*0.5 $
    ELSE delta_x = sign(xcrange)*(MAX(x)-MIN(x))/(N_ELEMENTS(x)-1)
  ycrange = !Y.crange(1)-!Y.crange(0)
  IF (N_ELEMENTS(y) LE 1) $
    THEN delta_y = ycrange*0.5 $
    ELSE delta_y = sign(ycrange)*(MAX(y)-MIN(y))/(N_ELEMENTS(y)-1)
  xmaxlength = delta_x
  ymaxlength = delta_y ; xscale and yscale are the box widths

; [xy]unit -> the lengths in device coordinates of a single unit along the
; two axes in data coordinates
; (Use the whole axis range for the conversion so as to prevent truncation of the
; scaling. A small data range might in extreme cases might produce a zero scaling
; factor, since device coordinates are integer.)
  xunit=con_coords(abs(xcrange),/to_raw)/abs(xcrange) ;/distance
  yunit=con_coords(abs(ycrange),/to_raw,/y)/abs(ycrange) ;/distance
; Apply externally specified scalings
  if not diff_scale and keyword_set(dataangle) then begin
    if n_elements(xmap) ne 0 then xunit=xunit*xmap(good)
    if n_elements(ymap) ne 0 then yunit=yunit*ymap(good)
  endif
; [xy]maxlength -> the spacings between x and y points in device coords.
; (These will be arrays if [xy]map supplied.)
  xmaxlength=delta_x*xunit
  ymaxlength=delta_y*yunit

; maxlength -> the spacing of either x or y points, whichever are closer
; Arrow head size is scaled by this spacing.
  maxlength=min(abs([xmaxlength,ymaxlength]))
  scale_head=arrow_head*maxlength

; [xy]maxlength each become equal in magnitude to whichever is smaller.
  if not diff_scale then begin
    xmaxlength=sign(xcrange)*maxlength
    ymaxlength=sign(ycrange)*maxlength
  endif

; [xy]scale -> scaling factors for x- and y-components of the vector.
; Note that magmax(0) eq maxmag(1) and xmaxlength eq ymaxlength if not diff_scale.
; For /deviceangle, this is the final scaling: x and y have the same scale, and
; [1,1] will point at 45 degrees, assuming that equal displacements in device
; coordinates correspond to equal distances on the plot. (I don't think this is
; guaranteed - JMG.)
  xscale = length*xmaxlength/maxmag(0)*(1-2*KEYWORD_SET(x_reverse))
  yscale = length*ymaxlength/maxmag(1)*(1-2*KEYWORD_SET(y_reverse))

; Arrow lengths
  if not diff_scale and keyword_set(dataangle) then begin
; Transform angles so that vectors point along *apparent* bearing, meaning that a
; vector [1,1], for instance, will point along a line of 1 data coordinate in
; each axis, rather than at 45 degrees.
; Calculate the angle at which the arrow should point
    angle=atan(yunit*vgood,xunit*ugood)
    badangle=where(finite(angle) eq 0,count) ; because magnitude of zero
    if count ne 0 then angle(badangle)=0
; Length of the arrow should be mag*[xy]scale (NB xscale eq yscale, since not
; diff_scale). Work out its extent in x and y.
    dx=mag*xscale*cos(angle)
    dy=mag*yscale*sin(angle)
  endif else begin
    dx = xscale*ugood
    dy = yscale*vgood
  endelse

; plt_arrow must be called in device coordinates because otherwise the arrowhead
; is distorted. It assumes that it is using a coordinate system where equal
; displacements give equal distances.
  plt_arrow, x0, y0, dx, dy, scale_head, /device, color = colors(good)

ENDIF ;; end of test whether there are arrows to plot

if nbad gt 0 then $		;Dots for missing?
  oplot, x(bad mod s(1)), y(bad / s(1)), psym = 3,color=colors(nbad(i))

;============================================================
; plot an arrow or arrows in the bottom left hand corner to give scale
;============================================================

if keyword_set(noscale) then return

;==============================
; work out charsize and base posn
;==============================
if n_elements(charsize) eq 0 then charsize=!p.charsize
size=charsize
if size eq 0 then size=1.
if (small_chars()) then size=size/2.0 ; size is the character size

;============================================================
; plot first arrow
;============================================================

comp_sub_posn,x0,y0,xsize=xsize,ysize=ysize,charsize=charsize,/arrow
if (n_elements(max_arrow) lt 1) then value=nicemultiple(maxmag(0)/length,/down) $
  else value=max_arrow(0)

dx=xscale*value
csdev=char_dimens(charsize=charsize,/device)
x0=con_coords(x0,/data,/to_raw,/x)
y0=con_coords(y0,/data,/to_raw,/y)
plt_arrow,x0,y0+csdev(1)/3,dx,0,scale_head,/device,color=xyoutscolor

if n_elements(unit) eq 0 then pl_unit='' else pl_unit=' '+unit(0)
xyouts,x0+dx+1.5*csdev(0),y0,nicenumber(value,/format)+pl_unit,/device,size=size,$
  Color=xyoutscolor,width=wth

; Draw the vertical key arrow
if (diff_scale or n_elements(unit) eq 2) then begin

  case n_elements(max_arrow) of
    0: value=nicemultiple(maxmag(1)/length,/down)
    2: value=maxmag(1)
    else: ; use the same value as for the first arrow
  endcase
  x0=x0+dx+abs(con_coords(wth,/to_raw,/norm))+4*csdev(0) ;/distance
  dy=yscale*value
  plt_arrow,x0,y0-0.5*dy+csdev(1)/3,0.,dy,scale_head,/device,color=xyoutscolor

  if n_elements(unit) eq 0 then pl_unit='' else $
    pl_unit=' '+unit(1 mod n_elements(unit))
  xyouts,x0+1.5*csdev(0),y0,nicenumber(value,/format)+pl_unit,/device,size=size,$
    Color=xyoutscolor

endif

end
