PRO plothist, arr, xhist,yhist, BIN=bin,  NOPLOT=NoPlot, OVERPLOT=Overplot, $
           PSYM = psym, Peak=Peak, Fill=Fill, FCOLOR=Fcolor, FLINE=FLINE, $
           FSPACING=Fspacing, FPATTERN=Fpattern, FORIENTATION=Forientation, $
           NAN = NAN, _EXTRA = _extra, Halfbin = halfbin, AUTOBin = autobin, $
	   Boxplot = boxplot, xlog = xlog, ylog = ylog, yrange = yrange, $
	   Color = color
;+
; NAME:
;      PLOTHIST
; PURPOSE:
;      Plot the histogram of an array with the corresponding abcissa.
;
; CALLING SEQUENCE:
;      plothist, arr, xhist, yhist, [, BIN=, /FILL, /NOPLOT, /OVERPLOT, PEAK=,
;                                     /AUTOBIN,  ...plotting keywords]
; INPUTS:
;      arr - The array to plot the histogram of.   It can include negative
;            values, but non-integral values will be truncated.              
;
; OPTIONAL OUTPUTS:
;      xhist - X vector used in making the plot  
;              ( = lindgen( N_elements(h)) * bin + min(arr) )
;      yhist - Y vector used in making the plot  (= histogram(arr/bin))
;
; OPTIONAL INPUT KEYWORDS:
;      /AUTOBIN - Automatically determines bin size of the histogram as the
;                 square root of the number of samples. Only valid when BIN
;                 is not set.
;      BIN -  The size of each bin of the histogram,  scalar (not necessarily
;             integral).  If not present (or zero), the bin size is set to 1.
;      /BOXPLOT - If set, then each histogram data value will be plotted
;             "box style" with vertical lines drawn from Y=0 at each end of 
;              the bin width
;      /HALFBIN - Set this keyword to a nonzero value to shift the binning by
;              half a bin size.     This is useful for integer data, where e.g.
;              the bin for values of 6 will go from 5.5 to 6.5.   The default
;              is to set the HALFBIN keyword for integer data, and not for
;              non-integer data.    Note: prior to May 2002, the default was 
;              to always shift the binning by half a bin.                
;      /NAN - If set, then check for the occurence of IEEE not-a-number values
;      /NOPLOT - If set, will not plot the result.  Useful if intention is to
;             only get the xhist and yhist outputs.
;      /OVERPLOT - If set, will overplot the data on the current plot.  User
;            must take care that only keywords valid for OPLOT are used.
;      PEAK - if non-zero, then the entire histogram is normalized to have
;             a maximum value equal to the value in PEAK.  If PEAK is
;             negative, the histogram is inverted.
;      /FILL - if set, will plot a filled (rather than line) histogram.
;
; The following keywords take effect only if the FILL keyword is set:
;      FCOLOR - color to use for filling the histogram
;      /FLINE - if set, will use lines rather than solid color for fill (see
;              the LINE_FILL keyword in the POLYFILL routine)
;      FORIENTATION - angle of lines for fill (see the ORIENTATION keyword
;              in the POLYFILL routine)
;      FPATTERN - the pattern to use for the fill (see the PATTERN keyword
;              in the POLYFILL routine)
;      FSPACING - the spacing of the lines to use in the fill (see the SPACING
;              keyword in the POLYFILL routine)
;
; Any input keyword that can be supplied to the PLOT procedure (e.g. XRANGE,
;    LINESTYLE, /XLOG, /YLOG) can also be supplied to PLOTHIST.
;
; EXAMPLE:
;       (1) Create a vector of random 1000 values derived from a Gaussian of 
;       mean 0, and sigma of 1.    Plot the histogram of these values with a 
;       binsize of 0.1, and use a box plotting style.
;
;       IDL> a = randomn(seed,1000)
;       IDL> plothist,a, bin = 0.1, /boxplot
;
;       (2) As before, but fill the plot with diagonal lines at a 45 degree 
;           angle
;
;       IDL> plothist,a, bin=0.1, /fill, /fline, forient=45
;
; MODIFICATION HISTORY:
;        Written     W. Landsman            January, 1991
;        Add inherited keywords W. Landsman        March, 1994
;        Use ROUND instead of NINT  W. Landsman   August, 1995
;        Add NoPlot and Overplot keywords.   J.Wm.Parker  July, 1997
;        Add Peak keyword.   J.Wm.Parker  Jan, 1998
;        Add FILL,FCOLOR,FLINE,FPATTERN,FSPACING keywords. J.Wm.Parker Jan, 1998
;	 Converted to IDL V5.0   W. Landsman 21-Jan-1998
;        Add /NAN keyword        W. Landsman October 2001
;        Don't plot out of range with /FILL, added HALFBIN keyword, make
;        half bin shift default for integer only W. Landsman/J. Kurk May 2002
;        Add BOXPLOT keyword, use exact XRANGE as default W.L.  May 2006
;        Allow use of /XLOG and /YLOG keywords  W.L. June 2006
;-
;			Check parameters.
 On_error,2
 compile_opt idl2

 if N_params() LT 1 then begin   
	print,'Syntax - plothist, arr, [xhist,yhist, ' 
        print, '         [/AUTOBIN, BIN=, /BOXPLOT, HALFBIN=, PEAK=, /NOPLOT,'
	print, '         /OVERPLOT, /FILL...plotting keywords]' 
        print,'Fill keywords: FCOLOR=, /FLINE, FORIENTATION=, FPATTERN=,' + $
              'FSPACING= '
	return
 endif

 if N_elements( arr ) LT 2 then message, $
      'ERROR - Input array must contain at least 2 elements'
 arrmin = min( arr, MAX = arrmax)
 if ( arrmin EQ arrmax ) then message, $
       'ERROR - Input array must contain distinct values'

 
  if not keyword_set(BIN) then $
   if keyword_set(AUTOBIN) then $
     bin = (max(arr)-min(arr))/sqrt(N_elements(arr)) $
   else $
     bin = 1.0 $
 else bin = float(abs(bin))

; Compute the histogram and abcissa.    Determine if a half bin shift is 
; desired (default for integer data)    
 
 if N_elements(halfbin) EQ 0 then begin 
    dtype = size(arr,/type)
    halfbin = (dtype NE 4) and (dtype NE 5)       ;Non-integer data?
 endif 
 halfbin = keyword_set(halfbin)
 if keyword_set(NAN) then begin
      good = where(finite(arr) )
      if halfbin then y = round( ( arr[good] / bin)) $
                 else y = floor( ( arr[good] / bin))
 endif else if halfbin then y = round( ( arr / bin)) $
                       else y = floor( ( arr/ bin)) 
 
 yhist = histogram( y )
 N_hist = N_elements( yhist )
 xhist = lindgen( N_hist ) * bin + min(y*bin) 
 if not halfbin then xhist = xhist + 0.5*bin
;;;
;   If renormalizing the peak, do so.
;
if keyword_set(Peak) then yhist = yhist * (Peak / float(max(yhist)))

;;;
;   If not doing a plot, exit here.
;
 if keyword_set(NoPlot) then return

 if keyword_set(over) then begin    ;if overplotting, was original plot a log?
      if N_elements(ylog) EQ 0 then ylog = !Y.type
      if N_elements(xlog) EQ 0 then xlog = !X.type
 endif     
 if not keyword_set(PSYM) then psym = 10         ;Default histogram plotting
 if not keyword_set(COLOR) then color = !p.color
  if not keyword_set(XRANGE) then xrange = [ xhist[0]-bin ,xhist[N_hist-1]+bin ]
 if not keyword_set(xstyle) then xstyle=1
 
 if  keyword_set(ylog) then begin 
     ymin =0.1
     if N_elements(yrange) EQ 2 then ymin = ymin < yrange[0] 
     ydata = [ymin, yhist>ymin, ymin] 
 endif else ydata = [0, yhist, 0]
 xdata = [xhist[0] - bin, xhist, xhist[n_hist-1]+ bin]
 if keyword_set(xlog) then xrange[0] = xrange[0]>1
 
 if keyword_set(Overplot) then begin
     oplot, xdata , ydata,  $ 
        PSYM = psym, _EXTRA = _extra ,color=color
 endif else begin
 
    plot, xdata , ydata,  $ 
        PSYM = psym, _EXTRA = _extra,xrange=xrange,color=color,xstyle=xstyle, $
	xlog = xlog, ylog = ylog, yrange=yrange
 endelse

   ; Make histogram boxes by drawing lines in data color.

if keyword_set(boxplot) then begin
    ycrange = keyword_set(ylog)? 10^!Y.CRANGE : !Y.CRANGE
FOR j =0 ,N_Elements(xhist)-1 DO BEGIN
   PlotS, [xhist[j], xhist[j]]-bin/2, [YCRange[0], yhist[j] < Ycrange[1]], $
          Color=Color
ENDFOR 

endif

;;;
;   If doing a fill of the histogram, then go for it.
;
 if keyword_set(Fill) then begin
    Xfill = transpose([[Xhist-bin/2.0],[Xhist+bin/2.0]])
    Xfill = reform(Xfill, n_elements(Xfill))
    Xfill = [Xfill[0], Xfill, Xfill[n_elements(Xfill)-1]]
    Yfill = transpose([[Yhist],[Yhist]])
    Yfill = reform(Yfill, n_elements(Yfill))
    
    xcrange = keyword_set(xlog)? 10^!X.CRANGE : !X.CRANGE
    ycrange = keyword_set(ylog)? 10^!Y.CRANGE : !Y.CRANGE
    if keyword_set(ylog) then Yfill = [ycrange[0]/10, Yfill, ycrange[0]/10] $
                          else yfill = [0, yfill, 0 ]
    Xfill = Xfill > XCRANGE[0] < XCRANGE[1]    ;Make sure within plot range
    Yfill = Yfill > YCRANGE[0] < YCRANGE[1]

    if keyword_set(Fcolor) then Fc = Fcolor else Fc = !P.Color
    if keyword_set(Fline) then begin
       if keyword_set(Fspacing) then Fs = Fspacing else Fs = 0
       if keyword_set(Forientation) then Fo = Forientation else Fo = 0
       polyfill, Xfill,Yfill, color=Fc, /line_fill, spacing=Fs, orient=Fo
    endif else begin
       if keyword_set(Fpattern) then begin
          polyfill, Xfill,Yfill, color=Fc, pattern=Fpattern
       endif else begin
          polyfill, Xfill,Yfill, color=Fc
       endelse
    endelse

;;;
;   Because the POLYFILL can erase/overwrite parts of the originally plotted
; histogram, we need to replot it here.
;
    oplot, [xhist[0] - bin, xhist, xhist[n_hist-1]+ bin] , [0,yhist,0],  $ 
       PSYM = psym, _EXTRA = _extra
 endif


 return
 end