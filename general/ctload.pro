;+
; NAME:
;       CTLOAD
;
; PURPOSE:
;
;       This is a drop-in replacement for the ITTVIS-supplied program LOADCT.
;       The same keywords used with LOADCT apply. In addition, a REVERSE keyword
;       is supplied to reverse the color table vectors, and a CLIP keyword is
;       supplied to be able to clip the normal LOADCT color table. This is
;       extremely useful if you wish to use a reduced number of colors. Also,
;       all color table loading is handled silently. (To fix a major pet-peeve
;       of mine.)
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;
;       CTLOAD, table
;
; AUGUMENTS:
;
;       table:         Optional table number to load. Integer from 0 to the number of
;                      tables in the file, minus 1. Default value is 0.
;
; KEYWORDS:
;
;       BOTTOM:        The first color table index. Set to 0 by default.
;
;       CLIP:          A one- or two-element integer array that indicates how to clip
;                      the original color table vectors. This is useful if you are
;                      restricting the number of colors, and do not which to have
;                      black or white (the usual color table end members) in the
;                      loaded color table. CLIP[0] is the lower bound. (A scalar
;                      value of CLIP is treated as CLIP[0].) CLIP[1] is the upper
;                      bound. For example, to load a blue-temperature color bar
;                      with only blue colors, you might type this:
;
;                        IDL> CTLOAD, 1, CLIP=[110,240]
;                        IDL> CINDEX
;
;                     Or, alternatively, if you wanted to include white at the upper
;                     end of the color table:
;
;                        IDL> CTLOAD, 1, CLIP=110
;                        IDL> CINDEX
;
;       RGB_TABLE:    If this keyword is set to a named variable, the color table
;                     is returned as an [NCOLORS,3] array and no colors are loaded
;                     in the display.
;
;       FILE:         The name of a color table file to open. By default colors1.tbl in
;                     the IDL directory.
;
;       GET_NAMES:    If set to a named variable, the names of the color tables are returned
;                     and no colors are loaded in the display. Note that RGB_TABLE cannot be
;                     used concurrently with GET_NAMES. Use two separate calls if you want both.
;
;       NCOLORS:      The number of colors loaded. By default, !D.TABLE_SIZE.
;
;       REVERSE:      If this keyword is set, the color table vectors are reversed.
;
;       SILENT:       This keyword is provided ONLY for compatibility with LOADCT. *All*
;                     color table manipulations are handled silently.
;
; EXAMPLES:
;
;       Suppose you wanted to create a color table that displayed negative values with
;       red-temperature values and positive values with blue-temperature values, and you
;       would like the red-temperature values to be reversed in the color table (so dark
;       colors adjoin in the color table and indicate values near zero). You could do this:
;
;           CTLoad, 0
;           CTLoad, 3, /REVERSE, CLIP=[32,240], BOTTOM=1, NCOLORS=10
;           CTLoad, 1, CLIP=[64, 245], BOTTOM=11, NCOLORS=10
;           Colorbar, NCOLORS=20, BOTTOM=1, DIV=10, RANGE=[-10,10]
;
;       Here is an example that shows the difference between LOADCT and CTLOAD:
;
;           ERASE, COLOR=FSC_COLOR('Charcoal)
;           LoadCT, 5, NCOLORS=8
;           Colorbar, NCOLORS=8, DIVISIONS=8, POSITION=[0.1, 0.65, 0.9, 0.75], XMINOR=0, XTICKLEN=1
;           CTLoad, 5, NCOLORS=8, CLIP=[16, 240]
;           Colorbar, NCOLORS=8, DIVISIONS=8, POSITION=[0.1, 0.35, 0.9, 0.45], XMINOR=0, XTICKLEN=1
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 30 October 2007.
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright � 2007 Fanning Software Consulting
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
PRO CTLOAD, table, $
   BOTTOM=bottom, $
   CLIP = clip, $
   RGB_TABLE=color_table, $
   FILE=file, $
   GET_NAMES=get_names, $
   NCOLORS=ncolors, $
   REVERSE=reverse, $
   SILENT=silent

   COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
   Compile_Opt idl2

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      Help, LAST_MESSAGE=1, OUTPUT=traceback
      Help, Calls=callStack
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
      Print,''
      Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
      Print, ''
      FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
      void = Dialog_Message(traceback[0], /Error, TITLE='Trapped Error')
      IF N_Elements(lun) NE 0 THEN Free_Lun, lun
      RETURN
   ENDIF

   ; Check keywords and arguments.
   IF N_Elements(table) EQ 0 THEN table = 0
   IF N_Elements(bottom) EQ 0 THEN bottom = 0 ELSE bottom = 0 > bottom < (!D.TABLE_SIZE-1)
   IF N_Elements(clip) EQ 0 THEN clip = [0,255]
   IF N_ELements(clip) EQ 1 THEN clip = [clip, 255]
   clip = 0 > clip < 255
   IF N_Elements(file) EQ 0 THEN file = Filepath('colors1.tbl', SUBDIRECTORY=['resource', 'colors'])

   ; Be sure !D.TABLE_SIZE is established.
   IF (!D.NAME EQ 'X') AND (!D.WINDOW EQ -1) THEN BEGIN
      Window, /Free, /Pixmap, XSIZE=10, YSIZE=10
      WDelete, !D.WINDOW
   ENDIF

   IF N_Elements(ncolors) EQ 0 THEN ncolors = !D.TABLE_SIZE
   reverse = KEYWORD_SET(reverse)

   ; Open and read the color table files.
   OPENR, lun, file, /GET_LUN
   ntables = 0B
   READU, lun, ntables

   ; Make sure table number is within range.
   IF (table GE ntables) OR (table LT 0) THEN $
      Message, 'Table number must be from 0 to ' + StrTrim(Fix(ntables)-1,2) + '.'

   ; Read the table names, if required, and return.
   IF Arg_Present(get_names) THEN BEGIN
      get_names = BytArr(32, ntables)
      Point_LUN, lun, ntables * 768L + 1
      READU, lun, get_names
      FREE_LUN, LUN
      get_names = StrTrim(get_names, 2)
      RETURN
   ENDIF

   ; Read the color table.
   theTables = Assoc(lun, BytArr(256), 1)
   r = theTables[table*3]
   g = theTables[table*3+1]
   b = theTables[table*3+2]

   ; Close the file.
   FREE_LUN, lun

   ; Clip the colors.
   r = r[clip[0]:clip[1]]
   g = g[clip[0]:clip[1]]
   b = b[clip[0]:clip[1]]
   nclipcolors = (clip[1]-clip[0]) + 1

   ; Interpolate to the number of colors asked for.
   IF ncolors NE nclipcolors THEN BEGIN
      p = (Lindgen(ncolors) * nclipcolors) / (ncolors-1)
      r = r[p]
      g = g[p]
      b = b[p]
   ENDIF

  ; Need to reverse the colors?
  IF reverse THEN BEGIN
     r = Reverse(r)
     g = Reverse(g)
     b = Reverse(b)
  ENDIF

  ; Load a color_table, if needed. Otherwise, load color vectors.
  IF Arg_Present(color_table) THEN BEGIN
     color_table = [[r], [g], [b]]
  ENDIF ELSE BEGIN
     r_orig = BYTSCL(Indgen(!D.TABLE_SIZE))
     g_orig = r_orig
     b_orig = r_orig
     r_orig[bottom] = r
     g_orig[bottom] = g
     b_orig[bottom] = b
     r_curr = r_orig
     g_curr = g_orig
     b_curr = b_orig
     TVLCT, r, g, b, bottom
  ENDELSE

