;----------------------------------------------------------------------------
; $Id: div_vel_sph.pro,v 1.7 2002/11/21 22:56:38 johnny Exp $
;+
; NAME:
;   DIV_VEL_SPH
;
; PURPOSE:
;   Calculate divergence of horizontal velocity on a horizontal 2-D
;   finite difference grid on the surface of the earth (e.g. a CCM3 
;   Gaussian grid, a cylindrical grid) using a 9-point stencil.
;
; CATEGORY:
;   Atmospheric Sciences
;
; CALLING SEQUENCE:
;   Result = DIV_VEL_SPH(in_u, in_v, in_lon, in_lat)
;
; INPUTS:
;   in_u:      Zonal velocity [m/s], floating array dimensioned (NX,NY).
;              Unchanged by function.
;
;   in_v:      Meridional velocity [m/s], floating array dimensioned 
;              (NX,NY).  Unchanged by function.
;
;   in_lon:    Longitude [deg] for each x column of in_u and in_v,
;              floating array dimensioned (NX).  In degrees east 
;              [0,360].  Unchanged by function.
;
;   in_lat:    Latitude [deg] for each y row of in_u and in_v, floating
;              array dimensioned (NY).  In degrees north [-90,90].
;              Unchanged by function.
;
; INPUT KEYWORD PARAMETERS:
;   XPER:      If keyword is set, the input u and v are assumed to
;              wrap-around in the x-direction, i.e. that column index
;              NX (if there was one in the input arrays) is the same
;              as column index 0.  If keyword is not set, the input
;              are not assumed periodic in the x-direction.
;
; OUTPUT KEYWORD PARAMETERS:
;   None.
;
; OUTPUTS:
;   Result:    Velocity divergence [1/s], double floating dimensioned 
;              same as input variables.  Created/overwritten by 
;              function.
;
; FILE DEVICE I/O:
;   None.
;
; COMMON BLOCKS:
;   None.
;
; EXAMPLE:
;   Given the following made-up data and routine call:
;      lon = [0, 120, 240]
;      lat = [56, 54, 52]
;      u = RANDOMU(32,N_ELEMENTS(lon),N_ELEMENTS(lat))
;      v = RANDOMU(132,N_ELEMENTS(lon),N_ELEMENTS(lat))
;      result = DIV_VEL_SPH(u,v,lon,lat,/Xper)
;      PRINT, result
;   IDL prints:
;      7.1258348e-07   9.2000576e-07   1.3517769e-06
;      8.4462600e-07   7.4539150e-07   8.5726839e-07
;      9.5612383e-07   5.9408516e-07   3.5999669e-07
;
; REFERENCES:
; - Haltiner, G. J. and R. T. Williams (1980):  Numerical Prediction
;   and Dynamic Meteorology (New York:  John Wiley and Sons), ISBN
;   0-471-05971-4, Section 5-11.  Abbreviated HW80.
; - Tung, W.-W. (2002):  personal communication.  Fortran source code
;   to calculate divergence.
;
; MODIFICATION HISTORY:
; - 21 Nov 2002:  Orig. ver. Johnny Lin, CIRES/University of Colorado.
;   Email:  air_jlin@yahoo.com.  Passed passably adequate tests.
;
; NOTES:
; - Written for IDL 5.3.
; - Although grid spacing does not have to be identical (and thus 
;   Gaussian as well as cylindrical grids are accomodated), the 
;   definition of positive directions are with respect to the x and 
;   y-dimensions of the array (i.e. zonally and meridionally), and 
;   the 2-D finite difference grid is assumed to correspond one-to-one 
;   in orientation with the IDL array as written out by the PRINT
;   command.  Thus, element [0,0] is the upper- and left-most element, 
;   while [NX-1,NY-1] is the lower- and right-most element of the grid.  
;   As a result, the x-direction increases (in deg east) with increasing 
;   index value, while the y-direction increases (in deg north) with 
;   decreasing index value.  Thus, a Cartesian grid, for instance, 
;   will generally not be properly processed by this function since 
;   the x- and y-directions are not generally aligned zonally and 
;   meridionally.
; - The method for calculating divergence uses all outer points of the 
;   9 point stencil in order to ensure the flux and advection forms are 
;   consistent for interior points.  Differencing is (2nd order?)
;   centered.  I do not know if the flux/advection consistency is also 
;   true for boundary points since they use "centered" differencing on 
;   duplicated values.
; - It is assumed that the surface of the earth is a sphere.  To change
;   to another planet, changing parameter R_e should be enough (but this
;   hasn't been tested yet).
; - All keyword parameters are optional unless otherwise stated.
; - No procedures called with _Extra keyword are invoked.
; - No built-in functions and procedures called.
;-
; Copyright (c) 2002 Johnny Lin.  For licensing, distribution conditions,
; and contact information, see http://www.johnny-lin.com/lib.html.
;----------------------------------------------------------------------------

FUNCTION DIV_VEL_SPH, in_u, in_v, in_lon, in_lat  $
                    , XPER   = in_xper  $
                    , _EXTRA = extra




; -------------------- Error Check and Parameter Setting --------------------

COMPILE_OPT IDL2
ON_ERROR, 0

epsilon = (MACHAR()).eps         ;- Set machine precision tolerance
R_e     = 6.371d6                ;- Earth's mean radius [m]
d2r     = !DPI / 180.d0          ;- Factor to mult. to go from deg to rad

NX = N_ELEMENTS(in_lon)          ;- Set number of points in x and
NY = N_ELEMENTS(in_lat)          ;  y-directions

u_in   = in_u                                  ;- Protect input data and
v_in   = in_v                                  ;  make lon_in and lat_in
lon_in = REBIN(in_lon, NX, NY)                 ;  the input lon/lat vectors
lat_in = REBIN(REFORM(in_lat, 1,NY), NX, NY)   ;  in in_u and in_v dims

if (KEYWORD_SET(in_xper) eq 1) then  $         ;- Set flag for whether x
   xper = 1  $                                 ;  is periodic or not (xper
else  $                                        ;  =1 means is periodic)
   xper = 0

u_dim   = SIZE(u_in  , /Dimensions)            ;- Input array dimensions
v_dim   = SIZE(v_in  , /Dimensions)
lon_dim = SIZE(lon_in, /Dimensions)
lat_dim = SIZE(lat_in, /Dimensions)

tmp = [ (u_dim ne v_dim), (u_dim ne lon_dim)  $   ;- Check all input arrays
      , (u_dim ne lat_dim) ]                      ;  have same dimensions
if (TOTAL(tmp) gt epsilon) then  $
   MESSAGE, 'error--input dims not same'


;- Check input lon and lat vectors increase correctly/in correct range:

tmp = SORT(lon_in[*,0])  ne LINDGEN(NX)
if (TOTAL(tmp) gt epsilon) then MESSAGE, 'error--lon bad order'
tmp = SORT(-lat_in[0,*]) ne LINDGEN(NY)
if (TOTAL(tmp) gt epsilon) then MESSAGE, 'error--lat bad order'

if ( (MAX(lon_in) gt 360.0+epsilon) or  $
     (MIN(lon_in) lt 0.0-epsilon) ) then  $
   MESSAGE, 'error--bad lon range'

if ( (MAX(lat_in) gt  90.0+epsilon) or  $
     (MIN(lat_in) lt -90.0-epsilon) ) then  $
   MESSAGE, 'error--bad lat range'




; ------- Inflate Boundaries of of Input Array By 1-Element Every Side ------
;
; Algorithm:  The boundary of the input arrays are inflated by one on
; every side.  This allows use of the magnitude 1 SHIFT operator without 
; worrying about accounting for wraparound, as long as the final values
; kept do not include the boundary.

;- Add an extra column before and after the first and last columns,
;  (wrap-around if array is periodic in the x-direction; duplicate
;  boundaries if the array is not periodic):

if (xper eq 1) then begin                         ;+ case is periodic in x

   u   = [u_in[NX-1,*]  , u_in  , u_in[0,*]   ]
   v   = [v_in[NX-1,*]  , v_in  , v_in[0,*]   ]
   lon = [lon_in[NX-1,*], lon_in, lon_in[0,*] ]
   lat = [lat_in[NX-1,*], lat_in, lat_in[0,*] ]

endif else begin                                  ;+ case isn't periodic in x

   u   = [u_in[0,*]  , u_in  , u_in[NX-1,*]   ]
   v   = [v_in[0,*]  , v_in  , v_in[NX-1,*]   ]
   lon = [lon_in[0,*], lon_in, lon_in[NX-1,*] ]
   lat = [lat_in[0,*], lat_in, lat_in[NX-1,*] ]

endelse


;- Deallocate the *_in variables:

tmp = TEMPORARY(u_in)
tmp = TEMPORARY(v_in)
tmp = TEMPORARY(lon_in)
tmp = TEMPORARY(lat_in)
tmp = 0


;- Add an extra row before and after the top and bottom rows:

u   = [ [u[*,0]]  , [u]  , [u[*,NY-1]]   ]
v   = [ [v[*,0]]  , [v]  , [v[*,NY-1]]   ]
lon = [ [lon[*,0]], [lon], [lon[*,NY-1]] ]
lat = [ [lat[*,0]], [lat], [lat[*,NY-1]] ]




; -------- Allocate Velocity/Location Variables According to Stencil --------
;
; Algorithm:  The addresses for position refer to the following 9-point
; stencil (after HW80, Fig. 5.10):
;
;    6 --- 2 --- 5
;    |     |     |
;    3 --- 0 --- 1
;    |     |     |
;    7 --- 4 --- 8
;
; where the point "0" is the point where divergence is being calculated
; for.

u1 = 0.25d0 * ( SHIFT(u, -1, -1)  $
              + SHIFT(u, -1,  0) * 2.d0  $
              + SHIFT(u, -1,  1) )

u3 = 0.25d0 * ( SHIFT(u,  1, -1)  $
              + SHIFT(u,  1,  0) * 2.d0  $
              + SHIFT(u,  1,  1) )

tmp = TEMPORARY(u)   ;- Deallocate u


v2 = 0.25d0 * ( SHIFT(v,  1,  1)  $
              + SHIFT(v,  0,  1) * 2.d0  $
              + SHIFT(v, -1,  1) )

v4 = 0.25d0 * ( SHIFT(v,  1, -1)  $
              + SHIFT(v,  0, -1) * 2.d0  $
              + SHIFT(v, -1, -1) )

tmp = TEMPORARY(v)   ;- Deallocate v
tmp = 0




; --------------------------- Calculate Divergence --------------------------
;
; Algorithm:  Remember that the u,v,lon,lat arrays have an extraneous
; boundary.  Thus, the divergence (div) that is actually correct are
; the points in the interior that exclude the outer boundary.

;- Calculate base distances along the two axis through stencil point 0:

lon1 = SHIFT(lon, -1,  0)
lon3 = SHIFT(lon,  1,  0)
tmp  = TEMPORARY(lon)       ;- Deallocate lon
tmp  = 0

lat2 = SHIFT(lat,  0,  1)
lat4 = SHIFT(lat,  0, -1)
lat0 = TEMPORARY(lat)       ;- Set lat at point 0 and deallocate lon

udist = R_e * COS(lat0 * d2r)  $     ;- Zonal dist. betw. pt. 1 and 3
      * (lon1-lon3) * d2r
neg = WHERE(udist lt 0.0, count)     ;- Points where udist is < 0 are
if (count gt 0) then begin           ;  b/c lon3 gt lon1; recompute based
   tmp = R_e * COS(lat0 * d2r)  $    ;  upon the fact pt. 1 is east of
       * (lon1+360.d0-lon3) * d2r    ;  pt. 3
   udist[neg] = tmp[neg]
endif

vdist = R_e * (lat2-lat4) * d2r      ;- Meridional dist. betw. pt. 2 and 4

zeroes = WHERE(udist eq 0.0, count)  ;- Set 0 udist and vdist values to
if (count gt 0) then  $              ;  NaN to prevent division by 0
   udist[zeroes] = !VALUES.F_NAN
zeroes = WHERE(vdist eq 0.0, count)
if (count gt 0) then  $
   vdist[zeroes] = !VALUES.F_NAN


;- Calculate divergence (discard dummy boundary points):

dudx = (TEMPORARY(u1) - TEMPORARY(u3)) / TEMPORARY(udist)
dvdy = (TEMPORARY(v2) - TEMPORARY(v4)) / TEMPORARY(vdist)

div  = (TEMPORARY(dudx) + TEMPORARY(dvdy))[1:NX,1:NY]




; ------------------------------ Prepare Output -----------------------------

Result = TEMPORARY(div)         ;- Output divergence

RETURN, Result




END     ;=== end of function ===
 
; ========== end file ==========
