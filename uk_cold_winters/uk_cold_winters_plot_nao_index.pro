PRO uk_cold_winters_plot_nao_index

; Makes a line plot of the monthly NAO index, given a netCDF file as input

; Give the name of the input file containing the NAO index
input_file='/home/ss901165/datasets/NAO_INDEX/nao_gibraltar_iceland.jan-dec_mmeans.1821-2011.nc'

; Read the NAO index from the file
; OPEN_AND_EXTRACT is a function that opens a netCDF file and extracts
; a given variable.  It can also read only a segment of a variable, using
; the offset=[x] and count=[y] options, where 
; [x] is the position from which to start reading (note that zero is the first position)
; [y] is the number of points to read

nao_index=OPEN_AND_EXTRACT(input_file,'index')

; You could read only the values from 1871 to the present with the following command:
;
; nao_index=OPEN_AND_EXTRACT(input_file,'index',offset=[600],count=[1692])
;
; offset=[600] because we want to skip the first 50 years (50 years x 12 months)
; count=[1692] because we want to read 141 years (1871-2011; 141 years x 12 months) 

; Now we need to know the total number of months that we will be plotting.  You can 
; find the number of points in a vector with the N_ELEMENTS(x) command, where x is
; the name of the vector
n_months=N_ELEMENTS(nao_index)

; This command replaces the missing values in the dataset with a missing value that IDL 
; understands.
nao_index[where(ABS(nao_index) ge 100)]=!Values.F_NaN

; Give the name for the PostScript format output file
psfile='/home/ss901165/idl/uk_cold_winters/uk_cold_winters_plot_nao_index.1821-2011.ps'

; This command opens the PostScript file and sets the font size and margins
PSOPEN,file=psfile,FONT=2,CHARSIZE=120,MARGIN=1000,SPACE2=300,XOFFSET=1000,YOFFSET=2000,$
       TFONT=2,TCHARSIZE=100

; This command sets the axis boundaries (x axis will be years; y axis will be NAO index)
GSET,XMIN=1820,YMIN=-8,XMAX=2012,YMAX=8

; This command sets up the x points for plotting. findgen(x) generates a vector of length
; (x) of numbers, starting from zero and incrementing by one.
months=findgen(n_months)/12.+1820

; This command draws the line plot.  The COL= command makes the line red.
GPLOT,X=months,Y=nao_index,COL=FSC_COLOR('red')

; This command draws the axes
AXES,XSTEP=10,YSTEP=1,XMINOR=5,YMINOR=0.5,XTITLE='Year',YTITLE='NAO index'

; This command closes the PostScript file and displays it on the screen.
PSCLOSE

; To stop the image displaying on the screen, use this command instead of the one above:
; PSCLOSE,/NOVIEW

STOP
END
