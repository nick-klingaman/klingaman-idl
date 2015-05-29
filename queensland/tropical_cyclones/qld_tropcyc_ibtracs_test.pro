PRO qld_tropcyc_ibtracts_test

; Test procedure for the IBTRACS dataset
ibtracs_file='/home/ss901165/datasets/IBTRACS/Basin.SP.ibtracs.v02r01.nc'

; Grab tropical cyclones
latitudes=OPEN_AND_EXTRACT(ibtracs_file,'lat')*0.01
longitude=OPEN_AND_EXTRACT(ibtracs_file,'lon')*0.01
years=OPEN_AND_EXTRACT(ibtracs_file,'season')



STOP

END

