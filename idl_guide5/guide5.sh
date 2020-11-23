#!/bin/sh
#Setup file for idl_guide5

#Modify the next line to point to where you installed the idl_guide5 directory
base_dir=/home/users/npklingaman/idl/idl_guide5

#
#No modifications should be needed below this line
#

GUIDE_LIB=$base_dir/lib
GUIDE_EX=$base_dir/examples
GUIDE_DATA=$base_dir/data

export IDL_PATH=+$base_dir:$IDL_PATH
export PATH IDL_PATH GUIDE_LIB GUIDE_EX GUIDE_DATA
alias gex='cd $GUIDE_EX'
alias glib='cd $GUIDE_LIB'
alias gdata='cd $GUIDE_DATA'


