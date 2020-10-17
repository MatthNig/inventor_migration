#################################################################
# Description:    Script to evaluate migration flows of patent  #
#                 inventors into the U.S. and EU countries      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           16.10.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/..."

# Load names of patents' inventors --------------------------------------------
inv_reg <- readRDS(paste0(mainDir1, "/..."))
print("Names of patent inventors loaded")

####################################################
## Count number of inventors per country and year ##
####################################################


################################################################################
###### Calculate shares of foreign origin inventors per country and year #######
################################################################################


##########################
###### Plot Shares #######
##########################

