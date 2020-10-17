#################################################################
# Description:    Script to classify inventors' origins based   #
#                 on the trained LSTM model.                    #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           17.10.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")
library("reticulate")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# function to encode names -----------------------------------------------------
source(paste0(getwd(),"/Code/training_data/names_encoding_function.R"))
print("Functions are loaded")

# Load names of patents' inventors --------------------------------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))
print("Names of patent inventors loaded")

#######################################
########## Preprocess names ###########
#######################################




#######################################
########## Encode all names ###########
#######################################




######################################
##### Classify inventors' origins ####
######################################




############################
##### Save the dataset #####
############################


