#################################################################
# Description:    Script to determine the number of patents'    #
#                 forward citations. Classification of frontier #
#                 patents based on these received citations.    #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           02.11.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}
#setwd(...)

#########################
####### Load data #######
#########################

#### Load patent information ---------------------------------------------------
#cit_dat <- readRDS(paste0(mainDir1,  "/created data/", "forward_cit_dat.rds"))
pat_dat <- readRDS(paste0(mainDir1,  "/created data/", "pat_dat.rds"))
head(pat_dat)
print("Data on patent citations loaded")

## if the same p_key has an USPTO and an EPO application, only consider the USPTO

#### Load company information ---------------------------------------------------
firm_dat <- readRDS(paste0(mainDir1,  "/created data/", "firm_reg.rds"))
head(firm_dat)

#### Load inventor information ---------------------------------------------------
inv_dat <- readRDS(paste0(mainDir1,  "/created data/", "inv_reg_CHcommute_adj.rds.rds"))
head(inv_dat)

#################################
####### Build the dataset #######
#################################

#### combine patents citations with firms -------------------------------------
VARS <- colnames(firm_dat)[!names(firm_dat) %in% names(pat_dat)]
tmp <- firm_dat[, c("p_key", VARS)]
df <- left_join(pat_dat, tmp, by = "p_key")

#### combine with inventors -------------------------------------
VARS <- colnames(inv_dat)[!names(inv_dat) %in% names(df)]
tmp <- inv_dat[, c("p_key", VARS)]
df <- left_join(df, tmp, by = "p_key")

## subset to non-U.S. firms
# ....

## for p_keys with multiple patent applications assign the region, lat and long to all patents
# ....

## if the same p_key has an USPTO and an EPO application, only consider the USPTO
tmp <- setDT(df)
tmp0 <- tmp[, .(.N, .I), by = p_key]
tmp0 <- tmp0[tmp0$N > 1, ]
tmp0 <- tmp0$I
tmp0 <- tmp[tmp0, ]
head(tmp0)

## for every p_year, calculate the share of world class patents developed in the U.S.
# => is this share increasing?
# => what about specific tech_fields?
# is it different for frontier and non-frontier firms?


# Ressources:
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/











