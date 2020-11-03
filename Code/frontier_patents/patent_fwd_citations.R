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

#### Load company information --------------------------------------------------
firm_dat <- readRDS(paste0(mainDir1,  "/created data/", "firm_reg.rds"))
head(firm_dat)
print("Firm data of patent ownership loaded")

#### Load inventor information -------------------------------------------------
inv_dat <- readRDS(paste0(mainDir1,  "/created data/", "inv_reg_CHcommute_adj.rds"))
head(inv_dat)
print("Inventor data of patents loaded")

#################################
####### Data processing #########
#################################

#### Patent information: -------------------------------------------------------
# Sometimes there several equivalent patents per p_key. In these cases, use 
# USPTO granted patents with the most 5-year citations as the reference patent.
tmp <- setDT(pat_dat)
keep_idx <- tmp[order(pat_off, fwd_cits5), .I[.N], keyby = p_key]$V1 # order by patent office and citations and choose the last obs per p_key
pat_dat <- pat_dat[keep_idx, ]
tmp <- NULL
paste("Obtained patents information of", nrow(pat_dat), "different patents cleaned for equivalents.")

#### Inventor information: -------------------------------------------------------
# Sometimes there are several equivalent patents per p_key and inventor. In these cases, only use 
# one patent per p_key
inv_dat <- inv_dat %>% distinct(p_key, name, .keep_all = TRUE) %>% as.data.frame()

# for every inventor_name / p_key combination assign the region, lat and long information to all patents
# ....

# function(df){
# 
#         # identify patents with missing information and subset to these observations 
#         NA_keys <- which(is.na(df$regio_inv))
#         tmp0 <- df[NA_keys, ]
#         tmp0 <- select(tmp0, - Up_reg_code, -regio_inv, -regio_pat)
#         
#         # search for the p_keys and inventors with region information in other patents
#         tmp1 <- df %>% select(p_key, name, Up_reg_code, regio_inv, regio_pat) %>% 
#                 filter(name %in% tmp0$name & p_key %in% tmp0$p_key & is.na(regio_inv) == FALSE)
#         
#         # join information from other patents to those with missing information
#         tmp0 <- left_join(x = tmp0, y = tmp1, by = c("p_key", "name"))
#         tmp0 <- tmp0[is.na(tmp0$regio_pat) == FALSE, ]
#         
#         }


#### firm information
# Sometimes there several equivalent patents per p_key and firm. In these cases, only use 
# one patent per p_key
firm_dat <- firm_dat %>% distinct(p_key, organization, .keep_all = TRUE) %>% as.data.frame()

#################################
####### Data merging ############
#################################

#### Assign firms to patent information data -----------------------------------
VARS <- colnames(firm_dat)[!names(firm_dat) %in% names(pat_dat)]
tmp <- firm_dat[, c("p_key", VARS)]
colnames(tmp)[-c(1:2)] <- paste0(colnames(tmp)[-c(1:2)], "_firm")
df <- full_join(pat_dat, tmp, by = "p_key")

#### Assign inventors to patent and firm information ---------------------------
VARS <- colnames(inv_dat)[!names(inv_dat) %in% names(df)]
VARS <- VARS[!VARS %in% c("ipc_main", "pat_off_name")]
tmp <- inv_dat[, c("p_key", VARS)]
change_names <- colnames(tmp)[colnames(tmp) %in% c("Up_reg_code", "lat", "lng")]
change_names <- which(colnames(tmp) %in% change_names)
colnames(tmp)[change_names] <- paste0(colnames(tmp)[change_names], "_inv")
df <- full_join(df, tmp, by = "p_key")
df[1234:1244, ]

#################################
####### Exploratory analysis ####
#################################


## subset to non-U.S. firms & frontier patents
tmp <- df %>% filter(country_firm != "US" & world_class_90 == 1)

## for every firm calculate the share of frontier patents with at least 1 inventor from the U.S. per year
# (1)
# => group_by firm and p_year
# => calculate number of distinct p_keys (= number of patents)
tmp <- tmp %>% group_by(organization, p_year) %>%
        distinct(p_key) %>% summarise(count = n())
tmp[tmp$organization == "saint gobain abrasifs", ]

# (2)
# => group_by firm and p_year
# => count number of patents with ctry_inv == "US"

# (3)
# => calculate share
tmp <- filter(df, organization == "saint gobain abrasifs")

## for every p_year, calculate the share of world class patents developed in the U.S.
# => is this share increasing?
# => what about specific tech_fields?
# is it different for frontier and non-frontier firms?

rm(list=ls())

# Ressources:
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html











