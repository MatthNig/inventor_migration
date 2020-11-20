##################################################################
# Description:  Generates a dataset that features information    #
#               on the owner, number of citations, the inventors #
#               and technological fields of a patent.            #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Date:         19.11.2020                                       #
##################################################################

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

################################################
####### Load functions for data analysis #######
################################################

source(paste0(getwd(), "/Code/frontier_patents/onshoring_analysis_functions.R"))

#########################
####### Load data #######
#########################

#### Load patent information ---------------------------------------------------
pat_dat <- readRDS(paste0(mainDir1,  "/created data/", "pat_dat.rds"))
print("Data on patent citations loaded")

#### Load company information --------------------------------------------------
firm_dat <- readRDS(paste0(mainDir1,  "/created data/", "firm_reg.rds"))
print("Firm data of patent ownership loaded")

#### Load inventor information -------------------------------------------------
inv_dat <- readRDS(paste0(mainDir1,  "/created data/", "inv_reg_CHcommute_adj.rds"))
print("Inventor data of patents loaded")

print("All data loaded")

#################################
####### Data processing #########
#################################

#### Patent information: -------------------------------------------------------

# Problem:
# Sometimes there several equivalent patents per p_key. E.g. the same invention 
# is protected by a US and European patent. In these cases, use the USPTO patents 
# with the most 5-year citations as the reference patent for this p_key.

# order by patent office and citations and choose the last obs per p_key 
# i.e. USPTO with the most citations.
tmp <- setDT(pat_dat)
keep_idx <- tmp[order(pat_off, fwd_cits5), .I[.N], keyby = p_key]$V1 
pat_dat <- pat_dat[keep_idx, ]
paste("Obtained patents information of", nrow(pat_dat), 
      "different patents cleaned for equivalents.")
# clean:
tmp <- NULL
keep_idx <- NULL

#### Inventor information: -----------------------------------------------------

# Problem:
# Sometimes there several equivalent patents per p_key and inventor. 
# E.g. the same invention is protected by a US and European patent. 
# To omit these duplicated patent-inventor pairs, only take one combination per p_key
# However, on EP patents there is information on regional location of inventors but not
# on coordinates. For USPTO its the opposite. Thus, before omitting duplicated patents
# assign the region, lat and long information to all observations per every inventor_name-p_key combination

# ## !!!! not run: inefficent... p_key time name is way too many observations..
# # idea: first identify the NAs, then search for the same inventor/p-key combination
# # only then do the computations
# assign_geo_info <- function(df, vars){
#         
#         # convert to data.table and keep row-indexes
#         df <- setDT(df, key = c("p_key", "name"))[, idx := rownames(df)]
# 
#         # filter to inventors where one patent equivalent has geographical 
#         # information and the other does not.
#         geo_info <- df[, .SD[.N > 1], .SDcols = c(vars, "idx"), by = .(p_key, name)]
#         # NA_obs <- geo_info[is.na(geo_info$lat), c("p_key", "name", "idx")]
#         NA_obs <- geo_info %>% 
#                 filter_at(vars(vars[1]), all_vars(is.na(.))) %>%
#                 select(p_key, name, idx)
#         geo_info <- geo_info[complete.cases(geo_info),]
#         
#         # assign the information from the one patent to the other
#         # and replace observations in the original data
#         NA_obs <- setDT(geo_info %>% select(-idx), key =c("p_key", "name"))[NA_obs]
#         NA_obs$idx <- as.numeric(NA_obs$idx)
#         NA_obs <- NA_obs[complete.cases(NA_obs), ] %>% as.data.frame()
#         
#         # return as a data.frame
#         df <- as.data.frame(df)
#         df[NA_obs$idx, c(vars)] <- NA_obs[, c(vars)]
#         
#         return(df)
# }
# 
# # Assign geo_information on all equivalent patents
# inv_dat <- assign_geo_info(df = inv_dat, vars = c("lat", "lng"))
# inv_dat <- assign_geo_info(df = inv_dat, vars = "regio_inv")

# Subset inventors to unique patent-inventor combination
inv_dat <- inv_dat %>%
        distinct(p_key, name, .keep_all = TRUE) %>% 
        as.data.frame()

#### firm information -----------------------------------------------------

# Problem:
# Sometimes there several equivalent patents per p_key and firm 
# E.g. the same invention is protected by a US and European patent. 
# To omit these duplicated patent-firm pairs, only take one combination per p_key
firm_dat <- firm_dat %>% 
        distinct(p_key, organization, .keep_all = TRUE) %>% 
        as.data.frame()

print("All data processing completed")

#################################
####### Data merging ############
#################################

#### Combine information on firms with patent characteristics ------------------
VARS <- colnames(firm_dat)[!names(firm_dat) %in% names(pat_dat)]
tmp <- firm_dat[, c("p_key", VARS)]
colnames(tmp)[-c(1:2)] <- paste0(colnames(tmp)[-c(1:2)], "_firm")
df <- merge(setDT(pat_dat, key = "p_key"), 
            setDT(tmp, key = "p_key"), 
            by = "p_key", all = TRUE) # merge.data.table
# clean:
tmp <- NULL

#### Combine information on inventors with patent and firm characteristic-------
VARS <- colnames(inv_dat)[!names(inv_dat) %in% names(df)]
VARS <- VARS[!VARS %in% c("ipc_main", "pat_off_name")]
tmp <- inv_dat[, c("p_key", VARS)]
change_names <- colnames(tmp)[colnames(tmp) %in% c("Up_reg_code", "lat", "lng")]
change_names <- which(colnames(tmp) %in% change_names)
colnames(tmp)[change_names] <- paste0(colnames(tmp)[change_names], "_inv")
df <- merge(setDT(df, key = "p_key"), 
            setDT(tmp, key = "p_key"), 
            by = "p_key", all = TRUE) # merge.data.table
# clean:
tmp <- NULL

#### Subset to variables of interest:
exclude_vars <- c("grant_lag", "npl_cits", "pub_date", "pub_nbr_oecd",
                  "pub_nbr_firm", "cross_bord")
df <- as.data.frame(df)
df <- df[,!colnames(df) %in% exclude_vars]

print("Patent characteristics combined with firm and inventor information.")

##############################
####### Save the data ########
##############################

# save the dataset
save_path <- "/scicore/home/weder/nigmat01/Data"
saveRDS(object = df,
        file = paste0(save_path, "/pat_dat_all.rds"))
print("Data merged and saved as 'pat_dat_all.rds'.")

# clean everything from global environment
rm(list=ls())


