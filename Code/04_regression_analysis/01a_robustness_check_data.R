##################################################################
# Description:  Script to generate data for robustness checks.   #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 20.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

# directories  -----------------------------------------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
mainDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################################################
####### Load data and functions for data analysis #######
#########################################################

#### patent data
df <- readRDS(paste0(datDir, "/pat_dat_all_final.rds"))

#### inventor flows
inv_dat <- readRDS(paste0(mainDir, "/created data/inventor_origin.rds"))

#### helper functions to identify attracted patents from foreign countries
source(paste0(getwd(), "/Code/03_onshoring_analysis/onshoring_analysis_functions.R"))
source(paste0(getwd(), "/Code/04_regression_analysis/01b_robustness_check_helper_functions.R"))

#### baseline regression data
reg_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))
test_cols <- names(reg_dat)

#########################################################################
####### (A) Exclude patents from subsidiaries of non-US companies #######
#########################################################################

#### Collect data: only consider non-subsidiaries
dat <- df %>% dplyr::select(-country_firm_adj) %>% 
        dplyr::rename(country_firm_adj = country_firm)
dat <- country_onshoring(df = dat, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)
dat <- data_processing_fun(dat = dat)
# => "Data on onshored patents prepared. 129'776 onshored patents used for analysis"

#### Add the new variable to the robustness check data:
NEW_VAR <- "onshored_patents_exclsubsidiaries"
robustness_dat <- combine_dat_fun(dat = dat, merge_df = reg_dat, new_var = NEW_VAR)

#########################################################################
####### (B) Exclude onshored patents from companies in tax havens #######
#########################################################################

#### Collect data: discard tax havens
TAX_HAVENS <- c("CH", "IE", "NL", "LU", "BM", "KY")
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)
paste("Discard", nrow(dat[dat$country_firm_adj %in% TAX_HAVENS, ]), 
      "entries from patents assigned by companies in tax havens") # 954'511
dat <- dat %>% filter(!country_firm_adj %in% TAX_HAVENS)
dat <- data_processing_fun(dat = dat)
# => "Data on onshored patents prepared. 130'427 onshored patents used for analysis"

#### Add the new variable to the robustness check data:
NEW_VAR <- "onshored_patents_notaxhavens"
robustness_dat <- combine_dat_fun(dat = dat, 
                                  merge_df = robustness_dat, 
                                  new_var = NEW_VAR)

############################################################################
### (C) At least two US based inventors involved in patent development #####
############################################################################

#### Collect data: Minimum 2 inventors from the USA per patent
MIN_INV <- 2
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = MIN_INV, 
                         world_class_indicator = FALSE)
dat <- data_processing_fun(dat = dat)
# => "Data on onshored patents prepared. 114'749 onshored patents used for analysis"

#### add the new variable to the baseline data:
NEW_VAR <- "onshored_patents_twoinv"
robustness_dat <- combine_dat_fun(dat = dat, 
                                  merge_df = robustness_dat, 
                                  new_var = NEW_VAR)

############################################################
### (D) Only world class onshored patents (in top 10%) #####
############################################################

dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = TRUE)
dat <- data_processing_fun(dat = dat)
# => "Data on onshored patents prepared. 28'650  onshored patents used for analysis"

#### add the new variable to the baseline data:
NEW_VAR <- "onshored_patents_worldclass"
robustness_dat <- combine_dat_fun(dat = dat, 
                                  merge_df = robustness_dat, 
                                  new_var = NEW_VAR)

###############################################
### (D) Exclude largest sending countries #####
###############################################

#### Collect data: discard tax havens
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)
sending_countries <- subset(dat, onshored == 1)
(sending_countries <- sending_countries %>% 
                group_by(country_firm_adj) %>% 
                summarise(count = n()) %>% 
                arrange(-count))
sending_countries <- sending_countries[1:2, ]$country_firm_adj

paste("Discard", nrow(dat[dat$country_firm_adj %in% sending_countries, ]), 
      "entries from patents assigned by companies in tax havens") # 6'949'469
dat <- dat %>% filter(!country_firm_adj %in% sending_countries)
dat <- data_processing_fun(dat = dat)
# => "Data on onshored patents prepared. 116'624  onshored patents used for analysis"

#### Add the new variable to the robustness check data:
NEW_VAR <- "onshored_patents_excldejp"
robustness_dat <- combine_dat_fun(dat = dat, 
                                  merge_df = robustness_dat, 
                                  new_var = NEW_VAR)



################################################
### SAVE THE DATASET FOR ROBUSTNESS CHECKLS ####
################################################

write.csv(dat, paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"),
          row.names = FALSE)
print("Saved dataset")



