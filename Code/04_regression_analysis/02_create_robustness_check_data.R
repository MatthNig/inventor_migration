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
#setwd("/scicore/home/weder/nigmat01/inventor_migration")

#########################################################
####### Load data and functions for data analysis #######
#########################################################

#### patent data
df <- readRDS(paste0(datDir, "/pat_dat_all_final.rds"))

#### inventor flows
inv_dat <- readRDS(paste0(mainDir, "/created data/inventor_origin.rds"))

#### helper functions to identify attracted patents from foreign countries
source(paste0(getwd(), "/Code/04_regression_analysis/robustness_check_helper_functions.R"))

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

###############################################################################
### (E) Exclude tech-fields with largest inflows of non-western inventors #####
###############################################################################

# identify US tech_fields with largest relative inflows between 1985 and 2015:
tmp <- inv_dat %>% filter(Ctry_code == "US" & p_year %in% c(1985, 2015)) %>%
        group_by(tech_field, p_year) %>% summarize(count = n())
tmp <- assign_TechGroup(df = tmp) %>% group_by(TechGroup, p_year) %>% summarize(count = sum(count))
base_year <- tmp %>% filter(p_year == 1985) %>% rename(base_count = count) %>% select(-p_year)
tmp <- merge(tmp %>% filter(p_year == 2015), base_year, by = "TechGroup")
tmp <- tmp %>% mutate(inflow = count / base_count) %>% arrange(-inflow)
print("Exluding the following tech_fields:")
(excl_techfields <- tmp[tmp$inflow > 5, "TechGroup"])
# [1] "Information & Communication Technology" "Electrical Machinery"                  
# [3] "Audiovisual Technologies"               "Computer Science"                      
# [5] "Medical Technology"
tmp <- NULL
excl_techfields <- NULL
base_year <- NULL

#########################################################################
### (F) Calculate inventor numbers based on highest prediction only #####
#########################################################################

# (1) assign technological groups and TimePeriods to inventors
dat <- assign_TechGroup(df = inv_dat)
dat <- assign_TimePeriod(df = dat)

# get aggregate non-western origin shares:
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian", 
                        "Arabic", "Persian", "Turkey",
                        "SouthEastAsia")

# (2) calculate the number of inventors per TechGroup, region and Period:
dat <- filter(dat, Ctry_code == "US" & origin %in% NON_WESTERN_ORIGIN) %>%
        group_by(Up_reg_label, TechGroup, TimePeriod) %>%
        summarise(N_inv_nonwestern_classification = n()) %>% na.omit() %>%
        rename(regio_inv = Up_reg_label)

robustness_dat <- merge(robustness_dat, dat, 
                        by = c("regio_inv", "TechGroup", "TimePeriod"),
                        all.x = TRUE)

### impute these inventor figures: --------------------------------
dat <- assign_TechGroup(df = inv_dat)
dat <- assign_TimePeriod(df = dat)

# (1) spatial distribution of non-western origin in
# in the USA in the pre-1986 period:
initial_state_shares_fun <- function(df, origins){

  # predicted number of inventors with given origin in initial period at the country-level
  tmp <- df %>%
    filter(p_year < 1985 & Ctry_code == "US") %>%
    group_by(origin) %>% summarise(N_inv_overall_initial = n())


  # predicted number of inventors with given ethnic origin in initial period at state-level
  tmp0 <- df %>%
    filter(p_year < 1985 & Ctry_code == "US") %>%
    group_by(Up_reg_label, origin) %>% summarise(N_inv_state_initial = n())

  # bring them together and calculate the initial shares:
  tmp <- left_join(tmp0, tmp, by = "origin")
  tmp <- tmp %>% filter(origin %in% origins) %>%
    mutate(initial_state_share = N_inv_state_initial / N_inv_overall_initial)

  return(tmp)
}

initial_state_shares <- initial_state_shares_fun(df = dat,
                                                 origins = NON_WESTERN_ORIGIN)

# (2) For every non-western ethnic origin, calculate the stock of
# inventors for every TimePeriod at the country level
foreign_inv_stock_fun <- function(df, origins){

  tmp <- df %>%
    filter(Ctry_code == "US" & origin %in% origins) %>%
    group_by(TimePeriod, origin) %>% summarise(N_inv_overall = n())
  return(tmp)
}
foreign_inv_stock <- foreign_inv_stock_fun(df = dat, origins = NON_WESTERN_ORIGIN)

# (3) For every non-western ethnic origin, calculate the stock of inventors
# for every TimePeriod and industry
foreign_inv_stock_industry_fun <- function(df, origins){

  tmp <- df %>%
    filter(Ctry_code == "US" & origin %in% origins) %>%
    group_by(TimePeriod, TechGroup, origin) %>% summarise(N_inv_TechGroup = n())
  return(tmp)
}

foreign_inv_stock_industry <- foreign_inv_stock_industry_fun(df = dat,
                                                             origins = NON_WESTERN_ORIGIN)
foreign_inv_stock_industry <- filter(foreign_inv_stock_industry,
                                     is.na(TimePeriod) == FALSE)

# calculate industry shares per TimePeriod for every ethnic origin
foreign_inv_stock_industry <- merge(foreign_inv_stock_industry, foreign_inv_stock,
                                    by = c("TimePeriod", "origin"), all = TRUE)
foreign_inv_stock_industry <- mutate(foreign_inv_stock_industry,
                                     share_industry = N_inv_TechGroup / N_inv_overall)

# Test: --------------
test_fun <- function(){
  tmp <- foreign_inv_stock_industry %>% group_by(TimePeriod, origin) %>%
    filter(is.na(TimePeriod) == FALSE) %>% summarise(share = sum(share_industry, na.rm = TRUE))
  
  test_cond <- sum(tmp$share)
  
  if(test_cond == nrow(tmp)){print("All shares correctly calculated")}else{
    warning("Shares do not sum to 1")}
}
test_fun()
# -------------------------------
 
foreign_inv_stock_industry <- foreign_inv_stock_industry %>% na.omit() %>%
  select(TimePeriod, TechGroup, origin, share_industry)

# (3) For every non-western ethnic origin, calculate the total stock of inventors
# for every technology-state pair in every TimePeriod
inv_stock_regiotech_fun <- function(df){
  tmp <- df %>% filter(Ctry_code == "US") %>%
    group_by(TechGroup, Up_reg_label, TimePeriod) %>%
    summarise(total_inv_regiotech = n()) %>%
    filter(is.na(TimePeriod) == FALSE)
  return(tmp)
}

inv_stock_regiotech <- inv_stock_regiotech_fun(df = dat)

# combine indicators and calculate imputed foreign origin shares
dat <- merge(inv_stock_regiotech, initial_state_shares, by = "Up_reg_label")
dat <- merge(dat, foreign_inv_stock, by = c("TimePeriod", "origin"))
dat <- merge(dat, foreign_inv_stock_industry, by = c("TimePeriod", "TechGroup", "origin"))
dat <- dat %>% mutate(pred_N_foreign_inv = initial_state_share * share_industry * N_inv_overall) %>%
  group_by(TimePeriod, TechGroup, Up_reg_label) %>%
  summarise(pred_N_foreign_inv = sum(pred_N_foreign_inv),
            total_inv_regiotech = mean(total_inv_regiotech)) %>%
  mutate(non_western_share_imputed = pred_N_foreign_inv / total_inv_regiotech) %>%
  filter(total_inv_regiotech >= 30) %>%
  rename(regio_inv = Up_reg_label)

# merge to regression data:
dat <- dat %>% select(-total_inv_regiotech, -non_western_share_imputed) %>% 
        rename(imputed_N_inv_nonwestern_classification = pred_N_foreign_inv)
robustness_dat <- merge(robustness_dat, dat, 
                        by = c("regio_inv", "TechGroup","TimePeriod"),
                        all.x = TRUE)
print("Created and merged origin indicators based on highest origin prediction.")

foreign_inv_stock <- NULL
foreign_inv_stock_industry <- NULL
initial_state_shares <- NULL
inv_stock_regiotech <- NULL


#########################################################################
### (G) Calculate the number of offshoring firms instead of patents #####
#########################################################################

# (1) identify offshored patents to the USA
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = MIN_INV, 
                         world_class_indicator = FALSE)

dat <- assign_TechGroup(df = dat)
dat <- assign_TimePeriod(df = dat)

# (2) identify all unique firms per techfield and regio inv with offshored patents to the USA
dat <- dat %>% filter(onshored == 1 & ctry_inv == "US") %>% 
        distinct(organization, regio_inv, TechGroup, TimePeriod)
N_firms_overall <- dat %>% distinct(organization) %>% nrow() # N = 14'151
paste("Considering", N_firms_overall, "non-American firms with offshored patents to the USA")
dat <- dat %>% group_by(regio_inv, TechGroup, TimePeriod) %>% 
        summarize(N_offshoring_firms = n()) %>% na.omit()

# (3) add to robustness data
robustness_dat <- merge(robustness_dat, dat, 
                        by = c("regio_inv", "TechGroup","TimePeriod"),
                        all.x = TRUE)

################################################
### SAVE THE DATASET FOR ROBUSTNESS CHECKLS ####
################################################

write.csv(robustness_dat, 
          paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"),
          row.names = FALSE)
print("Saved dataset")



