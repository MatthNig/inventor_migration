##################################################################
# Description:  Script to generate data for robustness checks.   #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 25.06.2021                                       #
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
setwd("/scicore/home/weder/nigmat01/inventor_migration")
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
source(paste0(getwd(), "/Code/04_regression_analysis/robustness_check_helper_functions.R"))

################################
### Use 5-year periods #########
################################

assign_5TimePeriod <- function(df){
  
  df <- mutate(df, TimePeriod = case_when(
    p_year %in% c(1978:1984) ~ "1980",
    p_year %in% c(1985:1989) ~ "1985",
    p_year %in% c(1990:1994) ~ "1990",
    p_year %in% c(1995:1999) ~ "1995",
    p_year %in% c(2000:2004) ~ "2000",
    p_year %in% c(2005:2009) ~ "2005",
    p_year %in% c(2010:2015) ~ "2010")
  )
}

##### prepare the raw datasets:
df <- assign_5TimePeriod(df)
df <- assign_TechGroup(df)
inv_dat <- assign_5TimePeriod(inv_dat)
inv_dat <- assign_TechGroup(inv_dat)

##### offshored patents:
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)
tmp <- onshoring_TechState(df = dat)
tmp <- tmp %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))

#### inventor prevalence:
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey",
                        "SouthEastAsia")
dat <- foreign_share_TechState(df = inv_dat, country = "US", min_inv = 30,
                               origins = NON_WESTERN_ORIGIN)
dat <- dat %>% rename(non_western_share = foreign_share)

#### merge and assign "regio_tech" dummies:
dat <- merge(dat, tmp,
             by = c("regio_inv", "TechGroup", "TimePeriod"),
             all = TRUE)
tmp <- NULL
dat <- dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
dat <- mutate(dat, onshored_patents = ifelse(is.na(onshored_patents), 0, onshored_patents))
REGIONS <- read.csv(paste0(getwd(), "/Data/patent_data/US_state_labels.csv"))
names(REGIONS) <- c("regio_inv", "reg_label")
dat <- merge(dat, REGIONS, by = "regio_inv")
TECH_GROUP_No <- dat %>% distinct(TechGroup) %>% mutate(TechGroup_No = paste0("0", rownames(.)))
dat <- merge(dat, TECH_GROUP_No, by = "TechGroup")
dat$regio_tech <- paste0(dat$reg_label, dat$TechGroup_No)
TECH_GROUP_No <- NULL
REGIONS <- NULL

#### patenting trends at TechGroup-Level
tmp <- df %>% filter(ctry_inv == "US") %>%
  distinct(p_key, .keep_all = TRUE)
tmp <- tmp %>% group_by(TimePeriod, TechGroup) %>% summarise(N_patents_TechGroup = n()) %>% na.omit()
dat <- left_join(dat, tmp, by = c("TimePeriod", "TechGroup"))

#### imputation:
# (1) For every selected ethnic origin, calculate it's inventors
# spatial distribution in the USA in the pre-1985 period:
initial_state_shares_fun <- function(df, origin){
  
  # predicted number of inventors with given origin in initial period at the country-level
  tmp <- df %>% filter(p_year < 1985 & Ctry_code == "US")
  names(tmp) <- gsub("prob_", "", names(tmp))
  tmp <- tmp %>% select(contains(origin)) %>%
    summarise_all(.funs = sum)
  N_foreign_inv_overall <- tmp[,]
  
  # predicted number of inventors with given origin in initial period at the state-level
  tmp <- df %>% filter(p_year < 1985 & Ctry_code == "US")
  names(tmp) <- gsub("prob_", "", names(tmp))
  tmp <- tmp %>% group_by(Up_reg_label) %>% select(contains(origin)) %>%
    summarise_all(.funs = sum)
  names(tmp) <- c("Up_reg_label", "N_foreign_inv_state")
  
  # bring them together and calculate the initial distribution across states:
  tmp <- tmp %>%
    mutate(initial_state_share = N_foreign_inv_state / N_foreign_inv_overall,
           origin = origin) %>%
    select(-N_foreign_inv_state)
  
  return(tmp)
}

ORIGINS <- c(NON_WESTERN_ORIGIN, "AngloSaxon")

initial_state_shares <- lapply(ORIGINS, function(x){
  tmp <- initial_state_shares_fun(df = inv_dat, origin = x)
  return(tmp)
}
)
initial_state_shares <- bind_rows(initial_state_shares)

# (2) For every selected ethnic origin, calculate the stock of
# inventors for every TimePeriod at the country level
foreign_inv_stock_fun <- function(df, origin){
  
  tmp <- df %>% filter(Ctry_code == "US")
  names(tmp) <- gsub("prob_", "", names(tmp))
  tmp <- tmp %>% group_by(TimePeriod) %>% select(contains(origin)) %>%
    summarise_all(.funs = sum)
  names(tmp) <- c("TimePeriod", "N_foreign_inv")
  tmp <- tmp %>% mutate(origin = origin)
  return(tmp)
}

foreign_inv_stock <- lapply(ORIGINS, function(x){
  tmp <- foreign_inv_stock_fun(df = inv_dat, origin = x)
  return(tmp)}
)
foreign_inv_stock <- bind_rows(foreign_inv_stock)
foreign_inv_stock <- filter(foreign_inv_stock, is.na(TimePeriod) == FALSE) %>%
  rename(N_inv_overall = N_foreign_inv)

# (3) For every selected ethnic origin, calculate the stock of inventors
# for every TimePeriod and industry
foreign_inv_stock_industry_fun <- function(df, origin){
  
  tmp <- df %>% filter(Ctry_code == "US")
  names(tmp) <- gsub("prob_", "", names(tmp))
  tmp <- tmp %>% group_by(TimePeriod, TechGroup) %>% select(contains(origin)) %>%
    summarise_all(.funs = sum)
  names(tmp) <- c("TimePeriod", "TechGroup", "N_inv")
  tmp <- tmp %>% mutate(origin = origin)
  return(tmp)
}

foreign_inv_stock_industry <- lapply(ORIGINS, function(x){
  tmp <- foreign_inv_stock_industry_fun(df = inv_dat, origin = x)
  return(tmp)}
)
foreign_inv_stock_industry <- bind_rows(foreign_inv_stock_industry)
foreign_inv_stock_industry <- filter(foreign_inv_stock_industry,
                                     is.na(TimePeriod) == FALSE)

# calculate industry shares per TimePeriod for every ethnic origin
foreign_inv_stock_industry <- merge(foreign_inv_stock_industry, foreign_inv_stock,
                                    by = c("TimePeriod", "origin"), all = TRUE)
foreign_inv_stock_industry <- mutate(foreign_inv_stock_industry,
                                     share_industry = N_inv / N_inv_overall)

# Test: --------------
test_fun <- function(){
  tmp <- foreign_inv_stock_industry %>% group_by(TimePeriod, origin) %>% summarise(share = sum(share_industry))
  test_count <- 0
  for(i in 1:nrow(tmp)){
    if(round(tmp$share[i], 3) != 1){warning(paste("Shares for origin", tmp$origin[i],
                                                  "in Period", tmp$TimePeriod[i], "do not sum to unity"))}else{
                                                    test_count <- test_count + 1}}
  if(test_count == nrow(tmp)){print("All shares correctly calculated")}
}
test_fun()
# -------------------------------

foreign_inv_stock_industry <- foreign_inv_stock_industry %>% na.omit() %>%
  select(TimePeriod, TechGroup, origin, share_industry)

# (4) Get the total stock of inventors
# for every technology-state pair in every TimePeriod
inv_stock_regiotech_fun <- function(df){
  tmp <- df %>% filter(Ctry_code == "US") %>%
    group_by(TechGroup, Up_reg_label, TimePeriod) %>%
    summarise(N_inv_regiotech = n()) %>%
    filter(is.na(TimePeriod) == FALSE)
  return(tmp)
}

inv_stock_regiotech <- inv_stock_regiotech_fun(df = inv_dat)

# (5) Calculate the imputed stock of inventors
# for every technology-state pair in every TimePeriod based on initial distribution
impute_inv_stock_regiotech_fun <- function(df){
  
  # initial inventor distribution across states
  initial_inv_total <- df %>% filter(Ctry_code == "US" & p_year < 1985) %>% nrow()
  initial_inv_state <- df %>%
    filter(Ctry_code == "US" & p_year < 1985) %>%
    group_by(Up_reg_label) %>%
    summarise(initial_inventors = n()) %>% mutate(initial_share_state = initial_inventors / initial_inv_total)
  
  # annual distribution of inventors across TechGroups:
  period_inv_total <- df %>% filter(Ctry_code == "US") %>%
    group_by(TimePeriod) %>% summarise(N_inv_total = n())
  period_inv_techgroup <- df %>% filter(Ctry_code == "US") %>%
    group_by(TimePeriod, TechGroup) %>%
    summarise(N_inv_TechGroup = n()) %>%
    filter(is.na(TimePeriod) == FALSE)
  period_inv_techgroup <- left_join(period_inv_techgroup, period_inv_total, by = "TimePeriod")
  period_inv_techgroup <- period_inv_techgroup %>%
    mutate(annual_share_TechGroup = N_inv_TechGroup / N_inv_total) %>%
    select(TimePeriod, TechGroup, annual_share_TechGroup, N_inv_total)
  
  # impute inventor stocks for regio-tech pairs:
  res <- inv_stock_regiotech[, c("TechGroup", "Up_reg_label", "TimePeriod")]
  res <- left_join(res,
                   initial_inv_state[, c("Up_reg_label", "initial_share_state")],
                   by = "Up_reg_label")
  res <- left_join(res, period_inv_techgroup, by = c("TechGroup", "TimePeriod"))
  res <- res %>% mutate(imputed_N_inv_regiotech = N_inv_total * annual_share_TechGroup * initial_share_state)
  res <- res %>% select(Up_reg_label, TechGroup, TimePeriod, imputed_N_inv_regiotech)
  
  return(res)
}

imputed_inv_stock_regiotech <- impute_inv_stock_regiotech_fun(df = inv_dat)

# combine indicators and calculate imputed foreign origin shares
res <- merge(inv_stock_regiotech, imputed_inv_stock_regiotech,
             by = c("TechGroup", "Up_reg_label","TimePeriod"), all = TRUE)
res <- merge(res, initial_state_shares, by = "Up_reg_label")
res <- merge(res, foreign_inv_stock, by = c("TimePeriod", "origin"))
res <- merge(res, foreign_inv_stock_industry, by = c("TimePeriod", "TechGroup", "origin"))
res <-mutate(res, imputed_N_foreign_inv = initial_state_share * share_industry * N_inv_overall)

# all non-western:
tmp0 <- res %>% filter(origin != "AngloSaxon") %>%
  group_by(TimePeriod, TechGroup, Up_reg_label) %>%
  summarise(imputed_N_foreign_inv = sum(imputed_N_foreign_inv),
            N_inv_regiotech = mean(N_inv_regiotech),
            imputed_N_inv_regiotech = mean(imputed_N_inv_regiotech)) %>%
  mutate(non_western_share_imputed = imputed_N_foreign_inv / imputed_N_inv_regiotech) %>%
  filter(N_inv_regiotech >= 30) %>%
  rename(regio_inv = Up_reg_label) %>%
  select(-N_inv_regiotech, -imputed_N_inv_regiotech)

# anglosaxon
tmp1 <- res %>% filter(origin == "AngloSaxon") %>%
  group_by(TimePeriod, TechGroup, Up_reg_label) %>%
  summarise(imputed_N_inv_anglosaxon = sum(imputed_N_foreign_inv),
            N_inv_regiotech = mean(N_inv_regiotech),
            imputed_N_inv_regiotech = mean(imputed_N_inv_regiotech)) %>%
  mutate(anglosaxon_share_imputed = imputed_N_inv_anglosaxon / imputed_N_inv_regiotech) %>%
  filter(N_inv_regiotech >= 30) %>%
  rename(regio_inv = Up_reg_label) %>%
  select(-N_inv_regiotech, -imputed_N_inv_regiotech)

# China & India:
res <- res %>% filter(origin %in% c("China", "India")) %>%
  group_by(TimePeriod, TechGroup, Up_reg_label) %>%
  summarise(imputed_N_inv_ChinaIndia = sum(imputed_N_foreign_inv),
            N_inv_regiotech = mean(N_inv_regiotech),
            imputed_N_inv_regiotech = mean(imputed_N_inv_regiotech)) %>%
  mutate(ChinaIndia_share_imputed = imputed_N_inv_ChinaIndia / imputed_N_inv_regiotech) %>%
  filter(N_inv_regiotech >= 30) %>%
  rename(regio_inv = Up_reg_label)

res <- merge(res, tmp0, by = c("TimePeriod", "TechGroup", "regio_inv"))
res <- merge(res, tmp1, by = c("TimePeriod", "TechGroup", "regio_inv"))

# merge to regression data:
dat <- left_join(dat, res, by = c("regio_inv", "TechGroup","TimePeriod"))

# clean:
dat$TimePeriod = as.character(dat$TimePeriod)
dat <- dat %>% mutate(N_inv_nonwestern = non_western_share * N_inv_regiotech) %>%
  rename(imputed_N_inv_nonwestern = imputed_N_foreign_inv)

# convert shares to percentage numbers
dat <- as.data.frame(dat)
dat[, grepl("share", names(dat))] <- dat[, grepl("share", names(dat))] * 100

##################################################
###### save the dataset with 5-year periods ######
##################################################

write.csv(dat, 
          paste0(getwd(), "/Data/regression_data/time_period_robustness.csv"),
          row.names = FALSE)
print("Saved dataset")
