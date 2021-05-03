##################################################################
# Description:  Script to generate a dataset of onshored patents #
#               and inventor flows for regression analysis. The  #
#               data is at the level of technological fields and #
#               U.S. states per year.                            #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 31.03.2021                                       #
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

#### on-/off-shoring of patents
df <- readRDS(paste0(datDir, "/pat_dat_all_final.rds"))

#### inventor flows
inv_dat <- readRDS(paste0(mainDir, "/created data/inventor_origin.rds"))

#### functions to identify attracted patents from foreign countries
source(paste0(getwd(), "/Code/03_onshoring_analysis/onshoring_analysis_functions.R"))

######################################################################
##### Patents offshored to the USA by technology area and State ######
######################################################################

# (1) assign U.S. affiliates of foreign companies to their headquarter country:
df <- df %>% select(-country_firm) %>% rename(country_firm = country_firm_adj)

# (2) identify all patents that have been offshored to the U.S. by foreign firms:
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)

# (3) Define technological groups and assign patents to them:
assign_TechGroup <- function(df){
        
        df <- mutate(df, TechGroup = case_when(
                tech_field %in% c(1) ~ "Electrical Machinery",
                tech_field %in% c(2) ~ "Audiovisual Technologies",
                tech_field %in% c(3:5, 7) ~ "Information & Communication Technology",
                tech_field %in% c(6) ~ "Computer Science",
                tech_field %in% c(8) ~ "Semiconducturs",
                tech_field %in% c(9:12) ~ "Instruments",
                tech_field %in% c(13) ~ "Medical Technology",
                tech_field %in% c(14:16) ~ "Pharmaceuticals & Biotechnology",
                tech_field %in% c(18:23) ~ "Chemistry & Materials",
                tech_field %in% c(25, 26, 28, 29, 31) ~ "Machines & Mechanical Engineering",
                tech_field %in% c(24, 27, 30) ~ "Engines, Turbines, Thermal & Environmental Technologies",
                tech_field %in% c(32) ~ "Transport",
                tech_field %in% c(33:35) ~ "Consumer Goods & Civil Engineering"
                )
        )
}

dat <- assign_TechGroup(df = dat)

# Note: selection of groups is motivated from Schmoch (2008):
# http://www.world-intellectual-property-organization.com/edocs/mdocs/classifications/en/ipc_ce_41/ipc_ce_41_5-annex1.pdf
# and Hall (2001):
# https://www.nber.org/system/files/working_papers/w8498/w8498.pdf

# (4) Define 5- or 3-year periods and assign patents to them:

# previously:
# p_year %in% c(1978:1982) ~ "1982",
# p_year %in% c(1983:1987) ~ "1987",

assign_TimePeriod <- function(df){
        
        df <- mutate(df, TimePeriod = case_when(
          p_year %in% c(1978:1984) ~ "1980",
          p_year %in% c(1985:1988) ~ "1988",
          p_year %in% c(1989:1991) ~ "1991",
          p_year %in% c(1992:1994) ~ "1994",
          p_year %in% c(1995:1997) ~ "1997",
          p_year %in% c(1998:2000) ~ "2000",
          p_year %in% c(2001:2003) ~ "2003",
          p_year %in% c(2004:2006) ~ "2006",
          p_year %in% c(2007:2009) ~ "2009",
          p_year %in% c(2010:2012) ~ "2012",
          p_year %in% c(2013:2015) ~ "2015")
        )
}
dat <- assign_TimePeriod(df = dat)
paste(nrow(dat), "patents used for analysis") # N = 13'049'201

# (5) calculate the number of offshored patents to the U.S. by TechGoup, Period and State:
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, TimePeriod)]
        
        return(onshored_pat)
}

dat <- onshoring_TechState(df = dat)

# (6) Summarize:
tmp <- dat %>% group_by(regio_inv) %>% 
        summarize(onshored_patents = sum(onshored_patents)) %>%
        mutate(share = onshored_patents / sum(onshored_patents)) %>%
        arrange(-onshored_patents)
paste0(tmp[is.na(tmp$regio_inv), ]$onshored_patents, 
       " onshored patents (", round(tmp[is.na(tmp$regio_inv), ]$share,3)*100,
       "%) had missing technology field or state information.") # N=98315 (40.3%)
paste(sum(tmp[is.na(tmp$regio_inv) == FALSE, ]$onshored_patents), 
      "onshored patents assigned to technology-state pairs.") # N=145'790
tmp <- NULL

# (7) Discard NA's
dat <- dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
print("Data on onshored patents prepared.")

######################################################################
##### Ethnic origin shares of inventors by technology and State ######
######################################################################

# (1) assign technological groups and TimePeriods to inventors
inv_dat <- assign_TechGroup(df = inv_dat)
inv_dat <- assign_TimePeriod(df = inv_dat)

# (2) calculate origin shares by Region, TechGroup and Year
foreign_share_TechState <- function(df, country,
                                    origins, min_inv){
        
        # calculate total number of inventors per region and TechGroup-field
        total_inventors <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, TimePeriod) %>%
                summarise(total_inventors = n())
        
        # drop all origins that are not of interest:
        DROP_ORIGINS <- names(df)[grepl("prob_", names(df))]
        DROP_ORIGINS <- DROP_ORIGINS[!DROP_ORIGINS %in% paste0("prob_", origins)]
        df <- df[, !names(df) %in% DROP_ORIGINS]
        
        # sum up origin probabilities per region and TechGroup-field
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, TimePeriod) %>%
                select(contains("prob")) %>%
                summarise_all(.funs = sum)

        # calculate share of foreign origin per region and tech-field
        tmp <- merge(tmp, total_inventors, 
                     by = c("Up_reg_label", "TechGroup", "TimePeriod"))
        tmp <- filter(tmp, total_inventors >= min_inv)
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        tmp <- gather(tmp, key = "origin", value = "share", -TimePeriod, -Up_reg_label,
                      -TechGroup, -total_inventors)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp <- tmp %>% group_by(Up_reg_label, TechGroup, TimePeriod) %>%
                summarize(foreign_share = sum(share)) %>%
                rename(regio_inv = Up_reg_label)
        tmp <- tmp %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
        
        return(tmp)
}

# get aggregate non-western origin shares:
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian", 
                        "Arabic", "Persian", "Turkey",
                        "SouthEastAsia")
origin_share_dat <- foreign_share_TechState(df = inv_dat, country = "US", min_inv = 30,
                               origins = NON_WESTERN_ORIGIN)
origin_share_dat <- origin_share_dat %>% rename(non_western_share = foreign_share)

# get non-domestic ethnic origin share
tmp <- foreign_share_TechState(df = inv_dat, country = "US", 
                               min_inv = 30,
                               origins = "AngloSaxon")
tmp$non_domestic_share <- 1 - tmp$foreign_share
tmp <- tmp[, names(tmp) != "foreign_share"]
origin_share_dat <- merge(origin_share_dat, tmp, 
                          by = c("regio_inv","TechGroup","TimePeriod"),
                          all = TRUE)

# Add Chinese and Indian ethnic origin shares:
tmp <- lapply(c("China", "India"), function(x){
  tmp <- foreign_share_TechState(df = inv_dat, country = "US", min_inv = 30, origins = x)
  names(tmp)[ncol(tmp)] <- paste0(x, "_share")
  return(tmp)
  })
for (i in 1:length(tmp)){
  origin_share_dat <- merge(origin_share_dat, tmp[[i]], 
        by = c("regio_inv","TechGroup","TimePeriod"),
        all = TRUE)
  }
tmp <- NULL
print("Data on inventor origin shares prepared.")

##############################################
########### Combine and clean data ###########
##############################################

# (1) merge onshoring and inventor origin information
dat <- merge(dat, origin_share_dat,
             by = c("regio_inv", "TechGroup", "TimePeriod"), 
             all = TRUE)
origin_share_dat <- NULL
print("Merged the number of offshored patents and inventor origin information")

# (2) clean the dataset and set NA onshoring observations to 0
dat <- dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
dat <- mutate(dat, onshored_patents = ifelse(is.na(onshored_patents), 0, onshored_patents))

# (3) assign "regio_tech" dummies
REGIONS <- read.csv(paste0(getwd(), "/Data/patent_data/US_state_labels.csv"))
names(REGIONS) <- c("regio_inv", "reg_label")
dat <- merge(dat, REGIONS, by = "regio_inv")
TECH_GROUP_No <- dat %>% distinct(TechGroup) %>% mutate(TechGroup_No = paste0("0", rownames(.)))
dat <- merge(dat, TECH_GROUP_No, by = "TechGroup")
dat$regio_tech <- paste0(dat$reg_label, dat$TechGroup_No)

#################################################################
########### Construct Controls for Patenting Trends ##############
#################################################################

# get unique patents with at least one U.S. inventor for every tech_group and TimePeriod
tmp <- df %>% filter(ctry_inv == "US") %>%
  distinct(p_key, .keep_all = TRUE)
tmp <- assign_TechGroup(df = tmp)
tmp <- assign_TimePeriod(df = tmp)

# add overall patenting trends at state level
controls <- tmp %>% group_by(TimePeriod, regio_inv) %>% summarise(N_patents_state = n()) %>% na.omit()
dat <- left_join(dat, controls, by = c("TimePeriod", "regio_inv"))

# overall patenting trends at technology field level
controls <- tmp %>% group_by(TimePeriod, TechGroup) %>% summarise(N_patents_TechGroup = n()) %>% na.omit()
dat <- left_join(dat, controls, by = c("TimePeriod", "TechGroup"))

print("Added overall patenting trends as controls.")

#################################################################
########### Construct weights for regression analysis ###########
#################################################################

## weights based on initial sample period patenting shares:
weights <- df %>% filter(ctry_inv == "US" & p_year < 1985) %>%
  distinct(p_key, .keep_all = TRUE)
weights <- merge(weights, REGIONS, by = "regio_inv")
weights <- assign_TechGroup(weights)
weights <- merge(weights, TECH_GROUP_No, by = "TechGroup")
weights$regio_tech <- paste0(weights$reg_label, weights$TechGroup_No)
weights <- weights %>% group_by(regio_tech) %>% 
  summarise(N_patents = n(),
            weight_initial_patents = N_patents / nrow(weights)) %>%
  select(-N_patents)
dat <- merge(dat, weights, by = "regio_tech", all.x = TRUE)

print("Added weights to technology-state pairs based on inital sample period.")

#################################################################
########### Construct imputed explanatory variables #############
#################################################################

#### Imputation based on: Card, 2001): --------------------------------------------------

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

# -------------------------classification based---------------------------------

# # (1) For every non-western ethnic origin, calculate it's spatial distribution of
# # in the USA in the pre-1986 period:
# initial_state_shares_fun <- function(df, origins){
# 
#   # predicted number of inventors with given origin in initial period at the country-level
#   tmp <- df %>%
#     filter(p_year < 1986 & Ctry_code == "US") %>%
#     group_by(origin) %>% summarise(N_inv_overall_initial = n())
# 
# 
#   # predicted number of inventors with given ethnic origin in initial period at state-level
#   tmp0 <- df %>%
#     filter(p_year < 1986 & Ctry_code == "US") %>%
#     group_by(Up_reg_label, origin) %>% summarise(N_inv_state_initial = n())
# 
#   # bring them together and calculate the initial shares:
#   tmp <- left_join(tmp0, tmp, by = "origin")
#   tmp <- tmp %>% filter(origin %in% origins) %>%
#     mutate(initial_state_share = N_inv_state_initial / N_inv_overall_initial)
# 
#   return(tmp)
# }
# 
# initial_state_shares <- initial_state_shares_fun(df = inv_dat,
#                                                  origins = NON_WESTERN_ORIGIN)
# 
# # (2) For every non-western ethnic origin, calculate the stock of
# # inventors for every TimePeriod at the country level
# foreign_inv_stock_fun <- function(df, origins){
# 
#   tmp <- df %>%
#     filter(Ctry_code == "US" & origin %in% origins) %>%
#     group_by(TimePeriod, origin) %>% summarise(N_inv_overall = n())
#   return(tmp)
# }
# foreign_inv_stock <- foreign_inv_stock_fun(df = inv_dat, origins = NON_WESTERN_ORIGIN)
# 
# # (3) For every non-western ethnic origin, calculate the stock of inventors
# # for every TimePeriod and industry
# foreign_inv_stock_industry_fun <- function(df, origins){
# 
#   tmp <- df %>%
#     filter(Ctry_code == "US" & origin %in% origins) %>%
#     group_by(TimePeriod, TechGroup, origin) %>% summarise(N_inv_TechGroup = n())
#   return(tmp)
# }
# 
# foreign_inv_stock_industry <- foreign_inv_stock_industry_fun(df = inv_dat,
#                                                              origins = NON_WESTERN_ORIGIN)
# foreign_inv_stock_industry <- filter(foreign_inv_stock_industry,
#                                      is.na(TimePeriod) == FALSE)
# 
# # calculate industry shares per TimePeriod for every ethnic origin
# foreign_inv_stock_industry <- merge(foreign_inv_stock_industry, foreign_inv_stock,
#                                     by = c("TimePeriod", "origin"), all = TRUE)
# foreign_inv_stock_industry <- mutate(foreign_inv_stock_industry,
#                                      share_industry = N_inv_TechGroup / N_inv_overall)
# 
# # Test: --------------
# test_fun <- function(){
#   tmp <- foreign_inv_stock_industry %>% group_by(TimePeriod, origin) %>%
#     filter(is.na(TimePeriod) == FALSE) %>% summarise(share = sum(share_industry, na.rm = TRUE))
#   test_cond <- sum(tmp$share)
#   if(test_count == nrow(tmp)){print("All shares correctly calculated")}else{
#     warning("Shares do not sum to 1")}
# }
# test_fun()
# # -------------------------------
# 
# foreign_inv_stock_industry <- foreign_inv_stock_industry %>% na.omit() %>%
#   select(TimePeriod, TechGroup, origin, share_industry)
# 
# # (3) For every non-western ethnic origin, calculate the total stock of inventors
# # for every technology-state pair in every TimePeriod
# inv_stock_regiotech_fun <- function(df){
#   tmp <- df %>% filter(Ctry_code == "US") %>%
#     group_by(TechGroup, Up_reg_label, TimePeriod) %>%
#     summarise(total_inv_regiotech = n()) %>%
#     filter(is.na(TimePeriod) == FALSE)
#   return(tmp)
# }
# 
# inv_stock_regiotech <- inv_stock_regiotech_fun(df = inv_dat)
# 
# # combine indicators and calculate imputed foreign origin shares
# res <- merge(inv_stock_regiotech, initial_state_shares, by = "Up_reg_label")
# res <- merge(res, foreign_inv_stock, by = c("TimePeriod", "origin"))
# res <- merge(res, foreign_inv_stock_industry, by = c("TimePeriod", "TechGroup", "origin"))
# res <- res %>% mutate(pred_N_foreign_inv = initial_state_share * share_industry * N_inv_overall) %>%
#   group_by(TimePeriod, TechGroup, Up_reg_label) %>%
#   summarise(pred_N_foreign_inv = sum(pred_N_foreign_inv),
#             total_inv_regiotech = mean(total_inv_regiotech)) %>%
#   mutate(non_western_share_imputed = pred_N_foreign_inv / total_inv_regiotech) %>%
#   filter(total_inv_regiotech >= 30) %>%
#   rename(regio_inv = Up_reg_label)
# 
# # merge to regression data:
# dat <- left_join(dat, res, by = c("regio_inv", "TechGroup","TimePeriod"))
# 
# # test:
# test_dat <- dat#[dat$non_western_share_imputed < 1, ]
# cor(test_dat$non_western_share_imputed, test_dat$non_western_share, use = "complete.obs")
# ggplot(data = test_dat, aes(x = non_western_share_imputed * 100, y = non_western_share * 100))+
#   geom_point()+geom_abline(intercept = 0, slope = 1, color = "blue")

print("Created instrument based on spatial distribution")

#################################################################
###### Construct lag variables for regression analysis ##########
#################################################################

# LAG_VARS <- names(dat)[grepl("share", names(dat))]
# tmp <- dat %>% group_by(regio_tech) %>% arrange(TimePeriod) %>%
#   mutate_at(c(LAG_VARS), ~dplyr::lag(., 1))
# idx_pos <- which(names(tmp) %in% LAG_VARS)
# names(tmp)[idx_pos] <- paste0("lag1_", LAG_VARS)
# tmp <- tmp[, c("regio_tech", "TimePeriod", paste0("lag1_", LAG_VARS))]
# dat <- merge(dat, tmp, by = c("regio_tech", "TimePeriod"))
# 
# print("Added lagged explanatory variables.")

########################################################
####### Save the dataset for regression analysis #######
########################################################

# create time trend variable
tmp <- data.frame(TimePeriod = sort(unique(dat$TimePeriod)), 
                  trend = seq(1, length(unique(dat$TimePeriod))))
dat <- left_join(dat, tmp, by = "TimePeriod")
dat$TimePeriod = as.character(dat$TimePeriod)

# calculate further metrics:
dat <- dat %>% mutate(anglo_saxon_share = 1 - non_domestic_share,
                      N_inv_nonwestern = non_western_share * N_inv_regiotech,
                      N_inv_non_domestic = non_domestic_share * N_inv_regiotech,
                      N_inv_anglosaxon = N_inv_regiotech - N_inv_non_domestic,
                      N_inv_China = China_share * N_inv_regiotech,
                      N_inv_India = India_share * N_inv_regiotech,
                      N_inv_ChinaIndia = N_inv_China + N_inv_India) %>%
  rename(imputed_N_inv_nonwestern = imputed_N_foreign_inv)

# convert shares to percentage numbers
dat[, grepl("share", names(dat))] <-dat[, grepl("share", names(dat))] * 100

# Save the complete dataset for regression analysis
write.csv(dat, paste0(getwd(), "/Data/regression_data/regression_data.csv"), row.names = FALSE)
print("Saved dataset as 'regression_data.csv")


