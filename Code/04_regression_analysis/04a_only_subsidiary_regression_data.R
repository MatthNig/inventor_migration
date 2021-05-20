##################################################################
# Description:  Script to generate a dataset of onshored patents #
#               from non-US subsidiaries and inventor flows      #
#               for regression analysis.                         #
#               The data is at the level of technological fields #
#               U.S. states for periods of 3 year intervals.     #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 30.05.2021                                       #
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

#### baseline regression data
reg_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))

##############################################################################################
## Patents offshored to US subsidiaries of non-US companies by technology area and State #####
##############################################################################################

# (1) identify all entries stated on patents by U.S. subsidiaries of non-US companies
tmp <- df %>% filter(country_firm == "US") %>% 
        mutate(subsidiary = case_when(
                country_firm != country_firm_adj ~ "yes",
                country_firm == country_firm_adj ~ "no"
        )) %>%
        filter(subsidiary == "yes")
paste("Identified", length(unique(tmp$p_key)), "unique patents from U.S. subsidiaries.")
# N = 69'133

# (2) identify those entries that are offshored to the U.S. (i.e. with a U.S. located inventor):
MIN_INV <- 1
onshoring_p_keys <- setDT(tmp)[ctry_inv == "US",
                               .(onshored = ifelse(.N >= MIN_INV, 1, 0)), 
                               by = .(p_key)]
setkey(onshoring_p_keys, p_key)
tmp <- onshoring_p_keys[setDT(tmp, key = "p_key")]
tmp[is.na(tmp$onshored) == TRUE, "onshored"] <- 0
onshoring_p_keys <- NULL

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

tmp <- assign_TechGroup(df = tmp)

# Note: selection of groups is motivated from Schmoch (2008):
# http://www.world-intellectual-property-organization.com/edocs/mdocs/classifications/en/ipc_ce_41/ipc_ce_41_5-annex1.pdf
# and Hall (2001):
# https://www.nber.org/system/files/working_papers/w8498/w8498.pdf

# (4) Define 3-year periods and assign patents to them:
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
tmp <- assign_TimePeriod(df = tmp)

#### evaluate the identified offshored patents
paste(nrow(tmp), "entries stated on patents from subsidiaries used for analysis") 
# N = 202'959
paste(nrow(tmp[tmp$p_year > 1984 & tmp$p_year < 2016, ]), 
      "entries of patents between 1985 and 2015 used for analysis") 
# N = 186'389
paste(nrow(tmp[tmp$onshored == 1 & tmp$ctry_inv == "US", ]),
      "entries stated on onshored patents to the U.S") 
# N = 161'488

# (5) calculate the number of unique offshored patents to the U.S. by TechGoup, Period and State:
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, TimePeriod)]
        
        return(onshored_pat)
}

patent_dat <- onshoring_TechState(df = tmp)

# (6) Summarize:
tmp <- patent_dat %>% group_by(regio_inv) %>% 
        summarize(onshored_patents = sum(onshored_patents)) %>%
        mutate(share = onshored_patents / sum(onshored_patents)) %>%
        arrange(-onshored_patents)
paste0(tmp[is.na(tmp$regio_inv), ]$onshored_patents, 
       " onshored patents (", round(tmp[is.na(tmp$regio_inv), ]$share,3)*100,
       "%) had missing technology field or state information.") 
# N = 30'893 (41.6%)
paste(sum(tmp[is.na(tmp$regio_inv) == FALSE, ]$onshored_patents), 
      "onshored patents assigned to technology-state pairs.") 
# N = 43'416

# (7) Discard NA's
patent_dat <- patent_dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
paste("Data on onshored patents prepared.", 
      sum(patent_dat$onshored_patents), 
      "onshored patents used for analysis") 
# N = 39'940

##################################################
####### Add to baseline regression dataset #######
##################################################

# (8) merge with reg_dat information
test <- names(reg_dat)
patent_dat <- rename(patent_dat, onshored_patents_subsidiaries_only = onshored_patents)
patent_dat <- merge(reg_dat, patent_dat, by = c("regio_inv", "TechGroup", "TimePeriod"), all.x = TRUE)
if(sum(test %in% names(patent_dat)) != length(test)){
  warning("Not all variables present in regressiond data")}else{
  print("Regression data ready.")}

# (9) assign 0 number of patents to regio_tech's where no onshored patents could be found
patent_dat <- mutate(patent_dat, 
                     onshored_patents_subsidiaries_only = ifelse(is.na(onshored_patents_subsidiaries_only),
                                                                 0, onshored_patents_subsidiaries_only)
                     )

########################################################
####### Save the dataset for regression analysis #######
########################################################

write.csv(patent_dat, paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"),
          row.names = FALSE)
print("Saved under dataset 'regression_data_robustness_checks.csv'")
