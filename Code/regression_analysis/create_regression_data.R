##################################################################
# Description:  Script to generate a dataset of onshored patents #
#               and inventor flows for regression analysis. The  #
#               data is at the level of technological fields and #
#               U.S. states per year.                            #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 11.02.2020                                       #
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

#### functions to identify offshored patents
source(paste0(getwd(), "/Code/onshoring_analysis/onshoring_analysis_functions.R"))

################################################################################
##### Aggregate data at the level of U.S. states and technological groups ######
################################################################################

#### on-/off-shoring of patents ------------------------------------------------

# (1) assign U.S. affiliates of foreign companies to their headquarter country:
df <- df %>% select(-country_firm) %>% rename(country_firm = country_firm_adj)

# (2) identify all patents that have been offshored to the U.S. by foreign firms:
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)

# (3) Define technological groups and assign patents to them:
assign_TechGroup <- function(df){
        
        df <- mutate(df, TechGroup = case_when(
                tech_field %in% c(1:2) ~ "Electrical & Audio-Visual Technologies",
                tech_field %in% c(3:5, 7) ~ "Information & Communication Technology",
                tech_field %in% c(6, 8) ~ "Computer Science & Semiconducturs",
                tech_field %in% c(9:10, 12) ~ "Instruments",
                tech_field %in% c(11, 13) ~ "Med-Tech",
                tech_field %in% c(14:16) ~ "Pharmaceuticals & Biotechnology",
                tech_field %in% c(18:23) ~ "Chemistry & Materials",
                tech_field %in% c(24:32) ~ "Machinery, Transportation & Mechanical Engineering",
                tech_field %in% c(33:35) ~ "Consumer Goods & Civil Engineering"
                )
        )
}

dat <- assign_TechGroup(df = dat)

# Note: selection of groups is motivated from Schmoch (2008):
# http://www.world-intellectual-property-organization.com/edocs/mdocs/classifications/en/ipc_ce_41/ipc_ce_41_5-annex1.pdf
# and Hall (2001):
# https://www.nber.org/system/files/working_papers/w8498/w8498.pdf

# (4) Define 5-year periods and assign patents to them:
assign_TimePeriod <- function(df){
        
        df <- mutate(df, TimePeriod = case_when(
                p_year %in% c(1978:1982) ~ "1980",
                p_year %in% c(1983:1987) ~ "1985",
                p_year %in% c(1988:1992) ~ "1990",
                p_year %in% c(1993:1997) ~ "1995",
                p_year %in% c(1998:2002) ~ "2000",
                p_year %in% c(2003:2007) ~ "2005",
                p_year %in% c(2008:2012) ~ "2010")
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

#### foreign origin inventor shares  ------------------------------------------------

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

get_foreign_shares <- function(df, type){
        if(type == "non-Western"){
                # Option A: calculate shares for non-Western ethnic origins
                NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                                        "Arabic", "Persian", "Turkey")
                tmp <- foreign_share_TechState(df = df, country = "US", min_inv = 30,
                                               origins = NON_WESTERN_ORIGIN)}else{
                                                       # Option B: calculate shares for all non-domestic ethnic origin:
                                                       tmp <- foreign_share_TechState(df = df, country = "US", 
                                                                                      min_inv = 30,
                                                                                      origins = "AngloSaxon")
                                                       tmp$foreign_share <- 1 - tmp$foreign_share}
        tmp$type <- type
        return(tmp)
        }

tmp <- get_foreign_shares(df = inv_dat, type = "non-Western")
# DO BOTH HERE...:
tmp <- tmp %>% rename(non_western_share = foreign_share) %>% select()

print("Data on inventor origin shares prepared.")


#### Create the dataset for regression analysis --------------------

# (1) merge onshoring and inventor origin information
dat <- merge(dat, tmp, 
             by = c("regio_inv", "TechGroup", "TimePeriod"), 
             all = TRUE)
tmp <- NULL
print("Merged the number of offshored patents and inventor origin information")

# (2) clean the dataset
dat <- dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
dat <- mutate(dat, onshored_patents = ifelse(is.na(onshored_patents), 0, onshored_patents))

# (3) assign "regio_tech" dummies
REGIONS <- read.csv(paste0(getwd(), "/Data/patent_data/US_state_labels.csv"))
names(REGIONS) <- c("regio_inv", "reg_label")
dat <- merge(dat, REGIONS, by = "regio_inv")
TECH_GROUP_No <- dat %>% distinct(TechGroup) %>% mutate(TechGroup_No = paste0("0", rownames(.)))
dat <- merge(dat, TECH_GROUP_No, by = "TechGroup")
dat$regio_tech <- paste0(dat$reg_label, dat$TechGroup_No)

#### create weights for technology-state pairs overall patenting shares:
weights <- df %>% filter(ctry_inv == "US") %>% #, p_year %in% seq(1978, 1990)) %>%
        distinct(p_key, .keep_all = TRUE)
weights <- merge(weights, REGIONS, by = "regio_inv")
weights <- assign_TechGroup(weights)
weights <- merge(weights, TECH_GROUP_No, by = "TechGroup")
weights$regio_tech <- paste0(weights$reg_label, weights$TechGroup_No)
weights <- weights %>% group_by(regio_tech) %>% 
        summarise(N_patents = n(),
                  weight = N_patents / nrow(weights)) %>%
        select(-N_patents)
dat <- merge(dat, weights, by = "regio_tech", all.x = TRUE)
print("Added weights to technology-state pairs based on inital sample period.")

########################################################
####### Save the dataset for regression analysis #######
########################################################

write.csv(dat, paste0(getwd(), "/Data/regression_data/regression_data.csv"), row.names = FALSE)
print("Saved dataset as 'regression_data.csv")


