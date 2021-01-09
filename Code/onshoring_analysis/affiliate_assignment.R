####################################################################
# Description:    Script to assign parent company's country to US  #
#                 affiliates.                                      #
# Authors:        Matthias Niggli/CIEB UniBasel                    #
# Date:           09.01.2021                                       #
####################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

# directories  -----------------------------------------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################
####### Load data #######
#########################

#### U.S. AFFILIATES OF NON-U.S. COMPANIES -------------------------------------
us_affiliates <- read.csv2(file = paste0(getwd(), "/Data/patent_data/US_affiliates_encoded.csv"),
                           header = TRUE, row.names = 1)
us_affiliates <- filter(us_affiliates, keep == 1) %>% select(-keep)
us_affiliates$organization <- gsub("\xa0", " ", us_affiliates$organization)
us_affiliates$classified <- NULL

#### LOAD FORTUNE500 COMPANIES -------------------------------------------------
fortune500 <- read.csv(file = paste0(getwd(), "/Data/patent_data/fortune500_affiliates.csv"), 
                       header = FALSE, sep = ";")
names(fortune500) <- names(us_affiliates)
fortune500$organization <- gsub("\xa0", " ", fortune500$organization)

#### COMBINE AND EVALUATE ------------------------------------------------------
us_affiliates <- rbind(us_affiliates, fortune500)
fortune500 <- NULL
us_affiliates <- us_affiliates[!duplicated(us_affiliates$organization), ]
print("Countries with the most U.S. affiliates:")
us_affiliates %>% group_by(country_firm_adj) %>% summarise(count = n()) %>% arrange(-count)

#### PATENT DATA ---------------------------------------------------------------
df <- readRDS(paste0(datDir, "/pat_dat_all.rds"))
df <- setDT(df)[, country_firm_adj := country_firm] # init adjusted country of the company
head(df)
print("Data ready for re-assignment of U.S. based branches.")

##############################################
####### Affiliate re-assignment ##############
##############################################

# Task: ------------------------------------------------------------------------
# for patents of every U.S. affiliate of a non-U.S. company (= stored in 'affiliates'),
# change 'country_firm_adjust' from 'US' to the company of the parent.
# (e.g. for a patent of 'hoffmann la roche', the 'country_firm_adj' is changed from 'US' to 'CH')

# subset to U.S. based companies in order to speed up computation: -------------
df <- df[, idx := rownames(df)] # keep rownames to merge again
us_applicants <- df[country_firm == "US", ]
us_applicants <- us_applicants %>% select(organization, country_firm, idx)

# add parent's country of origin to U.S. affiliates and subset -----------------
us_applicants <- merge(us_applicants, us_affiliates, by = "organization", all.x = TRUE)
us_applicants <- filter(us_applicants, is.na(country_firm_adj) == FALSE)
head(us_applicants, n = 20)
paste(nrow(us_applicants), "patents registered by U.S. branches of foreign companies havee been reassiged.")

# change the updated samples in the original data.frame ------------------------
setDF(df)
df[us_applicants$idx, ]$country_firm_adj <- us_applicants$country_firm_adj
df$idx <- NULL

# TEST if everything worked:  --------------------------------------------------
N_reassigned <- nrow(setDT(df)[country_firm != country_firm_adj, ])
if(N_reassigned == nrow(us_applicants)){
        print("Reassigning the country of U.S. branches to their parent companies' successfull.")}else{
                print("Reassigning not succesful")
        }

##############################################
####### Save the adjusted data.frame #########
##############################################

df %>% saveRDS(paste0(datDir, "/pat_dat_all_final.rds"))
print("Saved adjusted dataset as 'pat_dat_all_final.csv'")




