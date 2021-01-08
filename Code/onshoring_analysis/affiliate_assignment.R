####################################################################
# Description:    Script to assign parent company's country to US  #
#                 affiliates.                                      #
# Authors:        Matthias Niggli/CIEB UniBasel                    #
# Date:           08.01.2021                                       #
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

#### U.S. AFFILIATES OF NON-U.S. COMPANIES
us_affiliates <- read.csv2(file = paste0(getwd(), "/Data/patent_data/US_affiliates_encoded.csv"),
                           header = TRUE, row.names = 1)
us_affiliates <- filter(us_affiliates, keep == 1) %>% select(-keep)
us_affiliates$organization <- gsub("\xa0", " ", us_affiliates$organization)

#### PATENT DATA
df <- readRDS(paste0(datDir, "/pat_dat_all.rds"))
us_applicants <-setDT(df)[country_firm == "US", ][, country_firm_adj := country_firm]
us_applicants$idx <- rownames(us_applicants)

#############################################################################
####### Search for additional Fortune500 companies in U.S. applicants #######
#############################################################################
fortune500_companies <- read.table(file = paste0(getwd(), "/Data/patent_data/manual_firm_search.txt"),
                                   header = FALSE)$V1
fortune500_companies <- trimws(tolower(fortune500_companies))
tmp <- us_applicants %>% distinct(organization)

#############################################################################
####### Adjust country of origin for all U.S. based affiliates ##############
#############################################################################

for(i in 1:nrow(us_affiliates)){
        
        affiliate <- us_affiliates$organization[i]
        us_applicants[organization == affiliate, ]$country_firm_adj <- us_affiliates[i, ]$country_firm_adj
        }

test <- us_applicants %>% filter(grepl("bmw", organization) == TRUE) %>% distinct(organization)

