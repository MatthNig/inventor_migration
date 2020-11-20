##################################################################
# Description:  Script to generate a dataset of onshored patents #
#               and inventor flows for regression analysis. The  #
#               data is at the level of technological fields and #
#               U.S. states per year.                            #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 19.11.2020                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("plm")

# directories  -----------------------------------------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################################################
####### Load data and functions for data analysis #######
#########################################################

#### on-/off-shoring of patents
df <- readRDS(paste0(datDir, "/pat_dat_all.rds"))

#### inventor flows
inv_dat <- readRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds")) # random subset of inventors

#### functions
source(paste0(getwd(), "/Code/frontier_patents/patent_analysis_functions.R"))

##############################################################################
####### Aggregate at the level of U.S. states and technological groups #######
##############################################################################


#### on-/off-shoring of patents ------------------------------------------------
# (1) identify onshored patents for the U.S.
dat <- country_onshoring(df = df, onshoring_country = "US",
                         inventor_number = 1, world_class_indicator = FALSE)

# (2) define & assign technological groups to patents:
assign_TechGroup <- function(df){
        
        df <- mutate(df, TechGroup = case_when(
                tech_field %in% c(13:17) ~ "PharmaMedBioTech",
                tech_field %in% c(1:2, 9:10) ~ "ElectricVisualMeasurement",
                tech_field %in% c(3:8) ~ "TelcommIT",
                tech_field %in% c(19:21, 23) ~ "MaterialScience",
                tech_field %in% c(24:32) ~ "MachineryTransportationEnergy")
        )
}
dat <- assign_TechGroup(dat)

# (2) calculate the number of onshored patents per TechGoup and region
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, p_year)]
        
        return(onshored_pat)
}

dat <- onshoring_TechState(df = dat)

#### foreign origin shares  ------------------------------------------------

# (1) assign technological groups
inv_dat <- assign_TechGroup(inv_dat)

# (2) calculate foreign origin shares per region and TechGroup
foreign_share_TechState <- function(df, country, min_inv){
        
        # calculate total number of inventors per region and TechGroup-field
        total_inventors <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, p_year) %>%
                summarise(total_inventors = n())
        
        # sum up domestic origin probabilities per region and TechGroup-field
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, p_year) %>%
                summarise(share = sum(prob_AngloSaxon))

        # calculate share of domestic origin per region and tech-field
        tmp <- merge(tmp, total_inventors, 
                     by = c("Up_reg_label", "TechGroup", "p_year"))
        tmp <- filter(tmp, total_inventors >= min_inv)
        tmp$share = 1 - (tmp$share / tmp$total_inventors)
        tmp <- tmp %>% rename(foreign_share = share,
                              regio_inv = Up_reg_label) %>% 
                select(-total_inventors)
        
        return(tmp)
}

tmp <- foreign_share_TechState(df = inv_dat, country = "US",
                                min_inv = 10)

#### merge patent on-shoring with foreign origin information --------------------

dat <- merge(dat, tmp, 
             by = c("regio_inv", "TechGroup", "p_year"), 
             all = TRUE)
tmp <- NULL
print("Merged onshoring and inventor origin information")

#### create long period averages --------------------
assign_TimePeriod <- function(df){
        
        df <- mutate(df, TimePeriod = case_when(
                p_year %in% c(1979:1981) ~ "1980",
                p_year %in% c(1984:1986) ~ "1985",
                p_year %in% c(1989:1991) ~ "1990",
                p_year %in% c(1994:1996) ~ "1995",
                p_year %in% c(1999:2001) ~ "2000",
                p_year %in% c(2004:2006) ~ "2005",
                p_year %in% c(2009:2011) ~ "2010")
        )
}
dat <- assign_TimePeriod(dat)


tmp <- dat[is.na(dat$TimePeriod) == FALSE & 
                   is.na(dat$regio_inv) == FALSE &
                   is.na(dat$TechGroup) == FALSE, ]
tmp <- tmp %>% group_by(TimePeriod, TechGroup, regio_inv) %>% 
        summarise(onshored_patents = mean(onshored_patents, na.rm = TRUE),
                  foreign_share = mean(foreign_share, na.rm = TRUE)) %>%
        filter(is.nan(foreign_share) == FALSE) %>%
        mutate(onshored_patents = ifelse(is.nan(onshored_patents) == TRUE, 0, onshored_patents))
tmp$regio_tech <- paste0(tmp$regio_inv, tmp$TechGroup)

####################################
####### Descriptive analysis #######
####################################


####################################
####### Regression analysis ########
####################################

tmp <- pdata.frame(tmp, index = c("regio_tech", "TimePeriod"))
plm_model <- plm(data = tmp,
                 formula = log(1 + onshored_patents) ~ foreign_share + 
                         as.numeric(as.character(TimePeriod)):TechGroup,
                         # as.numeric(as.character(TimePeriod)):regio_inv, 
                 model = "within", effect = "individual")
summary(plm_model)

# maybe add U.S. regions from U.S. Bureau instead of state-time trend.
# make more and different tech groups that are still large enough
# create weight by 1980 patent counts 
