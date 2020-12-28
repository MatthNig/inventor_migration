##################################################################
# Description:  Script to generate a dataset of onshored patents #
#               and inventor flows for regression analysis. The  #
#               data is at the level of technological fields and #
#               U.S. states per year.                            #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 26.11.2020                                       #
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
mainDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
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
# inv_dat <- readRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds")) # random subset of inventors
inv_dat <- readRDS(paste0(mainDir, "/created data/inventor_origin.rds")) # full sample

#### functions
source(paste0(getwd(), "/Code/onshoring_analysis/onshoring_analysis_functions.R"))

##############################################################################
####### Aggregate at the level of U.S. states and technological groups #######
##############################################################################

#### on-/off-shoring of patents ------------------------------------------------
# (1) identify offshored patents to the U.S.
dat <- country_onshoring(df = df, onshoring_country = "US",
                         inventor_number = 1, world_class_indicator = FALSE)

# (2) define & assign technological groups to patents:
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
dat <- assign_TechGroup(dat)

# (2) calculate the number of offshored patents to the U.S. per TechGoup and region
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
                                min_inv = 30)

#### merge offshored patents with foreign origin information --------------------

dat <- merge(dat, tmp, 
             by = c("regio_inv", "TechGroup", "p_year"), 
             all = TRUE)
tmp <- NULL
print("Merged the number of offshored patents and inventor origin information")

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

dat <- dat[is.na(dat$TimePeriod) == FALSE & 
                   is.na(dat$regio_inv) == FALSE &
                   is.na(dat$TechGroup) == FALSE, ]

# this is critical... should first verify if I can actually do this.
dat <- mutate(dat,
              onshored_patents = ifelse(is.na(onshored_patents), 0, onshored_patents),
              foreign_share = ifelse(is.na(foreign_share), 0, foreign_share))

dat <- dat %>% group_by(TimePeriod, TechGroup, regio_inv) %>% 
        summarise(onshored_patents = mean(onshored_patents, na.rm = TRUE),
                  foreign_share = mean(foreign_share, na.rm = TRUE)) %>%
        filter(is.nan(foreign_share) == FALSE) %>%
        mutate(onshored_patents = ifelse(is.nan(onshored_patents) == TRUE, 0, onshored_patents))

REGIONS <- read.csv(paste0(getwd(), "/Data/patent_data/US_state_labels.csv"))
names(REGIONS) <- c("regio_inv", "reg_label")
dat <- merge(dat, REGIONS, by = "regio_inv")
TECH_GROUP_No <- dat %>% distinct(TechGroup) %>% mutate(TechGroup_No = paste0("0", rownames(.)))
dat <- merge(dat, TECH_GROUP_No, by = "TechGroup")
dat$regio_tech <- paste0(dat$reg_label, dat$TechGroup_No)

REGIONS <- NULL
TECH_GROUP_No <- NULL
print("Created dataset for analysis.")

####################################
####### Descriptive analysis #######
####################################

# show a correlation plot with y = change of onshored patents and x = change of foreign origin
# problem: log-difference is a bad approximation for percentage change when differences are large.
# therefore: normalize somehow e.g. scale by mean and sd of overall onshored patents.

# (1) calculate percentage change in offshored patents between 1990 and 2010
tmp <- dat %>% filter(TimePeriod %in% c(1995, 2010)) %>% 
        # filter(onshored_patents > 0) %>%
        mutate(onshored_patents = scale(onshored_patents)) %>%
        group_by(regio_tech) %>% 
        arrange(TimePeriod) %>%
        mutate(offshoring_change = onshored_patents - dplyr::lag(onshored_patents))


# (2) calculate percentage change in foriegn share between 1990 and 2010
tmp <- tmp %>% group_by(regio_tech) %>% 
        arrange(TimePeriod) %>%
        mutate(foreign_change = (foreign_share - dplyr::lag(foreign_share)))

tmp <- tmp[complete.cases(tmp), ]

# plot
ggplot(tmp, aes(x = foreign_change, y = offshoring_change))+
        geom_vline(xintercept = 0, linetype = "dotted")+
        geom_point(aes(color = TechGroup, size = onshored_patents))+
        geom_smooth(method = "lm")

####################################
####### Regression analysis ########
####################################

tmp <- pdata.frame(tmp, index = c("regio_tech", "TimePeriod"))
tmp$TimePeriod <- as.character(tmp$TimePeriod)

# only keep states with a minimum number of TechGroups or vice versa
keep_obs <- tmp %>% group_by(TimePeriod, regio_inv) %>%
        summarise(count = n()) %>% filter(count >= 5)
keep_obs <- unique(keep_obs$regio_inv)
plm_dat <- filter(tmp, regio_inv %in% keep_obs)
plm_dat <- pdata.frame(plm_dat, index = c("regio_tech", "TimePeriod"))
plm_dat$TimePeriod <- as.character(plm_dat$TimePeriod)


# fixes effects within transformation
plm_model <- plm(data = tmp,
                 # data = plm_dat,
                 formula = log(1 + onshored_patents) ~ foreign_share +
                         as.numeric(TimePeriod):as.character(TechGroup) +
                         as.numeric(TimePeriod):as.character(regio_inv),
                 model = "within", effect = "individual")
summary(plm_model)

# first difference fixed effects model
plm_model <- plm(data = tmp,
                 # data = plm_dat,
                 formula = log(1 + onshored_patents) ~ foreign_share -1 +
                         as.numeric(TimePeriod):as.character(TechGroup) +
                         as.numeric(TimePeriod):as.character(regio_inv),
                 model = "fd", effect = "individual")
summary(plm_model)


# maybe add U.S. regions from U.S. Bureau instead of state-time trend.
# make more and different tech groups that are still large enough
# create weight by 1980 patent counts 
