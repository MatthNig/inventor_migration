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
library("viridis")
library("gravity")


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

#### functions
source(paste0(getwd(), "/Code/onshoring_analysis/onshoring_analysis_functions.R"))

##############################################################################
####### Aggregate at the level of U.S. states and technological groups #######
##############################################################################

#### on-/off-shoring of patents ------------------------------------------------
df <- df %>% select(-country_firm) %>% rename(country_firm = country_firm_adj)

# (1) identify all patents that have been offshored to the U.S. by foreign firms
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)

# (2) Define technological groups and assign patents to them:
# These groups are derived from Schmoch and Hall.
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

# (3) Define 5-year "TimePeriod" and assign patents to them:
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
# N = 13049201 patents used for analysis

# (4) calculate the number of offshored patents to the U.S. by TechGoup, TimePeriod and Region:
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, TimePeriod)]
        
        return(onshored_pat)
}

dat <- onshoring_TechState(df = dat)

# -> THIS RETURNS WAY TO MANY NA's... 
# -> TAKE A LOOK AT THE DATASET HERE. "REGIO_INV" IS MISSING FOR MANY US PATENTS

print("Data on onshored patents prepared.")

#### foreign origin inventor shares  ------------------------------------------------

# (1) assign technological groups and TimePeriods to inventors
inv_dat <- assign_TechGroup(df = inv_dat)
inv_dat <- assign_TimePeriod(df = inv_dat)

# (2) calculate foreign origin shares by Region, TechGroup and Year
foreign_share_TechState <- function(df, country, min_inv){
        
        # calculate total number of inventors per region and TechGroup-field
        total_inventors <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, TimePeriod) %>%
                summarise(total_inventors = n())
        
        # sum up domestic origin probabilities per region and TechGroup-field
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(Up_reg_label, TechGroup, TimePeriod) %>%
                summarise(share = sum(prob_AngloSaxon))

        # calculate share of foreign origin per region and tech-field
        tmp <- merge(tmp, total_inventors, 
                     by = c("Up_reg_label", "TechGroup", "TimePeriod"))
        tmp <- filter(tmp, total_inventors >= min_inv)
        tmp$share = 1 - (tmp$share / tmp$total_inventors)
        tmp <- tmp %>% rename(foreign_share = share,
                              regio_inv = Up_reg_label) %>% 
                select(-total_inventors)
        
        return(tmp)
}

tmp <- foreign_share_TechState(df = inv_dat, country = "US",
                                min_inv = 10)

print("Data on inventor origin shares prepared.")


#### Create the dataset for regression analysis --------------------

# (1) merge onshoring and inventor origin information
dat <- merge(dat, tmp, 
             by = c("regio_inv", "TechGroup", "TimePeriod"), 
             all = TRUE)
tmp <- NULL
print("Merged the number of offshored patents and inventor origin information")

# (2) clean the dataset
dat <- dat[is.na(dat$TimePeriod) == FALSE & 
                   is.na(dat$regio_inv) == FALSE &
                   is.na(dat$TechGroup) == FALSE, ]
dat <- mutate(dat, onshored_patents = ifelse(is.na(onshored_patents), 0, onshored_patents))

# (3) assign "regio_tech" dummies
REGIONS <- read.csv(paste0(getwd(), "/Data/patent_data/US_state_labels.csv"))
names(REGIONS) <- c("regio_inv", "reg_label")
dat <- merge(dat, REGIONS, by = "regio_inv")
TECH_GROUP_No <- dat %>% distinct(TechGroup) %>% mutate(TechGroup_No = paste0("0", rownames(.)))
dat <- merge(dat, TECH_GROUP_No, by = "TechGroup")
dat$regio_tech <- paste0(dat$reg_label, dat$TechGroup_No)

print("Created dataset for regression analysis.")

####################################
####### Descriptive analysis #######
####################################

# (1) calculate percentage change in onshored patents between 1990 and 2010
tmp <- dat %>% filter(TimePeriod %in% c(1990, 2010)) %>% 
        filter(onshored_patents > 5) %>%
        group_by(regio_tech) %>% 
        arrange(TimePeriod) %>%
        mutate(offshoring_change = 100 * (onshored_patents - dplyr::lag(onshored_patents)) / dplyr::lag(onshored_patents))

# (2) calculate percentage change in foreign share between 1990 and 2010
tmp <- tmp %>% group_by(regio_tech) %>% 
        arrange(TimePeriod) %>%
        mutate(foreign_change = 100 * (foreign_share - dplyr::lag(foreign_share))) %>%
        filter(TimePeriod == 2010)

# (3) plot the relationship
ggplot(tmp, aes(x = offshoring_change, y = foreign_change))+
        geom_hline(yintercept = 0, #mean(tmp$foreign_change, na.rm = TRUE), 
                   linetype = "dotted", color = "red")+ 
        scale_color_viridis(option = "inferno", discrete = TRUE)+ guides(size = FALSE)+
        geom_point(aes(color = TechGroup, size = onshored_patents))+
        labs(title = "Offshoring and inventor diversity", 
             subtitle = "(1990 to 2010)", 
             x = "Growth in Offshoring \n (in percent)", y = "Change in foreign origin inventor share \n (percentage points)")+
        theme(legend.position = "bottom", panel.background = element_blank(),
              axis.line = element_line())

# might be a bit misleading because pharma was already high in 1990 and didnt change much more

####################################
####### Regression analysis ########
####################################

### FIXED EFFECTS ESTIMATOR -------------------------

# (1) construct panel data format
plm_dat <- pdata.frame(dat, index = c("regio_tech", "TimePeriod"))
plm_dat$TimePeriod <- as.character(plm_dat$TimePeriod)
plm_dat$foreign_share <- plm_dat$foreign_share * 100

# Optional 1: only keep states with a minimum number of TechGroups or vice versa
keep_obs <- plm_dat %>% group_by(TimePeriod, regio_inv) %>%
        summarise(count = n()) %>% filter(count >= 4)
keep_obs <- unique(keep_obs$regio_inv)
plm_dat <- filter(plm_dat, regio_inv %in% keep_obs)
plm_dat <- pdata.frame(plm_dat, index = c("regio_tech", "TimePeriod"))
plm_dat$TimePeriod <- as.character(plm_dat$TimePeriod)

# Optional 2: create weights based on overall patents in 1980-1985
weights <- df %>% filter(ctry_inv == "US", p_year %in% seq(1980, 1985)) %>% distinct(p_key, .keep_all = TRUE)
weights <- merge(weights, REGIONS, by = "regio_inv")
weights <- assign_TechGroup(weights)
weights <- merge(weights, TECH_GROUP_No, by = "TechGroup")
weights$regio_tech <- paste0(weights$reg_label, weights$TechGroup_No)
weights <- weights %>% group_by(regio_tech) %>% 
        summarise(N_patents = n(),
                  weight = N_patents / nrow(weights)) %>%
        select(-N_patents)

plm_dat <- merge(plm_dat, weights, by = "regio_tech", all.x = TRUE)
plm_dat <- pdata.frame(plm_dat, index = c("regio_tech", "TimePeriod"))

# for lag = 1 problems: https://stackoverflow.com/questions/41623583/error-0-non-na-cases-plm-package
# tmp <- plm_dat %>% group_by(TechGroup, regio_inv) %>% summarize(count = n())
# plm_dat <- merge(plm_dat, tmp, by = c("TechGroup", "regio_inv"), all.x = TRUE)
# plm_dat <- plm_dat %>% filter(count > 2)
# plm_dat <- pdata.frame(plm_dat, index = c("regio_tech", "TimePeriod"))
# plm_dat <- plm_dat[complete.cases(plm_dat),]

# estimate fixed effects model (N = 1999 with 'pat_dat_all_final.rds')
plm_model <- plm(data = plm_dat,
                 formula = log(1 + onshored_patents) ~ foreign_share +
                         as.numeric(TimePeriod):as.character(TechGroup_No) +
                         as.numeric(TimePeriod):as.character(regio_inv), 
                 model = "within", effect = "twoways")
summary(plm_model, vcov = vcovHC)

# fixed effects model with weights and robust standard errors
# https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html
# => maybe demean manually and run the regression...
plm_model <- lm(data = plm_dat,
                 formula = log(1 + onshored_patents) ~ foreign_share +
                        as.numeric(TimePeriod):as.character(TechGroup_No) +
                        as.numeric(TimePeriod):as.character(regio_inv) +
                        regio_tech + TimePeriod -1, 
                weights = weight)
summary(plm_model)
lmtest::coeftest(plm_model,  vcov = vcovHC(plm_model, 
                                           cluster = plm_dat$regio_tech, "HC1")) # with weights
length(table(model.frame(plm_model)$foreign_share))
round(cor(plm_model$fitted.values, plm_model$model[,1])^2, 3)


### FIRST DIFFERENCE ESTIMATOR -------------------------

# (-> to do: manually create and fit with OLS)
plm_model <- plm(data = plm_dat,
                 formula = log(1 + onshored_patents) ~ foreign_share +
                         as.numeric(TimePeriod):as.character(TechGroup_No) +
                         as.numeric(TimePeriod):as.character(regio_inv) -1, 
                 model = "fd", effect = "individual")
summary(plm_model)
lmtest::coeftest(plm_model, vcov = vcovHC)



### PPML ---------------------------
dat_ppml <- dat
dat_ppml$foreign_share <- dat_ppml$foreign_share * 100
dat_ppml$trend <- as.numeric(dat_ppml$TimePeriod)
dat_ppml <- merge(dat_ppml, weights, by = "regio_tech", all.x = TRUE)# %>% 
        # filter(is.na(weight) == FALSE)

ppml_model <- ppml(dependent_variable = "onshored_patents", dist = "foreign_share",
                  additional_regressors = c("TimePeriod", "regio_tech", "trend"),
                  data = dat_ppml, vce_robust= TRUE)
summary(ppml_model)

ppml_model <- glm(onshored_patents ~ foreign_share + 
                          # trend:regio_inv + trend:TechGroup_No + #trends
                          TimePeriod + regio_tech -1, # fixed effects
                  family="quasipoisson",
                  data = dat_ppml, 
                  weights = weight)
summary(ppml_model)
lmtest::coeftest(ppml_model, vcov=sandwich::vcovHC(ppml_model, "HC1"))[1:5,]
length(table(model.frame(ppml_model)$foreign_share))
round(cor(ppml_model$fitted.values, ppml_model$y)^2, 3)


# ressources:
# http://www.cazaar.com/ta/econ113/interpreting-beta
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3444996

# PRLIMINARY RESULTS:
# seems to work when using the time-period sum!
# 1pp increase in foreign share, raises onshoring by around 0.5-0.7%
# Story: 20pp increase of foreign inventors in the U.S. (1980-2010) fostered onshoring of patents by around 10-15% 
# (total onshoring increase for the U.S. was ~ 200% i.e. 5-10% of total increase attributed to foreign origin inventors)
# (this does not include spillover effects)

# with weighted least squares results are more pronounced:
# 1pp increase in foreign share, raises onshoring by around 1-2%
# Story: 20pp increase of foreign inventors in the U.S. (1980-2010) fostered onshoring of patents by around 20-40%
# (total onshoring increase for the U.S. was ~ 200% i.e. 20-40% of total increase (~ between 1/10 and 1/5)
# attributed to foreign origin inventors)
# (this does not include spillover effects)



# IDEAS:
# (maybe add U.S. regions from U.S. Bureau instead of state-time trend.)
# (make more and different tech groups that are still large enough)
# create weight by 1980 patent counts 

# https://stackoverflow.com/questions/44939997/ppml-package-gravity-with-time-fixed-effects
# https://stackoverflow.com/questions/41623583/error-0-non-na-cases-plm-package
