##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 11.02.2020                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse") # data processing
library("fixest") # poisson regression with fixed effects

# directories  -----------------------------------------------------------------
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################
####### Load data #######
#########################

dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
dat[, grepl("share", names(dat))] <-dat[, grepl("share", names(dat))] * 100
dat <- dat %>% mutate(
        TimePeriod = as.character(TimePeriod),
        trend = case_when(TimePeriod=="1980"~1, TimePeriod=="1985"~2,
                                        TimePeriod=="1990"~3,TimePeriod=="1995"~4,
                                        TimePeriod=="2000"~5,TimePeriod=="2005"~6,
                                        TimePeriod=="2010"~7)
        )
dat_panel <- panel(data = dat, panel.id = c("regio_tech", "TimePeriod"))

#########################################
####### PPML regression analysis ########
#########################################

# Resources: 
# https://cran.r-project.org/web/packages/fixest/index.html
# http://www.cazaar.com/ta/econ113/interpreting-beta
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3444996

## BASELINE SPECIFICATION ----------------------------------------------------- 
print("Choose explanatory variable from: ")
names(dat)[grepl("share", names(dat))]
EXPL_VAR <- "non_western_share"
LAGS <- "0"
FORMULA <- paste0("onshored_patents ~ l(", EXPL_VAR, ",", LAGS, # foreign origin
                  ") | regio_inv[[trend]] + TechGroup[[trend]] + # state and technology trends
                  regio_tech + TimePeriod") # state-technology and period fixed effects
FORMULA <- as.formula(FORMULA)

ppml_model <- fepois(data = dat_panel, fml = FORMULA)
summary(ppml_model, cluster = c("regio_tech", "TimePeriod")) # se clustered on tech-state pairs and time periods

# Optional: Use weights:
print("Choose weight variable from: ")
c(names(dat)[grepl("weight", names(dat))])
WEIGHT_VAR <- "weight_state_overall_patents"
ppml_model <- fepois(data = dat_panel, fml = FORMULA, weights = dat_plm[, WEIGHT_VAR])
summary(ppml_model, cluster = c("regio_tech", "TimePeriod")) # clustered on tech-state pairs and time period

# PRLIMINARY RESULTS: ---------------------------------------------------------
# 1pp increase in foreign share, raises onshoring by around 2%
# 20pp increase of foreign inventors in the U.S. (1980-2015) fostered onshoring of patents by around 40%
# Total onshoring increase for the U.S. was ~ 200% i.e. 1/5 of total increase attributed to foreign origin inventors
