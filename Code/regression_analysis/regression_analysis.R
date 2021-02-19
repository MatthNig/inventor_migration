##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 15.02.2020                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse") # data processing
library("fixest") # poisson regression with fixed effects
library("pglm") # panel data and glm models

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
        trend = ((TimePeriod-1985)/3)+1,
        TimePeriod = as.character(TimePeriod)
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
LAGS <- "1"
FORMULA <- paste0("onshored_patents ~ l(", EXPL_VAR, ", ", LAGS, ") |", # foreign origin
                  "regio_inv[[trend]] +", # state trends
                  "TechGroup[[trend]] +", # technology trends
                  "regio_tech + TimePeriod") # state-technology and period fixed effects
FORMULA <- as.formula(FORMULA)

ppml_model <- feglm(data = dat_panel, fml = FORMULA, family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech", "TimePeriod")) # se clustered on tech-state pairs and time periods

# Problem: IV only implemented for OLS
# https://stackoverflow.com/questions/65867315/error-in-the-fixest-package-in-r-when-running-feglm-with-instruments
# ALternative:
#  https://cran.r-project.org/web/packages/glmmML/index.html
# https://github.com/amrei-stammann/alpaca

# Optional: Use weights:
print("Choose weight variable from: ")
c(names(dat)[grepl("weight", names(dat))])
WEIGHT_VAR <- "weight_overall_patents"
ppml_model <- feglm(data = dat_panel, fml = FORMULA, weights = dat_panel[, WEIGHT_VAR], family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech", "TimePeriod")) # clustered on tech-state pairs and time period

# PRLIMINARY RESULTS: ---------------------------------------------------------
# 1pp increase in foreign share, raises onshoring by around 1.5%
# 20pp increase of foreign inventors in the U.S. (1980-2015) fostered onshoring of patents by around 30%
# Total onshoring increase for the U.S. was ~ 200% i.e. 1/6 of total increase attributed to foreign origin inventors

# Reduced form with instrument
iv_dat <- dat_panel[!dat_panel$TimePeriod %in% c("1978", "1982")]
EXPL_VAR <- "non_western_share_imputed"
LAGS <- "1"
FORMULA <- paste0("onshored_patents ~ l(", EXPL_VAR, ", ", LAGS, ") |", # foreign origin
                  "regio_inv[[trend]] +", # state trends
                  "TechGroup[[trend]] +", # technology trends
                  "regio_tech + TimePeriod") # state-technology and period fixed effects
FORMULA <- as.formula(FORMULA)
ppml_model <- feglm(data = iv_dat, fml = FORMULA, family = "quasipoisson", weights = iv_dat[, WEIGHT_VAR])
summary(ppml_model, cluster = c("regio_tech", "TimePeriod")) # se clustered on tech-state pairs and time periods

