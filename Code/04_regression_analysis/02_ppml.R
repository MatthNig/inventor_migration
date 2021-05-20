##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 11.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse") # data processing
library("fixest") # quasipoisson regression with fixed effects

# directories  -----------------------------------------------------------------
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

###################################
####### Load & process data #######
###################################

load_dat_fun <- function(subsidiaries){
  if(subsidiaries == "include"){
    tmp <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_incl_subsidiaries.csv"))
    }else{
      if(subsidiaries == "exclude"){
        tmp <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_excl_subsidiaries.csv"))
        }else{
      tmp <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_only_subsidiaries.csv"))
        }
    }
  return(tmp)
  }

#panel_dat <- load_dat_fun(subsidiaries = "include")
#panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))

panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"))
panel_dat <- panel_dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       TimePeriod > 1984)
panel_dat$TimePeriod <- as.character(panel_dat$TimePeriod)

# only use observations with at least T_min observations
T_min <- 2
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]
paste("Panel dataset with", nrow(panel_dat), "observations ready for regression analysis.")

#################################
####### PPML ESTIMATIONS ########
#################################

print("Choose explanatory variable from: ")
c(names(panel_dat)[grepl(c("share"), names(panel_dat))], names(panel_dat)[grepl(c("N_inv"), names(panel_dat))])

print("Choose weight variable from: ")
c(names(panel_dat)[grepl("weight", names(panel_dat))])

#### (1) BASELINE SPECIFICATION ----------------------------------------------------- 
DEP_VAR <- "onshored_patents_worldclass"

WEIGHT_VAR <- "weight_initial_patents"
EXPL_VAR <- "N_inv_nonwestern"
panel_dat$N_inv_rest <- panel_dat$N_inv_regiotech - panel_dat[, EXPL_VAR]
CTRL_VAR <- c("N_inv_rest", "N_patents_TechGroup")
LAG_VARS <- unique(c(EXPL_VAR, CTRL_VAR))

lag_fun <- function(lag_vars, dep_var, df){
  tmp <- df %>% mutate(idx = rownames(df))
  tmp <- tmp %>% group_by(regio_tech) %>% arrange(TimePeriod) %>%
  mutate_at(c(LAG_VARS), ~dplyr::lag(., 0)) %>% arrange(as.numeric(idx))
  
  reg_dat <- df[, c("regio_tech", "TimePeriod", dep_var, lag_vars)]
  reg_dat[, lag_vars] <- tmp[, lag_vars]
  reg_dat <- reg_dat[complete.cases(reg_dat), ]
  reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
  
  return(reg_dat)
}

reg_dat <- lag_fun(lag_vars = LAG_VARS, df = panel_dat, dep_var = DEP_VAR)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                     by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
EXPL_VAR <- paste0("log(", EXPL_VAR, ")")
CTRL_VAR <- paste0("log(", CTRL_VAR, ")")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0(DEP_VAR, "~", 
                  EXPL_VAR, # foreign origin
                  " + ", CTRL_VAR, # total inventors and patenting controls
                  "|",
                  "regio_tech", # state-technology fixed effects
                  # "+ regio_inv[[trend]]", # state trends
                  # "+ TechGroup[[trend]]", # technology trends
                  "")
FORMULA <- as.formula(FORMULA)
FORMULA

ppml_model <- feglm(data = reg_dat, fml = FORMULA, family = "quasipoisson")
print("PPML model without weights")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs

#### (2) Weighted BY INITIAL PATENTNING ACTIVITY : ---------------------------------
ppml_model <- feglm(data = reg_dat, fml = FORMULA, 
                    weights = reg_dat[, WEIGHT_VAR], family = "quasipoisson")
print("PPML model with weights")
summary(ppml_model, cluster = c("regio_tech")) # clustered on tech-state pairs and time period

#### (3) REDUCED FORM WITH INSTRUMENT: -----------------------------------------

# inspect correlation of imputed number of foreign origin inventors
cor(panel_dat$N_inv_nonwestern, panel_dat$imputed_N_inv_nonwestern, use = "complete.obs")
ggplot(panel_dat, aes(x = log(N_inv_nonwestern), y = log(imputed_N_inv_nonwestern)))+
        geom_point(alpha = 0.3) +
        geom_abline(intercept = 0, slope = 1, color = "blue") +
        labs(x = "Number of Non-Western Inventors (log-scale)", 
             y = "Imputed Number of Non-Western Inventors (log-scale)")+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# inspect correlation of imputed shares
cor(panel_dat$non_western_share, panel_dat$non_western_share_imputed, use = "complete.obs")
ggplot(panel_dat, aes(x = non_western_share, y = non_western_share_imputed))+
        geom_point(alpha = 0.3) +
        geom_abline(intercept = 0, slope = 1, color = "blue") +
        labs(x = "Share of Non-Western Inventors (log-scale)",
             y = "Imputed Share of Non-Western Inventors (log-scale)")+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# choose formula
EXPL_VAR <- "imputed_N_inv_nonwestern"
CTRL_VAR <- c("N_inv_rest", "N_patents_TechGroup")
LAG_VARS <- unique(c(EXPL_VAR, CTRL_VAR))

reg_dat <- lag_fun(lag_vars = LAG_VARS, dep_var = DEP_VAR, df = panel_dat)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                 by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
EXPL_VAR <- paste0("log(", EXPL_VAR, ")")
CTRL_VAR <- paste0("log(", CTRL_VAR, ")")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0(DEP_VAR, "~", 
                  EXPL_VAR, " + ", # foreign origin
                  CTRL_VAR, # total inventors and patenting controls
                  "|", 
                  "regio_tech", # state-technology fixed effects
                  # "+ regio_inv[[trend]]", # state trends
                  # "+ TechGroup[[trend]]", # technology trends
                  "")
FORMULA <- as.formula(FORMULA)
FORMULA

# estimate the model without weights
ppml_model <- feglm(data = reg_dat, fml = FORMULA, family = "quasipoisson")
print("PPML model with imputed number of inventors without weights:")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

# estimate the model with weights
ppml_model <- feglm(data = reg_dat, fml = FORMULA, weights = reg_dat[, WEIGHT_VAR],
                    family = "quasipoisson")
print("PPML model with imputed number of inventors with weights:")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

#################################
####### OLS ESTIMATION ##########
#################################

# drop onshored patents with zero values or add small constant
ols_transform <- function(transformation = "drop"){
        if(transformation == "drop"){
                panel_dat_ols <- panel_dat[panel_dat$onshored_patents != 0, ]}else{
                  panel_dat_ols <- panel_dat      
                  panel_dat_ols$onshored_patents <- panel_dat_ols$onshored_patents + 0.1
                }
        return(panel_dat_ols)
}

panel_dat_ols <- ols_transform(transformation = "constant")

EXPL_VAR <- "N_inv_nonwestern"
panel_dat_ols$N_inv_rest <- panel_dat_ols$N_inv_regiotech - panel_dat_ols[, EXPL_VAR]
CTRL_VAR <- c("N_inv_rest", "N_patents_TechGroup")
LAG_VARS <- unique(c(EXPL_VAR, CTRL_VAR))

reg_dat <- lag_fun(lag_vars = LAG_VARS, df = panel_dat)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                     by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
EXPL_VAR <- paste0("log(", EXPL_VAR, ")")
CTRL_VAR <- paste0("log(", CTRL_VAR, ")")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0("log(onshored_patents) ~", 
                  EXPL_VAR, # foreign origin
                  " + ", CTRL_VAR, # total inventors and patenting controls
                  "|",
                  "regio_tech", # state-technology fixed effects
                  # "+ regio_inv[[trend]]", # state trends
                  # "+ TechGroup[[trend]]", # technology trends
                  "")
FORMULA <- as.formula(FORMULA)
FORMULA

ppml_model <- feols(data = reg_dat, fml = FORMULA)
print("OLS model without weights")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs
ppml_model <- feols(data = reg_dat, fml = FORMULA, weights = reg_dat[, WEIGHT_VAR])
print("OLS model with weights:")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

#################################
####### TSLS ESTIMATION #########
#################################

# drop onshored patents with zero values or add small constant
ols_transform <- function(transformation = "drop"){
        if(transformation == "drop"){
                panel_dat_ols <- panel_dat[panel_dat$onshored_patents != 0, ]}else{
                  panel_dat_ols <- panel_dat      
                  panel_dat_ols$onshored_patents <- panel_dat_ols$onshored_patents + 0.1
                }
        return(panel_dat_ols)
}

panel_dat_ols <- ols_transform(transformation = "constant")

ENDO_VARS <- "N_inv_nonwestern"
panel_dat_ols$N_inv_rest <- panel_dat_ols$N_inv_regiotech - panel_dat_ols[, ENDO_VARS]
EXOG_VAR <- c("N_inv_rest", "N_patents_TechGroup")
INSTRUMENTS <- "imputed_N_inv_nonwestern"

LAG_VARS <- unique(c(ENDO_VARS, EXOG_VAR, INSTRUMENTS))
reg_dat <- lag_fun(lag_vars = LAG_VARS, df = panel_dat_ols)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                     by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
ENDO_VARS <- paste0("log(", ENDO_VARS, ")")
EXOG_VAR <- paste0("log(", EXOG_VAR, ")")
INSTRUMENTS <- paste0("log(", INSTRUMENTS, ")")

LAGS <- "0"
ENDO_VARS <- paste0("l(", ENDO_VARS, ", ", LAGS, ")", collapse = " + ")
EXOG_VAR <- paste(paste0("l(", EXOG_VAR, ", ", LAGS, ")"), collapse = " + ")
INSTRUMENTS <- paste0("l(", INSTRUMENTS, ", ", LAGS, ")", collapse = " + ")

FORMULA <- paste0("log(onshored_patents) ~", 
                  EXOG_VAR, # exogenous variables
                  "|",
                  "regio_tech", # state-technology fixed effects
                  # "+ TechGroup[[trend]]", # technology trends
                  "|",
                  ENDO_VARS, "~", INSTRUMENTS,
                  "")
FORMULA <- as.formula(FORMULA)
FORMULA

ppml_model <- feols(data = reg_dat, fml = FORMULA)
print("TSLS model without weights")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs
ppml_model <- feols(data = reg_dat, fml = FORMULA, weights = reg_dat[, WEIGHT_VAR])
print("TSLS model with weights:")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

# first stage
summary(ppml_model, stage=1)