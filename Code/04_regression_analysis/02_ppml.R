##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 04.05.2021                                       #
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

panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
panel_dat <- panel_dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       TimePeriod > 1984)
panel_dat$TimePeriod <- as.character(panel_dat$TimePeriod)

# only use observations with at least T_min observations
T_min <- 3
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
c(names(dat)[grepl("weight", names(dat))])
WEIGHT_VAR <- "weight_initial_patents"

#### (1) BASELINE SPECIFICATION ----------------------------------------------------- 
EXPL_VAR <- "N_inv_nonwestern"
panel_dat$N_inv_rest <- panel_dat$N_inv_regiotech - panel_dat[, EXPL_VAR]
CTRL_VAR <- c("N_inv_rest", "N_patents_TechGroup")
LAG_VARS <- unique(c(EXPL_VAR, CTRL_VAR))

lag_fun <- function(lag_vars, df){
  tmp <- df %>% mutate(idx = rownames(df))
  tmp <- tmp %>% group_by(regio_tech) %>% arrange(TimePeriod) %>%
  mutate_at(c(LAG_VARS), ~dplyr::lag(., 1)) %>% arrange(as.numeric(idx))
  
  reg_dat <- df[, c("regio_tech", "TimePeriod", "onshored_patents", lag_vars)]
  reg_dat[, LAG_VARS] <- tmp[, lag_vars]
  reg_dat <- reg_dat[complete.cases(reg_dat), ]
  reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
  
  return(reg_dat)
}

reg_dat <- lag_fun(lag_vars = LAG_VARS, df = panel_dat)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                     by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
EXPL_VAR <- paste0("log(", EXPL_VAR, ")")
CTRL_VAR <- paste0("log(", CTRL_VAR, ")")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0("onshored_patents ~", 
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
r2(x = ppml_model, "pr2")
logLik(object = ppml_model)

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

reg_dat <- lag_fun(lag_vars = LAG_VARS, df = panel_dat)
reg_dat <- left_join(reg_dat, panel_dat[!duplicated(panel_dat$regio_tech), c("regio_tech", WEIGHT_VAR)], 
                 by = "regio_tech") # add weights
reg_dat <- panel(data = reg_dat, panel.id = c("regio_tech", "TimePeriod"))
EXPL_VAR <- paste0("log(", EXPL_VAR, ")")
CTRL_VAR <- paste0("log(", CTRL_VAR, ")")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0("onshored_patents ~", 
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

# (1) klappt nicht mit reduced form: weshalb nicht? diejenigen regio_techs mit sehr grossen imputed shares sind jene
# traditionellen technologien, die nicht sonderlich viele patente anziehen. das zieht dann den schnitt extrem runter

# (2) für imputed share am entscheidendsten ist die Grösse des States. NY, Pennsylvania etc. haben teils extrem hohe
# imputed shares! Da einfach deshalb, weil sie in initial period sehr hohe Anteil hatten (~15-20%). Wenn dann in späteren
# Perioden relativ viele foreign origin inventors kommen, dann ist der imputed share enorm hoch d.h. höher als
# tatsächliche Anzahl inventors. Das Gegenteil ist der Fall bei kleinen States.


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
                  panel_dat_ols$onshored_patents <- panel_dat_ols$onshored_patents + 1
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











#################################
####### PGMM ESTIMATIONS ########
#################################

# Overview:
# https://cran.r-project.org/web/views/Econometrics.html

library("plm")
library("sandwich")
library("lmtest")

panel_dat <- dat %>% select(regio_tech, TimePeriod, # identifiers
                            onshored_patents, # outcome
                            N_inv_nonwestern, imputed_N_inv_nonwestern, # explanatory variables
                            N_inv_anglosaxon, imputed_N_inv_anglosaxon, # control variables
                            weight_initial_patents, # weights
                            regio_inv, TechGroup # grouping variables
                            )
tmp <- data.frame(TimePeriod = sort(unique(panel_dat$TimePeriod)), 
                  trend = seq(1, length(unique(panel_dat$TimePeriod))))
panel_dat <- left_join(panel_dat, tmp, by = "TimePeriod") %>% 
        mutate(TimePeriod = as.numeric(TimePeriod))

#### convert all character variables to numeric identifiers (required by the pgmm()-function):
convert_to_num <- function(df, exp_var){
        
        # construct numeric identifiers
        identifiers <- unique(panel_dat[, exp_var])
        tmp <- data.frame(id_num = as.numeric(seq(length(identifiers))))
        tmp[, exp_var] <- identifiers
        
        # merge to original df & substitute character identifier
        df <- merge(df, tmp, by = exp_var)
        df <- df %>% select(-all_of(exp_var))
        names(df)[which(colnames(df) == "id_num")] <- exp_var
        
        # return updated data.frame
        return(df)
        }

# apply for all character variables in the data.frame
for(i in c("regio_tech", "regio_inv", "TechGroup")){
        panel_dat <- convert_to_num(df = panel_dat, exp_var = i)
}
panel_dat$onshored_patents <- as.numeric(panel_dat$onshored_patents)
#panel_dat <- panel_dat %>% na.omit()

# check the number of observations per regio_tech. 
# At least three are needed for first difference transformation
N_old <- nrow(panel_dat)
keep_idx <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarize(count = n()) %>% 
        filter(count > 4)
keep_idx <- keep_idx$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_idx, ]
paste("Dropped", N_old - nrow(panel_dat), "observations from the sample.")

# order data.frame columns for pgmm() to recognize identifiers:
id_col <- which(names(panel_dat) == "regio_tech")
time_col <- which(names(panel_dat) == "TimePeriod")
remain_cols <- which(!seq(ncol(panel_dat))%in% c(id_col, time_col))
panel_dat <- panel_dat[, c(id_col, time_col, remain_cols)]
paste(nrow(panel_dat), "observations in the sample. Ready for estimation")

# model
pgmm_model <- pgmm(formula = onshored_patents ~ lag(exp(log(N_inv_nonwestern)), 1)
                   + as.character(TechGroup):trend
                   + exp(log(N_inv_anglosaxon))
                   |
                        lag(exp(log(imputed_N_inv_nonwestern)), 1)
                   + as.character(TechGroup):trend
                   + exp(log(imputed_N_inv_anglosaxon))
                   ,
                  lost.ts = 3, # => THIS IS THE CRUCIAL PARAMETER TO MAKE IT WORK
                  model = "twosteps", effect = "individual", 
                  # A1 = panel_dat$weight_initial_patents, A2 = panel_dat$weight_initial_patents,
                  data = panel_dat, transformation = "d")
summary(pgmm_model, robust = TRUE)
summary(pgmm_model, vcov = vcovHC)
lmtest::coeftest(pgmm_model, vcov = sandwich::vcovHC(pgmm_model, cluster = "group"))
# https://rdrr.io/rforge/plm/man/pgmm.html


### Problems:

# multicollinearity:
# There are too many trends in this regression. Instead, use only individual fixed effects
# with techology trends and controls for the state-level (GDP, unemp, ict etc. => check literature)

# lags:
# maybe log everything manually and only then use lag() or specify stats::lag()


# PRLIMINARY RESULTS: ---------------------------------------------------------
# (a) Regression with share:
# 1pp increase in foreign share, raises onshoring by around 1.5%
# 20pp increase of foreign inventors in the U.S. (1980-2015) fostered onshoring of patents by around 30%
# Total onshoring increase for the U.S. was ~ 200% i.e. 1/6 of total increase attributed to foreign origin inventors

# (b) Regression with log(N_inv):
# 1% increase in non-western inventors, raises onshoring by around 0.2-0.5%
# 700% increase of non-western inventors in the U.S. (1988-2015) fostered onshoring of patents by around 70-280%
# Total onshoring increase for the U.S. was ~ 1000% 
# i.e. 7-25% of total increase directly attributed to increase of foreign origin inventors
tmp <- dat %>% filter(TimePeriod %in% c("1988", "2015")) %>% 
        group_by(TimePeriod) %>% 
        summarise(N_inv_nonwestern = sum(N_inv_nonwestern, na.rm = TRUE), 
                  N_inv = sum(N_inv_regiotech, na.rm = TRUE))
print("Growth of Inventors: ")
tmp[2 ,-1] / tmp[1, -1] * 100
tmp <- dat %>% filter(TimePeriod %in% c("1991", "2015")) %>% 
        group_by(TimePeriod) %>% 
        summarise(N_onshored = sum(onshored_patents, na.rm = TRUE))
print("Growth of Onshored patents: ")
tmp[2 ,-1] / tmp[1, -1] * 100


# Problem: IV only implemented for OLS
# https://stackoverflow.com/questions/65867315/error-in-the-fixest-package-in-r-when-running-feglm-with-instruments
# ALternative:
# https://cran.r-project.org/web/packages/glmmML/index.html
# https://github.com/amrei-stammann/alpaca

#### subset to regio-techs that matter...

#### subset to regio-techs that matter...
tmp <- dat %>% group_by(regio_tech) %>% 
        summarize(total_onshored = sum(onshored_patents)) %>%
        filter(total_onshored >= 90)
panel_dat <- dat %>% filter(regio_tech %in% tmp$regio_tech)
panel_dat$trend <- NULL
tmp <- data.frame(TimePeriod = sort(unique(panel_dat$TimePeriod)), 
                  trend = seq(1, length(unique(panel_dat$TimePeriod))))
panel_dat <- left_join(panel_dat, tmp, by = "TimePeriod")
panel_dat$TimePeriod = as.character(panel_dat$TimePeriod)
panel_dat <- panel(data = panel_dat, panel.id = c("regio_tech", "TimePeriod"))



