##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 29.06.2021                                       #
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

#panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))
# panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/time_period_robustness.csv"))
panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"))

MIN_PERIOD <- 1984
# MIN_PERIOD <- 2000
panel_dat <- panel_dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       TimePeriod > MIN_PERIOD)
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

# if excluding some techfields:
# excl_tech <- c("Information & Communication Technology", "Electrical Machinery",
#                "Audiovisual Technologies", "Computer Science", "Medical Technology")
# panel_dat <- filter(panel_dat, !TechGroup %in% excl_tech)

# if excluding some states:
# excl_state <- c("New Hampshire", "Kentucky", "Delaware", "Washington", "South Carolina", "Vermont")
# panel_dat <- filter(panel_dat, !regio_inv %in% excl_state)


#################################
####### PPML ESTIMATIONS ########
#################################

print("Choose dependent variable from: ")
c(names(panel_dat)[grepl(c("shor"), names(panel_dat))])

print("Choose explanatory variable from: ")
c(names(panel_dat)[grepl(c("share"), names(panel_dat))], names(panel_dat)[grepl(c("N_inv"), names(panel_dat))])

print("Choose weight variable from: ")
c(names(panel_dat)[grepl("weight", names(panel_dat))])

#### (1) BASELINE SPECIFICATION ----------------------------------------------------- 
DEP_VAR <- "onshored_patents"

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
paste("Number of state-tech-field-pairs in the sample:", length(unique(reg_dat$regio_tech)))

#### (2) Weighted BY INITIAL PATENTNING ACTIVITY : ---------------------------------
ppml_model <- feglm(data = reg_dat, fml = FORMULA, 
                    weights = reg_dat[, WEIGHT_VAR], family = "quasipoisson")
print("PPML model with weights")
summary(ppml_model, cluster = c("regio_tech")) # clustered on tech-state pairs and time period
paste("Number of observations in the sample:", nrow(reg_dat[is.na(reg_dat$weight_initial_patents) == FALSE, ]))
paste("Number of state-tech-field-pairs in the sample:", length(unique(reg_dat[is.na(reg_dat$weight_initial_patents) == FALSE, ]$regio_tech)))


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
paste("Number of state-tech-field-pairs in the sample:", length(unique(reg_dat$regio_tech)))

# estimate the model with weights
ppml_model <- feglm(data = reg_dat, fml = FORMULA, weights = reg_dat[, WEIGHT_VAR],
                    family = "quasipoisson")
print("PPML model with imputed number of inventors with weights:")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods
paste("Number of observations in the sample:", nrow(reg_dat[is.na(reg_dat$weight_initial_patents) == FALSE, ]))
paste("Number of state-tech-field-pairs in the sample:", length(unique(reg_dat[is.na(reg_dat$weight_initial_patents) == FALSE, ]$regio_tech)))


