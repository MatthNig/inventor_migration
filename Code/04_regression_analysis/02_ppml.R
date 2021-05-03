##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 27.04.2021                                       #
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

dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
dat$TimePeriod <- as.character(dat$TimePeriod)
dat_panel <- panel(data = dat, panel.id = c("regio_tech", "TimePeriod"))
print("Panel data loaded and ready for estimation.")

#################################
####### PPML ESTIMATIONS ########
#################################

# Resources: 
# https://cran.r-project.org/web/packages/fixest/index.html
# http://www.cazaar.com/ta/econ113/interpreting-beta
# https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3444996

#### (1) BASELINE SPECIFICATION ----------------------------------------------------- 
# dat_panel <- dat_panel %>% filter(onshored_patents >= 5)
print("Choose explanatory variable from: ")
c(names(dat)[grepl(c("share"), names(dat))], names(dat)[grepl(c("N_inv"), names(dat))])

# EXPL_VAR <- "log(N_inv_nonwestern)"
EXPL_VAR <- "log(N_inv_ChinaIndia)"
dat_panel$N_inv_rest <- dat_panel$N_inv_regiotech - dat_panel[, EXPL_VAR]

#CTRL_VAR <- "N_inv_rest"
#CTRL_VAR <- "N_inv_regiotech"
CTRL_VAR <- c("log(N_inv_anglosaxon)",
              "log(N_patents_state)",
              "log(N_patents_TechGroup)")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0("onshored_patents ~", 
                  EXPL_VAR, " + ", # foreign origin
                  CTRL_VAR, # total inventors and patenting controls
                  "|", 
                  "regio_tech")
                  # "regio_inv + TechGroup") # state-technology fixed effects
FORMULA <- as.formula(FORMULA)
FORMULA

ppml_model <- feglm(data = dat_panel, fml = FORMULA, family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs

#### (2) Weighted BY INITIAL PATENTNING ACTIVITY : ---------------------------------
print("Choose weight variable from: ")
c(names(dat)[grepl("weight", names(dat))])
WEIGHT_VAR <- "weight_initial_patents"
ppml_model <- feglm(data = dat_panel, fml = FORMULA, 
                    weights = dat_panel[, WEIGHT_VAR], family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # clustered on tech-state pairs and time period

#### (3) REDUCED FORM WITH INSTRUMENT: -----------------------------------------

# drop pre 1984 observations because they were used to construct imputed inventor numbers:
dat_panel <- dat %>% filter(as.numeric(TimePeriod) > 1984)
dat_panel$trend <- NULL
tmp <- data.frame(TimePeriod = sort(unique(dat_panel$TimePeriod)), 
                  trend = seq(1, length(unique(dat_panel$TimePeriod))))
dat_panel <- left_join(dat_panel, tmp, by = "TimePeriod")
dat_panel$TimePeriod = as.character(dat_panel$TimePeriod)
dat_panel <- panel(data = dat_panel, panel.id = c("regio_tech", "TimePeriod"))

# inspect correlation of imputed number of foreign origin inventors
cor(dat_panel$N_inv_nonwestern, dat_panel$imputed_N_inv_nonwestern, use = "complete.obs")
ggplot(dat_panel, aes(x = log(N_inv_nonwestern), y = log(imputed_N_inv_nonwestern)))+
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
cor(dat_panel$non_western_share, dat_panel$non_western_share_imputed, use = "complete.obs")
ggplot(dat_panel, aes(x = non_western_share, y = non_western_share_imputed))+
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
EXPL_VAR <- "log(imputed_N_inv_nonwestern)"
EXPL_VAR <- "log(imputed_N_inv_ChinaIndia)"

#CTRL_VAR <- "N_inv_rest"
#CTRL_VAR <- "N_inv_regiotech"
CTRL_VAR <- c("log(imputed_N_inv_anglosaxon)",
              "log(N_patents_state)",
              "log(N_patents_TechGroup)")

LAGS <- "0"
EXPL_VAR <- paste0("l(", EXPL_VAR, ", ", LAGS, ")")
CTRL_VAR <- paste(paste0("l(", CTRL_VAR, ", ", LAGS, ")"), collapse = " + ")

FORMULA <- paste0("onshored_patents ~", 
                  EXPL_VAR, " + ", # foreign origin
                  CTRL_VAR, # total inventors and patenting controls
                  "|", 
                  "regio_inv + TechGroup") # state-technology fixed effects
FORMULA <- as.formula(FORMULA)
FORMULA

# estimate the model without weights
ppml_model <- feglm(data = dat_panel, fml = FORMULA, family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

# estimate the model with weights
ppml_model <- feglm(data = dat_panel, fml = FORMULA, weights = dat_panel[, WEIGHT_VAR],
                    family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs and time periods

# (1) klappt nicht mit reduced form: weshalb nicht? diejenigen regio_techs mit sehr grossen imputed shares sind jene
# traditionellen technologien, die nicht sonderlich viele patente anziehen. das zieht dann den schnitt extrem runter

# (2) für imputed share am entscheidendsten ist die Grösse des States. NY, Pennsylvania etc. haben teils extrem hohe
# imputed shares! Da einfach deshalb, weil sie in initial period sehr hohe Anteil hatten (~15-20%). Wenn dann in späteren
# Perioden relativ viele foreign origin inventors kommen, dann ist der imputed share enorm hoch d.h. höher als
# tatsächliche Anzahl inventors. Das Gegenteil ist der Fall bei kleinen States.


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
tmp <- data.frame(TimePeriod = sort(unique(dat_panel$TimePeriod)), 
                  trend = seq(1, length(unique(dat_panel$TimePeriod))))
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
dat_panel <- dat %>% filter(regio_tech %in% tmp$regio_tech)
dat_panel$trend <- NULL
tmp <- data.frame(TimePeriod = sort(unique(dat_panel$TimePeriod)), 
                  trend = seq(1, length(unique(dat_panel$TimePeriod))))
dat_panel <- left_join(dat_panel, tmp, by = "TimePeriod")
dat_panel$TimePeriod = as.character(dat_panel$TimePeriod)
dat_panel <- panel(data = dat_panel, panel.id = c("regio_tech", "TimePeriod"))



