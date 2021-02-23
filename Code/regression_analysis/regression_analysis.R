##################################################################
# Description:  Script to estimate regression models for the     #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 21.02.2020                                       #
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
# dat_panel <- panel(data = dat_panel, panel.id = c("regio_tech", "TimePeriod"))
print("Choose explanatory variable from: ")
c(names(dat)[grepl(c("share"), names(dat))], names(dat)[grepl(c("N_inv"), names(dat))])
EXPL_VAR <- "log(N_inv_nonwestern)"
# EXPL_VAR <- "log(N_inv_ChinaIndia)"
# EXPL_VAR <- "non_western_share"
LAGS <- "1"
FORMULA <- paste0("onshored_patents ~ l(", EXPL_VAR, ", ", LAGS, ")|", # foreign origin
                  # "l(log(N_inv_regiotech),", LAGS, ")", # domestic origin
                  # "|regio_inv[[trend]] +", # state trends
                  # "TechGroup[[trend]] +", # technology trends
                  "regio_tech + TimePeriod") # state-technology and period fixed effects
FORMULA <- as.formula(FORMULA)

ppml_model <- feglm(data = dat_panel, fml = FORMULA, family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # se clustered on tech-state pairs

#### (2) Weighted BY INITIAL PATENTNING ACTIVITY : ---------------------------------
print("Choose weight variable from: ")
c(names(dat)[grepl("weight", names(dat))])
WEIGHT_VAR <- "weight_initial_patents"
ppml_model <- feglm(data = dat_panel, fml = FORMULA, weights = dat_panel[, WEIGHT_VAR], family = "quasipoisson")
summary(ppml_model, cluster = c("regio_tech")) # clustered on tech-state pairs and time period

#### (3) REDUCED FORM WITH INSTRUMENT: -----------------------------------------
# drop pre 1984 observations because they were used to construct imputed inventor numbers:
dat_panel <- dat %>% filter(as.numeric(TimePeriod) > 1984)
dat_panel <- dat_panel %>% filter(onshored_patents >= 10)
dat_panel$trend <- NULL
tmp <- data.frame(TimePeriod = sort(unique(dat_panel$TimePeriod)), 
                  trend = seq(1, length(unique(dat_panel$TimePeriod))))
dat_panel <- left_join(dat_panel, tmp, by = "TimePeriod")
dat_panel$TimePeriod = as.character(dat_panel$TimePeriod)
dat_panel <- panel(data = dat_panel, panel.id = c("regio_tech", "TimePeriod"))

# inspect correlation of imputed number of foreign origin inventors
cor(dat_panel$N_inv_nonwestern, dat_panel$imputed_N_inv_nonwestern, use = "complete.obs")
ggplot(dat_panel, aes(x = log(N_inv_nonwestern), y = log(imputed_N_inv_nonwestern)))+
        geom_point()+
        geom_abline(intercept = 0, slope = 1, color = "blue")

# # inspect correlation of imputed shares
cor(dat_panel$non_western_share, dat_panel$non_western_share_imputed, use = "complete.obs")
ggplot(dat_panel, aes(x = log(onshored_patents+1), y = lag(non_western_share_imputed, 1)))+
        geom_point()+geom_smooth(method = "lm")
        # geom_abline(intercept = 0, slope = 1, color = "blue")

# choose formula
# EXPL_VAR <- "log(Imputed_N_inv_nonwestern)"
EXPL_VAR <- "non_western_share_imputed"
LAGS <- "1"
FORMULA <- paste0("onshored_patents ~ l(", EXPL_VAR, ", ", LAGS, ")", # foreign origin
                  # "+ l(log(N_inv_domestic),", LAGS, ")", # domestic origin
                  "|regio_inv[[trend]] +", # state trends
                  "TechGroup[[trend]] +", # technology trends
                  "TimePeriod +",
                  "regio_tech") # state-technology and period fixed effects
FORMULA <- as.formula(FORMULA)

# estimate the model
ppml_model <- feglm(data = dat_panel, fml = FORMULA, family = "quasipoisson")#, weights = dat_panel[, WEIGHT_VAR])
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

panel_dat <- dat %>% select(regio_tech, TimePeriod, onshored_patents, non_western_share,
                            N_inv_nonwestern, imputed_N_inv_nonwestern, non_western_share_imputed,
                            weight_initial_patents, regio_inv, TechGroup) %>%
        mutate(TimePeriod = as.numeric(TimePeriod))
tmp <- data.frame(regio_tech = unique(panel_dat$regio_tech), id_num = seq(length(unique(panel_dat$regio_tech))))
panel_dat <- merge(panel_dat, tmp, by = "regio_tech") %>% select(-regio_tech) %>% 
        filter(TimePeriod > 1985) %>% na.omit() %>% rename(regio_tech = id_num)
panel_dat$onshored_patents <- as.numeric(panel_dat$onshored_patents)
panel_dat <- panel_dat[,c(ncol(panel_dat), 1:(ncol(panel_dat)-1))]
panel_dat <- panel_dat %>% group_by(regio_tech) %>% mutate(count = n()) %>%
        # filter(count > 1) %>% 
        select(-count)
panel_dat$regio_tech <- as.numeric(panel_dat$regio_tech)
tmp <- data.frame(regio_inv = unique(panel_dat$regio_inv), id_num = seq(length(unique(panel_dat$regio_inv))))
panel_dat <- merge(panel_dat, tmp, by = "regio_inv") %>% select(-regio_inv) %>% rename(regio_inv = id_num)
tmp <- data.frame(TechGroup = unique(panel_dat$TechGroup), id_num = seq(length(unique(panel_dat$TechGroup))))
panel_dat <- merge(panel_dat, tmp, by = "TechGroup") %>% select(-TechGroup) %>% rename(TechGroup = id_num)
panel_dat <- as.data.frame(panel_dat)

# model
pgmm_model <- pgmm(data = panel_dat,
                   formula = onshored_patents ~ 
                           exp(log(N_inv_nonwestern))
                          # as.numeric(TimePeriod) +
                          # dplyr::lag(non_western_share, 1) +
                          # as.numeric(TimePeriod):as.character(TechGroup)
                          # as.numeric(TimePeriod)#:as.character(regio_inv)
                  |
                          exp(log(imputed_N_inv_nonwestern)),
                          # as.numeric(TimePeriod) +
                          # dplyr::lag(non_western_share_imputed, 1) +
                          # as.numeric(TimePeriod):as.character(TechGroup),
                          # as.numeric(TimePeriod),#:as.character(regio_inv),
                  model = "twostep", #weights = weight_initial_patents,
                  effect = "twoways")
summary(pgmm_model, vcov = vcovHC)
lmtest::coeftest(pgmm_model, vcov = sandwich::vcovHC(pgmm_model, cluster = "group"))

# => scheint mir irgendwie nur auf system GMM / dynamic panel ausgerichtet zu sein.

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
#  https://cran.r-project.org/web/packages/glmmML/index.html
# https://github.com/amrei-stammann/alpaca


