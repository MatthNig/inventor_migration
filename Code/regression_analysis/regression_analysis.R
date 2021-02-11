##################################################################
# Description:  Script estimate regression models investigating  #
#               the relationship between the share of foreign    #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 11.02.2020                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("viridis")

# directories  -----------------------------------------------------------------
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################
####### Load data #######
#########################

dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))

####################################
####### Descriptive analysis #######
####################################

# (1) calculate percentage change in onshored patents between 1990 and 2010
tmp <- dat %>% filter(TimePeriod %in% c(1980, 2010)) %>% 
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
        geom_smooth(method = "lm", se = FALSE)+
        scale_color_viridis(option = "inferno", discrete = TRUE)+ guides(size = FALSE)+
        geom_point(aes(color = TechGroup, size = weight))+
        labs(title = "Offshoring and inventor diversity", 
             subtitle = "(1980 to 2010)", 
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
dat$foreign_share <- dat$foreign_share * 100
plm_model <- lm(data = dat,
                formula = log(1 + onshored_patents) ~ foreign_share +
                        as.numeric(TimePeriod):as.character(TechGroup_No) +
                        as.numeric(TimePeriod):as.character(regio_inv) +
                        regio_tech + TimePeriod -1, 
                weights = weight)
summary(plm_model)
lmtest::coeftest(plm_model,  vcov = vcovHC(plm_model, 
                                           cluster = plm_dat$regio_tech, "HC1"))[1:5,] # with weights
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

# ressources: https://cran.r-project.org/web/packages/FENmlm/index.html

dat_ppml <- dat
dat_ppml$foreign_share <- dat_ppml$foreign_share * 100
dat_ppml$trend <- as.numeric(dat_ppml$TimePeriod)
# dat_ppml <- merge(dat_ppml, weights, by = "regio_tech", all.x = TRUE)# %>% 
# filter(is.na(weight) == FALSE)

ppml_model <- ppml(dependent_variable = "onshored_patents", dist = "foreign_share",
                   additional_regressors = c("TimePeriod", "regio_tech", "trend"),
                   data = dat_ppml, vce_robust= TRUE)
summary(ppml_model)

ppml_model <- glm(onshored_patents ~ foreign_share +
                          # lag(foreign_share, 1) +
                          trend:regio_inv + trend:TechGroup_No + #trends
                          TimePeriod + regio_tech -1, # fixed effects
                  family="quasipoisson",
                  data = dat_ppml, 
                  weights = weight)
summary(ppml_model)
lmtest::coeftest(ppml_model, vcov=sandwich::vcovHC(ppml_model, "HC1"))[1:5,]
nrow(model.frame(ppml_model)) # number of observations in the regression
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


