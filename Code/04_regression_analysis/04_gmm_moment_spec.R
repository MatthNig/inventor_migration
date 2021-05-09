##################################################################
# Description:  Script to estimate a gmm model for the           #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 07.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
          "caret",
          "gmm",
          "momentfit")

# install packages if necessary
inst_pkgs <- which(pkgs %in% rownames(installed.packages()) == FALSE)
inst_pkgs <- pkgs[inst_pkgs]
if(length(inst_pkgs) > 0){
        for(p in 1:length(inst_pkgs)){install.packages(inst_pkgs[p])}
}

# load packages
for (p in 1:length(pkgs)) {
        p <- pkgs[p]
        library(p, character.only = TRUE)
}
print("All necessary packages installed and loaded")

# directories  -----------------------------------------------------------------
setwd("/scicore/home/weder/nigmat01/inventor_migration") # for using the cluster
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                warning("Make sure your working directory is the repository directory.")}

###################################
####### Load & process data #######
###################################

load_dat_fun <- function(type){
        if(type == "all"){
                panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_subsidiaries.csv"))
        }else{
                panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
        }
}

panel_dat <- load_dat_fun("all")
print("Panel data loaded.")

# only use observations with information on non-western inventors:
panel_dat <- panel_dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       TimePeriod > 1984)

# only use observations with at least T_min observations
T_min <- 3
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]
paste("Panel dataset with", nrow(panel_dat), "observations loaded")

################################################
####### Define the variables of interest #######
################################################

# define the dependent variable
dep_var <- "onshored_patents"

# define all explanatory variables:
explan_vars <- c("N_inv_nonwestern",
                 "N_inv_regiotech",
                 "N_inv_anglosaxon", 
                 "N_patents_state", 
                 "N_patents_TechGroup")

# define all exogenous instruments:
instruments <- grep("N_inv", explan_vars, value = TRUE) # imputed inventors for endogenous variables_
instruments <- paste0("imputed_", instruments)
instruments <- c(instruments, grep("N_pat", explan_vars, value = TRUE)) # remaining exogenous variables:

# subset to these variable plus the region identifiers:
panel_dat <- panel_dat[, c("regio_tech", "TimePeriod", "TechGroup", 
                           dep_var, 
                           unique(c(explan_vars, instruments)))]

# create dummy variables for Tech-Groups for creating TechGroup-time-trends
N_obs <- nrow(panel_dat)
clusters <- panel_dat$regio_tech
dmy <- dummyVars(" ~ .", data = panel_dat %>% select(-regio_tech))
panel_dat <- data.frame(predict(dmy, newdata = panel_dat %>% select(-regio_tech)))
panel_dat <- panel_dat %>% select(-TechGroupTransport)
dmy <- NULL

# test and add clusters:
if(sum(names(panel_dat) == "regio_tech") != 0){warnings("Dummies not correctly specified")}
if(nrow(panel_dat) != N_obs){warnings("Dummies not correctly specified")}
panel_dat$regio_tech <- clusters
dummies <- grep("TechGroup", names(panel_dat), value = TRUE)
dummies <- dummies[dummies != "N_patents_TechGroup"]

# construct TechGroup time trends
#panel_dat[, dummies] <- panel_dat[, dummies] * panel_dat$TimePeriod

# HIER EINE ERWEITERUNG VORNEHMEN:
# > time trends als 1, 2, 3 ... TimePeriods haben.
# > Grund: mit e^2012 werden die moment conditions gigantisch gross 
# könnte ein problem für den algo sein?
trend <- data.frame(TimePeriod = sort(unique(panel_dat$TimePeriod)),
                    trend = seq(length(unique(panel_dat$TimePeriod)))
                    )
panel_dat <- left_join(panel_dat, trend, by = "TimePeriod")
trend <- NULL
panel_dat[, dummies] <- panel_dat[, dummies] * panel_dat$trend


###################################################
####### Specify the moment conditions model #######
###################################################

# demean the nonlinear conditional mean specification g(X_it) 
# according to Cameron & Trivedi (2007):
# Divide g(X_it) by g_i, the mean of g(X_it) per region over time T,
# to eliminate the fixed effects

moment_fun <- function(theta, x){
        
        # get the identifiers
        identifiers <- unique(x$regio_tech)
        
        # define moment conditions for every instrument:
        m <- lapply(INSTRUMENTS, function(instrument){
                
                # define moment conditions for every regio_tech
                m_i <- lapply(identifiers, function(id){
                        
                        tmp <- subset(x, regio_tech == id)
                        
                        # calculate the conditional mean for every year:
                        CMF_t <- exp(as.matrix(tmp[, names(theta)]) %*% as.matrix(theta, ncol = 1))
                        CMF_t <- CMF_t[, 1]

                        # transform the the conditional mean
                        # in order to eliminate the fixed effects (Cameron & Trivedi, 2005)
                        CMF_mean <- mean(CMF_t)
                        CMF_demeaned_i <- CMF_t / CMF_mean
                        
                        # calculate and return the T moment conditions for region i
                        y_it <- tmp$onshored_patents
                        y_mean <- mean(tmp$onshored_patents)
                        z_it <- tmp[, instrument]
                        m_i <- z_it * (y_it - (CMF_demeaned_i * y_mean))
                        return(m_i)
                })
                
                # stack the moment conditions for all N regio_tech's 
                # in a (NT x 1)-vector
                m <- unlist(m_i)
                names(m) <- NULL
                return(m)
        })
        
        # combine the p moment conditions in a matrix
        moments <- as.matrix(m[[1]])
        if(length(m) > 1){
                for(condition in 2:length(m)){
                        moments <- cbind(moments, m[[condition]])
                }
        }
        
        # return matrix of moment conditions
        return(moments)
}
print("Moment conditions specified")

###################################################
####### Specify the parameters of interest ########
###################################################

# choose explanatory variables
ENDOG_VARS <- "N_inv_nonwestern"

# define exogenous control variables
panel_dat$N_inv_rest <- panel_dat$N_inv_regiotech - panel_dat[, ENDOG_VARS]
EXOG_VARS <- c("N_inv_rest", "N_patents_TechGroup")

# choose whether to use time trends
param_spec <- function(explvars, trends = TRUE, theta_start = 0.2){
        
        tmp <- unlist(lapply(explvars, function(var){
                tmp <- grep(var, instruments, value = TRUE)
                return(tmp)}))
        instruments <- c(unique(tmp), explvars[!explvars %in% tmp])
        
        if(trends){
                explvars <- c(explvars, dummies)
                explvars <- explvars[!explvars %in% c("N_patents_TechGroup", "N_patents_state")]
                
                theta <- rep(theta_start, length(explvars))
                names(theta) <- explvars
                
                instruments <- c(instruments, dummies)
                instruments <- instruments[!instruments %in% c("N_patents_TechGroup", "N_patents_state")]
        }else{
                theta <- rep(theta_start, length(explvars))
                names(theta) <- explvars
                instruments <- instruments
        }
        PARAMS <- list(theta, explvars, instruments)
        names(PARAMS) <- c("theta", "explan_vars", "instruments")
        return(PARAMS)
}

PARAMS <- param_spec(trends = FALSE, theta_start = 0.1, explvars = c(ENDOG_VARS, EXOG_VARS))
#PARAMS <- param_spec(trends = TRUE, theta_start = 0.1, explvars = c(ENDOG_VARS, EXOG_VARS))
THETA <- PARAMS[["theta"]]
EXPLAN_VARS <- PARAMS[["explan_vars"]]
INSTRUMENTS <- PARAMS[["instruments"]]
INSTRUMENTS <- INSTRUMENTS[!INSTRUMENTS %in% ENDOG_VARS]

# log all instruments and explanatory variables:
LOG_VARS <- unique(c(grep("N_inv", c(EXPLAN_VARS, INSTRUMENTS), value = TRUE),
                     grep("N_pat", c(EXPLAN_VARS, INSTRUMENTS), value = TRUE)))
panel_dat[, LOG_VARS] <- sapply(panel_dat[, LOG_VARS], log)

if((length(THETA) == length(EXPLAN_VARS) & length(THETA) == length(INSTRUMENTS)) == FALSE){
        warning("Number of parameters, explanatory variables and instruments do not match")
}else{print("Variables and parameters successfully specified")}

# lag all explanatory variables and instruments
LAG <- 1
tmp <- panel_dat %>% mutate(idx = rownames(panel_dat))
tmp <- tmp %>% group_by(regio_tech) %>% arrange(TimePeriod) %>%
        select(-onshored_patents) %>% mutate_all(~dplyr::lag(., LAG)) %>% 
        arrange(as.numeric(idx))
panel_dat <- panel_dat[, c("regio_tech", "onshored_patents", unique(c(EXPLAN_VARS, INSTRUMENTS)))]
panel_dat[, -2] <- tmp[, names(panel_dat)[-2]]
tmp <- NULL
panel_dat <- panel_dat[complete.cases(panel_dat), ]
paste("Regression dataset with", nrow(panel_dat), "observations ready for analysis")

#################################
####### Estimate with GMM #######
#################################

# estimate using package "gmm"
# res <- gmm(g = moment_fun, x = panel_dat, 
#           t0 = THETA, type = "twoStep")
#print("Moment model estimated using package 'gmm':")
#print(summary(res))

# estimate using package "momentfit"
gmm_model <- momentModel(g = moment_fun, x = panel_dat,
                         theta0 = THETA,
                         vcov = "CL",
                         vcovOptions = list(cluster=~regio_tech), 
                         data = panel_dat)
gmm_model
res2 <- gmmFit(model = gmm_model, type = "twostep")
print("Moment model estimated using package 'momentFit':")
summary(res2, sandwich = TRUE, df.adj = TRUE)

# die Schätzung ohne technology field trends gibt fast exakt dasselbe wie TSLS
# warum funktionieren die Trends nicht??
# => könnte das sein weil sehr viele 0 sind und ihn das verwirrt?




