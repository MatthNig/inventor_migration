##################################################################
# Description:  Script to estimate a gmm model for the           #
#               relationship between the number of foreign       #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 24.06.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse", "caret", "momentfit")

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

#panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))
panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"))
print("Panel data loaded.")

# only use observations with information on non-western inventors:
min_period <- 1984
#min_period <- 2000
panel_dat <- panel_dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       TimePeriod > min_period)

# only use observations with at least T_min observations
T_min <- 3
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]
paste("Panel dataset with", nrow(panel_dat), "observations loaded")
keep_regiotech <- NULL

# if excluding some techfields:
# unique(panel_dat$TechGroup)
# excl_tech <- c("Information & Communication Technology", "Electrical Machinery",
#                "Audiovisual Technologies", "Computer Science", "Medical Technology")
# panel_dat <- filter(panel_dat, !TechGroup %in% excl_tech)


################################################
####### Define the variables of interest #######
################################################

print("Choose dependent variable from: ")
c(names(panel_dat)[grepl(c("shor"), names(panel_dat))])

print("Choose explanatory variable from: ")
c(names(panel_dat)[grepl(c("share"), names(panel_dat))], names(panel_dat)[grepl(c("N_inv"), names(panel_dat))])

print("Choose weight variable from: ")
c(names(panel_dat)[grepl("weight", names(panel_dat))])

# define the dependent variable
dep_var <- "onshored_patents"

# define all explanatory variables:
explan_vars <- c("N_inv_nonwestern",
                 "N_inv_regiotech",
                 "N_inv_anglosaxon", 
                 "N_patents_state", 
                 "N_patents_TechGroup")

# define all exogenous instruments:
instruments <- grep("N_inv", explan_vars, value = TRUE) # imputed inventors for endogenous variables
instruments <- paste0("imputed_", instruments)
instruments <- c(instruments, grep("N_pat", explan_vars, value = TRUE)) # remaining exogenous variables:

# subset to these variable plus the region identifiers:
panel_dat <- panel_dat[, c("regio_tech", "TimePeriod", "TechGroup", 
                           dep_var, 
                           unique(c(explan_vars, instruments)))]

# # create dummy variables for Tech-Groups for creating TechGroup-time-trends
# N_obs <- nrow(panel_dat)
# clusters <- panel_dat$regio_tech
# dmy <- dummyVars(" ~ .", data = panel_dat %>% dplyr::select(-regio_tech))
# panel_dat <- data.frame(predict(dmy, newdata = panel_dat %>% dplyr::select(-regio_tech)))
# panel_dat <- panel_dat %>% dplyr::select(-TechGroupTransport)
# dmy <- NULL
# 
# # test and add clusters:
# if(sum(names(panel_dat) == "regio_tech") != 0){warnings("Dummies not correctly specified")}
# if(nrow(panel_dat) != N_obs){warnings("Dummies not correctly specified")}
# panel_dat$regio_tech <- clusters
# dummies <- grep("TechGroup", names(panel_dat), value = TRUE)
# dummies <- dummies[dummies != "N_patents_TechGroup"]
# 
# # construct TechGroup time trends
# #panel_dat[, dummies] <- panel_dat[, dummies] * panel_dat$TimePeriod
# trend <- data.frame(TimePeriod = sort(unique(panel_dat$TimePeriod)),
#                     trend = seq(length(unique(panel_dat$TimePeriod)))
#                     )
# panel_dat <- left_join(panel_dat, trend, by = "TimePeriod")
# trend <- NULL
# panel_dat[, dummies] <- panel_dat[, dummies] * panel_dat$trend

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
THETA <- PARAMS[["theta"]]
EXPLAN_VARS <- PARAMS[["explan_vars"]]
INSTRUMENTS <- PARAMS[["instruments"]]
INSTRUMENTS <- INSTRUMENTS[!INSTRUMENTS %in% ENDOG_VARS]
PARAMS <- NULL

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
        dplyr::select(-dep_var) %>% mutate_all(~dplyr::lag(., LAG)) %>% 
        arrange(as.numeric(idx))
panel_dat <- panel_dat[, c("regio_tech", dep_var, unique(c(EXPLAN_VARS, INSTRUMENTS)))]
panel_dat[, -2] <- tmp[, names(panel_dat)[-2]]
tmp <- NULL

panel_dat <- panel_dat[complete.cases(panel_dat), ]
paste("Regression dataset with", nrow(panel_dat), "observations ready for analysis")

#################################
####### Estimate with GMM #######
#################################

# estimate using package "momentfit"
gmm_model <- momentModel(g = moment_fun, x = panel_dat,
                         theta0 = THETA,
                         vcov = "CL",
                         vcovOptions = list(cluster= ~regio_tech), 
                         data = panel_dat)
gmm_model
res2 <- gmmFit(model = gmm_model, type = "twostep")
print("Moment model estimated using package 'momentFit':")
summary(res2, sandwich = TRUE, df.adj = TRUE)


#### REMARKS:
# excluding subsidiaries:--------------
# with lag = 1 und T_min = 5: N = 2'807, beta = 0.24062, p = 0.14227 (=> non-significant)
# with lag = 1 und T_min = 3: N = 2'918, beta = -0.166744, p = 0.3857 (=> non-significant)
# with lag = 1 und T_min = 4: N = 2'876, beta = -0.186214, p = 0.3334 (=> non-significant)
# with lag = 0, always significant and substantially higher coefficient ...
# with lag = 0 und T_min = 5: N = 3'160, beta = 1.429055, p = < 2.2e-16 (=> significant)
# ***with lag = 0 und T_min = 3: N = 3'315, beta = 1.426228, p = < 2.2e-16 (=> significant)***
# with lag = 0 und T_min = 2: N = 3'343, beta = 1.426846, p = < 2.2e-16 (=> significant)


# including subsidiaries:------------
# with lag = 1 und T_min = 5: N = 2'807, beta = -0.0527991, p = 0.6393 (=> non-significant)
# ***with lag = 1 und T_min = 3: N = 2'918, beta = 0.245270, p = 0.125 (=> non-significant)***
# with lag = 1 und T_min = 4: N = 2'876, beta = 0.19997, p = 0.1284 (=> non-significant)
# with lag = 0, always significant and substantially higher coefficient ...
# with lag = 0 und T_min = 5: N = 3'160, beta = 0.74125, p = 8.715e-05 (=> significant)
# with lag = 0 und T_min = 3: N = 3'315 , beta = 0.74393, p = 7.327e-05 (=> significant)
# ***with lag = 0 und T_min = 2: N = 3343 , beta = 0.74436, p = 7.197e-05 *** (=> significant)***

# ****=> use lag = 0 and T_min = 2 (easy to argue) as baseline including subsidiaries****
# => robustness with lag = 1, T_min = 3, would be smaller but positive and close to significance
# => robustness with excluding subsidiaries and lag = 0 would be larger and significant

# rootogramm:
# https://arxiv.org/pdf/1605.01311.pdf
# https://www.r-bloggers.com/2016/06/rootograms/