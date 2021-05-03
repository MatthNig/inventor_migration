##################################################################
# Description:  Script to estimate a gmm models for the          #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 03.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
          "gmm",
          "lmtest",
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
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                warning("Make sure your working directory is the repository directory.")}

###################################
####### Load & process data #######
###################################

dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
print("Panel data loaded and ready for estimation.")

# only use observations with information on non-western inventors:
panel_dat <- dat %>% 
        filter(is.na(N_inv_nonwestern) == FALSE &
                       is.na(N_inv_anglosaxon) == FALSE &
                       TimePeriod > 1984)

# only use observations with at least T_min observations
T_min <- 5
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]

# define the dependent variable
dep_var <- "onshored_patents"

# define the explanatory variables:
explan_vars <- c("N_inv_nonwestern", "N_inv_anglosaxon", "N_patents_state", "N_patents_TechGroup")

# define the exogenous instruments (i.e. different moment conditions):
instruments <- grep("N_inv", explan_vars, value = TRUE) # imputed inventors for endogenous variables_
instruments <- paste0("imputed_", instruments)
instruments <- c(instruments, grep("N_pat", explan_vars, value = TRUE)) # remaining exogenous variables:

# subset to these variable plus the region identifiers:
panel_dat <- panel_dat[, c("regio_tech", dep_var, explan_vars, instruments)]

# log all instruments and explanatory variables:
panel_dat[, c("N_inv_nonwestern","N_inv_anglosaxon", instruments)] <- sapply(
        panel_dat[, c("N_inv_nonwestern", "N_inv_anglosaxon", instruments)], log)

###################################################
####### Specify the moment conditions model #######
###################################################

# demean the nonlinear conditional mean specification g(X_it) 
# according to Cameron & Trivedi (2007):
# Subtract g_i, the mean of g(X_it) per region over time T, from
# g(X_it) to eliminate the fixed effects

moment_fun <- function(theta, x){
        
        # get the identifiers
        identifiers <- unique(x$regio_tech)
        
        # define moment conditions for every instrument:
        m <- lapply(instruments, function(instrument){
                
                m_i <- lapply(identifiers, function(id){
                        
                        tmp <- subset(x, regio_tech == id)
                        
                        # transform the the conditional mean function
                        # in order to eliminate the fixed effects
                        CMF_t <- exp(tmp$N_inv_nonwestern * theta["N_inv_nonwestern"]
                                             + tmp$N_inv_anglosaxon * theta["N_inv_anglosaxon"]
                                             + tmp$N_patents_state * theta["N_patents_state"]
                                             + tmp$N_patents_TechGroup * theta["N_patents_TechGroup"]
                        )
                        CMF_mean <- mean(CMF_t)
                        CMF_demeaned_i <- CMF_t - CMF_mean
                        
                        # calculate and return the T moment conditions for region i
                        y_demeaned_i <- tmp$onshored_patents - mean(tmp$onshored_patents)
                        z_i <- tmp[, instrument]
                        m_i <-  z_i * (y_demeaned_i - CMF_demeaned_i)
                        return(m_i)
                })
                
                # stack the moment conditions for all N regions 
                # in a (NT x 1)-dimension vector
                m <- unlist(m_i)
                names(m) <- NULL
                return(m)
        })
        
        # combine the m moment conditions in a matrix
        moments <- as.matrix(m[[1]])
        if(length(m) > 1){
                for(condition in 2:length(m)){
                        moments <- cbind(moments, m[[condition]])
                }
        }

        # return matrix of moment conditions
        return(moments)
}

#################################
####### Estimate with GMM #######
#################################

PARAMS <- c(N_inv_nonwestern = 0 
            ,N_inv_anglosaxon = 0
            ,N_patents_state = 0
            ,N_patents_TechGroup = 0
            )


# estimate using package "gmm"
res <- gmm(g = moment_fun, x = panel_dat, 
           t0 = PARAMS, type = "twoStep",
           )
summary(res)

# estimate using package "momentfit"
gmm_model <- momentModel(g = moment_fun, x = panel_dat,
                    theta0 = PARAMS,
                    vcov = "CL",
                    vcovOptions = list(cluster=~regio_tech), 
                    data = panel_dat)
gmm_model
res2 <- gmmFit(model = gmm_model, type = "twostep")
summary(res2, sandwich = TRUE, df.adj = TRUE)

### Problem:
# estimator sieht ok aus aber die standard errors sind gewaltig gross...







###-----
# this one works on both methods:
# moment_fun <- function(theta, x){
#         
#         identifiers <- unique(df$regio_tech)
# 
#         m <- lapply(names(theta), function(moment){
#                 
#                 # specifiy the moment condition (i.e. the variable)
#                 EXPL_VAR <- as.character(moment)
#                 
#                 m_i <- lapply(identifiers, function(id){
#                         
#                         tmp <- subset(df, regio_tech == id)
#                         
#                         # determine number of time periods and the instrument for region i
#                         periods <- nrow(tmp)
#                         z <- log(tmp$imputed_N_inv_nonwestern)
#                         
#                         # transform the outcome and the conditional mean function
#                         # in order to eliminate the fixed effects
#                         y_demeaned <- tmp$onshored_patents - mean(tmp$onshored_patents)
#                         CMF_t <- exp(log(tmp[, EXPL_VAR]) * theta[moment])
#                         CMF_mean <- mean(CM_t)
# 
#                         # calculate and return the moment conditions for region i
#                         m_i <-  z * (y_demeaned - (CM_t - CM_mean))
#                         return(m_i)
#                 })
#                 
#                 # stack all the N x T moment conditions for all regions for moment m
#                 m <- unlist(m_i)
#                 
#                 return(m)
#         })
#         
#         # combine the different moment conditions
#         moments <- m[[1]]
#         for(condition in 2:length(m)){
#                 moments <- cbind(moments, m[[condition]])
#                 
#         }
#         
#         names(moments) <- names(theta)
#         
#         # return matrix of moment conditions
#         return(moments)
# }





