##################################################################
# Description:  Script to estimate a gmm models for the          #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 02.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
          "gmm")
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

# define the dependent variable
dep_var <- "onshored_patents"

# define the endogenous variables:
endog_vars <- c("N_inv_nonwestern", "N_inv_anglosaxon")

# define the exogenous instruments (i.e. different moment conditions):
instruments <- grep("N_inv", names(theta), value = TRUE) # imputed inventors for endogenous variables_
instruments <- paste0("imputed_", instruments)
instruments <- c(instruments, grep("N_pat", names(theta), value = TRUE)) # remaining exogenous variables:

# subset to these variable plus the region identifiers:
panel_dat <- panel_dat[, c("regio_tech", dep_var, endog_vars, instruments)]

# log all instruments and endogenous variables:
panel_dat[, c(endog_vars, instruments)] <- sapply(panel_dat[, c(endog_vars, instruments)], log)

###################################################
####### Specify the moment conditions model #######
###################################################


###-----
# 1) demean the conditional mean specification according to Cameron & Trivedi (2007)
# by substracting the mean over time.

# 2) estimate this transformed conditional mean function with gmm using instruments.

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

res <- gmm(g = moment_fun, 
           x = panel_dat, 
           t0 = c(N_inv_nonwestern = 0, 
                  N_inv_anglosaxon = 0,
                  N_patents_state = 0,
                  N_patents_TechGroup = 0)
           )
summary(res)





# ------ Solution 
moment_fun <- function(theta, x){
        
        # get the identifiers
        identifiers <- unique(x$regio_tech)
        
        # define moment conditions for every instrument:
        m <- lapply(instruments, function(instrument){
                
                m_i <- lapply(identifiers, function(id){
                        
                        tmp <- subset(x, regio_tech == id)
                        
                        # transform the outcome and the conditional mean function
                        # in order to eliminate the fixed effects
                        y_demeaned <- tmp$onshored_patents - mean(tmp$onshored_patents)
                        CMF_t <- exp(tmp$N_inv_nonwestern * theta["N_inv_nonwestern"] +
                                             tmp$N_inv_anglosaxon * theta["N_inv_anglosaxon"] +
                                             tmp$N_patents_state * theta["N_patents_state"] +
                                             tmp$N_patents_TechGroup * theta["N_patents_TechGroup"]
                                )
                        CMF_mean <- mean(CMF_t)
                        
                        # calculate and return the T moment conditions for region i
                        z <- tmp[, instrument]
                        m_i <-  z * (y_demeaned - (CMF_t - CMF_mean))
                        return(m_i)
                })
                
                # stack the moment conditions for all N regions 
                # in a (NT x 1)-dimension vector
                m <- unlist(m_i)
                names(m) <- NULL
                return(m)
        })
        
        # combine the moment condition vectors in a matrix
        moments <- as.matrix(m[[1]])
        for(condition in 2:length(m)){
                moments <- cbind(moments, m[[condition]])
                
        }
        
        # return matrix of moment conditions
        return(moments)
}

### -----


# momentfit equivalent
library(momentfit)
mod2 <- momentModel(g = moment_fun, x = panel_dat,
                    theta0 = c(N_inv_nonwestern = 0,
                              N_inv_anglosaxon = 0,
                              N_patents_state = 0,
                              N_patents_TechGroup = 0),
                    vcov = "CL", vcovOptions = list(cluster=~regio_tech), 
                    data = panel_dat)
mod2
res1 <- gmmFit(mod2, type = "twostep")
summary(res1, sandwich = TRUE, df.adj = TRUE)



