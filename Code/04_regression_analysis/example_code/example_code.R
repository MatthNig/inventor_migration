###############################
####### Data & packages #######
###############################

library("gmm")
library("momentfit")
library("dplyr")

PATH <- "/scicore/home/weder/nigmat01/inventor_migration/Code/04_regression_analysis/example_code"
# PATH <- "..."
setwd(PATH)
panel_dat <- read.csv("dat.csv")
head(panel_dat) # all variables are in logs

##################################################
####### Specify the moment condition model #######
##################################################

#### Transform the conditional mean specification to eliminate fixed effects
# According to Cameron & Trivedi (2007):
# Demean E(Y_it|X_it, a_i) = g_it = a_i + exp(X_it * theta)
# by using g_i = mean(g_it) to eliminate the fixed effects a_i.

moment_fun <- function(theta, x){
        
        # transform the vector of parameter values to a matrix and
        # state the (possibly endogenous) explanatory variables from g_it
        theta <- as.matrix(theta, ncol = 1)
        expl_vars <- rownames(theta)
        
        # get the region-techfield identifiers for the fixed effects
        identifiers <- unique(x$regio_tech)
        
        # define moment conditions for every instrument:
        m <- lapply(INSTRUMENTS, function(instrument){
                
                # specify moment condition for every region-techfield
                m_i <- lapply(identifiers, function(id){
                        
                        tmp <- subset(x, regio_tech == id)
                        
                        # transform the the conditional mean specification
                        # to eliminate the region-techfield fixed effects (Cameron & Trivedi, 2007):
                        g_it <- exp(as.matrix(tmp[, expl_vars]) %*% theta)
                        g_it <- g_it[, 1]
                        g_i <- mean(g_it)
                        g_demeaned_it <- g_it - g_i
                        
                        # calculate and return the T moment conditions for region-techfield i
                        y_demeaned_it <- tmp$onshored_patents - mean(tmp$onshored_patents)
                        z_it <- tmp[, instrument]
                        m_it <-  z_it * (y_demeaned_it - g_demeaned_it)
                        return(m_it)
                })
                
                # stack the moment conditions for all N regions 
                # in a (NT x 1) vector
                m <- unlist(m_i)
                names(m) <- NULL
                return(m)
        })
        
        # combine the m moment conditions in a matrix
        moments <- as.matrix(m[[1]])
        if(length(m) > 1){
                for(moment in 2:length(m)){
                        moments <- cbind(moments, m[[moment]])
                }
        }
        
        # return matrix of moment conditions
        return(moments)
}

###################################################
####### Specify the parameters and instruments ####
###################################################

EXPLAN_VARS <- c("N_inv_nonwestern_log", 
                 "N_inv_anglosaxon_log",
                 "N_patents_TechGroup_log"
                 )
THETA <- rep(0, length(EXPLAN_VARS))
names(THETA) <- EXPLAN_VARS # This is necessary for the moment function to work

INSTRUMENTS <- c("imputed_N_inv_nonwestern_log", 
                 "imputed_N_inv_anglosaxon_log",
                 "N_patents_TechGroup_log"
                 )
if((length(THETA) == length(EXPLAN_VARS) & length(THETA) == length(INSTRUMENTS)) == FALSE){
        warning("Number of parameters, explanatory variables and instruments do not match")
}else{print("Variables and parameters specified")}

#########################################
##### Estimate using package "gmm" ######
#########################################

res1 <- gmm(g = moment_fun, x = panel_dat, 
           t0 = THETA, type = "twoStep")
summary(res1)

#############################################
##### Estimate using package "momentFit" ####
#############################################

gmm_model <- momentModel(g = moment_fun, x = panel_dat,
                         theta0 = THETA,
                         vcov = "CL",
                         vcovOptions = list(cluster=~regio_tech), 
                         data = panel_dat)
gmm_model
res2 <- gmmFit(model = gmm_model, type = "twostep")
summary(res2, sandwich = TRUE, df.adj = TRUE)
