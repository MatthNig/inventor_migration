### testing stage for
# eliminate fixed effects of non-linear model
# introduce time-trends


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
T_min <- 3
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]
paste("Panel dataset with", nrow(panel_dat), "observations ready for regression analysis.")

################################################
####### Define the variables of interest #######
################################################

# define the dependent variable
dep_var <- "onshored_patents"

# define the explanatory variables:
explan_vars <- c("N_inv_nonwestern", 
                 "N_inv_anglosaxon", 
                 "N_patents_state", 
                 "N_patents_TechGroup")

# define the exogenous instruments:
instruments <- grep("N_inv", explan_vars, value = TRUE) # imputed inventors for endogenous variables_
instruments <- paste0("imputed_", instruments)
instruments <- c(instruments, grep("N_pat", explan_vars, value = TRUE)) # remaining exogenous variables:

# subset to these variable plus the region identifiers:
panel_dat <- panel_dat[, c("regio_tech", "TimePeriod", "TechGroup", 
                           dep_var, 
                           unique(c(explan_vars, instruments)))]

# log all instruments and explanatory variables:
panel_dat[, c("N_inv_nonwestern","N_inv_anglosaxon", instruments)] <- sapply(
        panel_dat[, c("N_inv_nonwestern", "N_inv_anglosaxon", instruments)], log)

# create dummy variables for Tech-Groups for creating TechGroup-time-trends
N_obs <- nrow(panel_dat)
clusters <- panel_dat$regio_tech
dmy <- dummyVars(" ~ .", data = panel_dat %>% select(-regio_tech))
panel_dat <- data.frame(predict(dmy, newdata = panel_dat %>% select(-regio_tech)))
panel_dat <- panel_dat %>% select(-TechGroupTransport)

# test and add clusters:
if(sum(names(panel_dat) == "regio_tech") != 0){warnings("Dummies not correctly specified")}
if(nrow(panel_dat) != N_obs){warnings("Dummies not correctly specified")}
panel_dat$regio_tech <- clusters

dummies <- grep("TechGroup", names(panel_dat), value = TRUE)
dummies <- dummies[dummies != "N_patents_TechGroup"]

# construct TechGroup time trends
panel_dat[, dummies] <- panel_dat[, dummies] * panel_dat$TimePeriod


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
        m <- lapply(INSTRUMENTS, function(instrument){
                
                m_i <- lapply(identifiers, function(id){
                        
                        tmp <- subset(x, regio_tech == id)
                        
                        # transform the the conditional mean function
                        # in order to eliminate the fixed effects
                        CMF_t <- exp(as.matrix(tmp[, names(theta)]) %*% as.matrix(theta, ncol = 1))
                        CMF_t <- CMF_t[, 1]
                        # CMF_t <- exp(rowSums(as.matrix(tmp[, names(theta)]) %*% diag(theta)))
                        
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

###################################################
####### Specify the parameters of interest ########
###################################################

# choose whether to use time trends
param_spec <- function(trends = TRUE){
        
        if(trends){
                explan_vars <- c(explan_vars, dummies)
                explan_vars <- explan_vars[!explan_vars %in% c("N_patents_TechGroup", "N_patents_state")]
                
                theta <- rep(0.1, length(explan_vars))
                names(theta) <- explan_vars
                
                instruments <- c(instruments, dummies)
                instruments <- instruments[!instruments %in% c("N_patents_TechGroup", "N_patents_state")]
        }else{
                theta <- rep(0.1, length(explan_vars))
                names(theta) <- explan_vars
                instruments <- instruments
        }
        PARAMS <- list(theta, explan_vars, instruments)
        names(PARAMS) <- c("theta", "explan_vars", "instruments")
        return(PARAMS)
}

PARAMS <- param_spec(trends = TRUE)
theta <- PARAMS[["theta"]]
EXPLAN_VARS <- PARAMS[["explan_vars"]]
INSTRUMENTS <- PARAMS[["instruments"]]

if((length(theta) == length(EXPLAN_VARS) & length(theta) == length(INSTRUMENTS)) == FALSE){
        warning("Number of parameters, explanatory variables and instruments do not match")
}else{print("Variables and parameters successfully specified")}


#################################
####### Estimate with GMM #######
#################################

# estimate using package "gmm"
res <- gmm(g = moment_fun, x = panel_dat, 
           t0 = theta, type = "twoStep")
summary(res)

# estimate using package "momentfit"
gmm_model <- momentModel(g = moment_fun, x = panel_dat,
                         theta0 = theta,
                         vcov = "CL",
                         vcovOptions = list(cluster=~regio_tech), 
                         data = panel_dat)
gmm_model
res2 <- gmmFit(model = gmm_model, type = "twostep")
summary(res2, sandwich = TRUE, df.adj = TRUE)


