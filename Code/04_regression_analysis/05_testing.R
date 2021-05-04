### testing stage for
# eliminate fixed effects of non-linear model
# introduce time-trends


#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
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
T_min <- 5
keep_regiotech <- panel_dat %>% 
        group_by(regio_tech) %>% 
        summarise(count = n()) %>% 
        filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]


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