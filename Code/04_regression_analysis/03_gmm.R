##################################################################
# Description:  Script to estimate a gmm models for the          #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 30.04.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
          "momentfit",
          "caret")

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

# only keep groups with at least T = 2 (previous: 4)
# because of panel and lag structure
N_old <- nrow(panel_dat)
keep_idx <- panel_dat %>%
  group_by(regio_tech) %>%
  summarize(count = n()) %>%
  filter(count > 5) #== length(unique(panel_dat$TimePeriod))) # only complete obs
keep_idx <- keep_idx$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_idx, ]
paste("Dropped", N_old - nrow(panel_dat), "observations from the sample.")

# select variables of interest
panel_dat <- panel_dat %>% select(regio_tech, regio_inv, TechGroup,
                                  onshored_patents,
                                  N_inv_nonwestern, imputed_N_inv_nonwestern,
                                  N_inv_ChinaIndia, imputed_N_inv_ChinaIndia,
                                  N_inv_anglosaxon, imputed_N_inv_anglosaxon,
                                  N_patents_state, N_patents_TechGroup)

##############################################
##### Create Dummies for fixed effects #######
##############################################

# save regio_techs for clustering:
# turn character variables to numerics
convert_to_num <- function(df, exp_var){

  # construct numeric identifiers
  identifiers <- unique(df[, exp_var])
  tmp <- data.frame(id_num = as.numeric(seq(length(identifiers))))
  tmp[, exp_var] <- identifiers

  # merge to original df & substitute character identifier
  df <- merge(df, tmp, by = exp_var)
  df <- df %>% select(-all_of(exp_var))
  names(df)[which(colnames(df) == "id_num")] <- exp_var

  # return updated data.frame
  return(df)
}

clusters <- convert_to_num(panel_dat, "regio_tech")$regio_tech

# create dummies

create_dummies <- function(fixed_effect = "two"){}

panel_dat <- panel_dat %>% select(-regio_inv, -TechGroup)
dmy <- dummyVars(" ~ .", data = panel_dat)
panel_dat <- data.frame(predict(dmy, newdata = panel_dat))
panel_dat <- panel_dat[, -1] # drop one level of the dummies as a reference category

dmy <- dummyVars(" ~ .", data = panel_dat %>% select(-regio_tech))
panel_dat <- data.frame(predict(dmy, newdata = panel_dat %>% select(-regio_tech)))
panel_dat <- panel_dat %>% select(-TechGroupTransport, -regio_invAlabama)

# add clusters:
if(sum(names(panel_dat) == "regio_tech") != 0){warnings("Dummies not correctly specified")}
panel_dat$regio_tech <- clusters

###################################
####### Estimation with GMM #######
###################################

EXPL_VAR <- "log(N_inv_nonwestern)"
EXPL_VAR <- "log(N_inv_ChinaIndia)"

CTRL_VAR <- c("log(N_inv_anglosaxon)",
              "log(N_patents_state)",
              "log(N_patents_TechGroup)")
EXPL_VAR_INSTR <- "log(imputed_N_inv_nonwestern)"
EXPL_VAR_INSTR <- "log(imputed_N_inv_ChinaIndia)"
CTRL_VAR_INSTR <- c("log(imputed_N_inv_anglosaxon)",
                    "log(N_patents_state)",
                    "log(N_patents_TechGroup)")

FIXED_EFFECTS <- grep("regio_", names(panel_dat), value = TRUE)
FIXED_EFFECTS <- c(FIXED_EFFECTS, grep("TechGroup", names(panel_dat), value = TRUE))
FIXED_EFFECTS <- FIXED_EFFECTS[FIXED_EFFECTS != "regio_tech"]
FIXED_EFFECTS <- FIXED_EFFECTS[FIXED_EFFECTS != "N_patents_TechGroup"]
# FIXED_EFFECTS <- FIXED_EFFECTS[1:10] # for testing

# 1) without estimating dummy-parameters
N_regressors <- sum(length(EXPL_VAR), length(CTRL_VAR))
theta_starting <- rep(0, times = N_regressors)
names(theta_starting) <- paste0("b", seq(N_regressors))

gmm_formula <- paste0("onshored_patents ~ exp(", 
                      paste0(
                        paste0("b", seq(N_regressors), "*", c(EXPL_VAR , CTRL_VAR), collapse = " + ") 
                            ," + ", paste(FIXED_EFFECTS, collapse = " + ")
                            ), 
                      ")")
gmm_formula <- as.formula(gmm_formula)

instruments <- paste("~", paste0(EXPL_VAR_INSTR, " + ",
                                 paste(CTRL_VAR_INSTR, collapse = " + ")
                                 , " + ", paste(FIXED_EFFECTS, collapse = " + ")
                                 )
                     )
instruments <- as.formula(instruments)

gmm_model <- gmm4(gmm_formula, instruments,
                  vcov = "CL", type = "twostep", 
                  vcovOptions=list(cluster=~regio_tech),
                  theta0=theta_starting, data=panel_dat)
summary(gmm_model, sandwich = TRUE, df.adj = TRUE)

# => kann es nicht schätzen mit cluster robust & twostep bei regio_tech FE.
"Error in chol.default(do.call(meatCL, opt)) : 
  the leading minor of order 399 is not positive definite"
# => bei onestep klappt es aber kann dann den sandwich se nicht schätzen

### ABER KANN ES SCHÄTZEN MIT regio_inv und tech_group dummies statt regio_tech!
# das kommt ja eigentlich auf dasselbe raus

# --- mit parameter für dummies:
N_regressors <- sum(length(EXPL_VAR), length(CTRL_VAR), length(FIXED_EFFECTS))
theta_starting <- rep(0, times = N_regressors)
names(theta_starting) <- paste0("b", seq(N_regressors))

gmm_formula <- paste0("onshored_patents ~ exp(", 
                      paste0(
                        paste0("b", seq(N_regressors), "*", c(EXPL_VAR , CTRL_VAR, FIXED_EFFECTS), 
                               collapse = " + ")),")")
gmm_formula <- as.formula(gmm_formula)

instruments <- paste("~", paste0(EXPL_VAR_INSTR, " + ",
                                 paste(CTRL_VAR_INSTR, collapse = " + ")
                                 , " + ", paste(FIXED_EFFECTS, collapse = " + ")
                                 )
                     )
instruments <- as.formula(instruments)

gmm_model <- gmm4(gmm_formula, instruments,
                  vcov = "CL", type = "twostep", 
                  vcovOptions=list(cluster=~regio_tech),
                  theta0=theta_starting, data=panel_dat)
summary(gmm_model, sandwich = TRUE, df.adj = TRUE)


