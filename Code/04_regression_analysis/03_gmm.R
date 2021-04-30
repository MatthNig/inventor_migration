##################################################################
# Description:  Script to estimate a gmm models for the          #
#               relationship between the share of foreign        #
#               origin inventors and offshoring to the USA       #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 29.04.2021                                       #
##################################################################


# NEW IDEA: INSTEAD OF TIME-DUMMIES AND TRENDS USE: ------------
# A) WITHIN TRANSFORMATION 
# B) OVERALL NUMBER OF PATENTS PER TECHGROUP
# c) OVERALL NUMBER OF PATENTS PER STATE


#######################################
## Load packages and set directories ##
#######################################

# packages----------------------------------------
pkgs <- c("tidyverse",
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
    print("Make sure your working directory is the repository directory.")}

###################################
####### Load & process data #######
###################################

dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data.csv"))
print("Panel data loaded and ready for estimation.")

# only use observations with information on non-western inventors:
panel_dat <- dat %>% 
  filter(is.na(N_inv_nonwestern) == FALSE &
           is.na(N_inv_anglosaxon) == FALSE)

# only keep groups with at least T = 4 
# because of panel and lag structure
N_old <- nrow(panel_dat)
keep_idx <- panel_dat %>% 
  group_by(regio_tech) %>% 
  summarize(count = n()) %>% 
  filter(count > 4)
keep_idx <- keep_idx$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_idx, ]
paste("Dropped", N_old - nrow(panel_dat), "observations from the sample.")

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

# apply for all character variables in the data.frame
for(i in c("regio_tech", "regio_inv", "TechGroup_No")){
  panel_dat <- convert_to_num(df = panel_dat, exp_var = i)
}
panel_dat$onshored_patents <- as.numeric(panel_dat$onshored_patents)

###################################
##### Within Transformation #######
###################################

# take logarithms before transformation
panel_dat <- panel_dat %>% mutate_at(vars(contains("N_inv")), log)

# calculate mean values by regio_tech
var_means <- panel_dat %>% 
  select(-TimePeriod, -trend, -TechGroup_No, weight_initial_patents, -reg_label, -regio_inv, -TechGroup) %>%
  group_by(regio_tech) %>%
  summarize_all(.funs = mean)
names(var_means)[-1] <- paste0(names(var_means)[-1], "_mean")

var_means <- merge(var_means, panel_dat, by = "regio_tech")
var_means <- var_means %>% select(ends_with("_mean"))
names(var_means) <- gsub(pattern = "_mean", replacement = "", x = names(var_means))

# test:
match_test <- function(){
  tmp <- panel_dat[, colnames(var_means)]
  
  if(sum(names(tmp) == names(var_means)) != ncol(var_means)){
    warning("Column names do not match.")}else{print("Columns matched")}
}
match_test()

# de-mean the variables
panel_dat[, names(var_means)] <- panel_dat[, names(var_means)] - var_means[, ]

# calculate time trend and subsample to post-1984
panel_dat <- panel_dat %>% filter(as.numeric(TimePeriod) > 1984)
panel_dat$trend <- NULL
tmp <- data.frame(TimePeriod = sort(unique(panel_dat$TimePeriod)), 
                  trend = seq(1, length(unique(panel_dat$TimePeriod))))
tmp$trend <- tmp$trend - mean(tmp$trend)
panel_dat <- left_join(panel_dat, tmp, by = "TimePeriod")

# one-hot-encoding for factors:
panel_dat$TimePeriod <- as.character(panel_dat$TimePeriod)
panel_dat$TechGroup_No <- as.character(panel_dat$TechGroup_No)
panel_dat$regio_inv <- as.character(panel_dat$regio_inv)
panel_dat <- select(panel_dat, - reg_label, -TechGroup)

library("caret")
dmy <- dummyVars(" ~ .", data = panel_dat)
panel_dat <- data.frame(predict(dmy, newdata = panel_dat))

###################################
####### Estimation with GMM #######
###################################

set_up_formula <- function(controls = "tech_trends"){
  
  if(controls == "time_dummies"){controls <- grep("TimePeriod",x = names(panel_dat), value = TRUE)[-1]}else{
    if(controls == "tech_trends"){controls <- grep("TechGroup",x = names(panel_dat), value = TRUE)[-1]}else{
      controls <- grep("regio_inv",x = names(panel_dat), value = TRUE)[-1]
    }}
  
  gmm_formula <- paste0("onshored_patents ~ exp(b1 * N_inv_nonwestern + b2 * N_inv_anglosaxon ",
                      paste0("+ b", seq(length(controls))+2, "*", controls, "*trend", collapse = " "),
                      ")")
  gmm_formula <- as.formula(gmm_formula)
  return(gmm_formula)
}

gmm_formula <- set_up_formula(controls = "tech_trends")

N_regressors <- 2 + length(controls)
theta_starting <- rep(0, times = N_regressors)
names(theta_starting) <- paste0("b", seq(N_regressors))
instruments <- as.formula(paste0("~imputed_N_inv_nonwestern + imputed_N_inv_anglosaxon + ", 
                                 paste(controls, collapse = " + ")))

gmm_model <- gmm4(gmm_formula, instruments,
                  vcov = "CL", type = "twostep", 
                  vcovOptions=list(cluster=~regio_tech),
                  theta0=theta_starting, data=panel_dat)
summary(gmm_model, sandwich = TRUE, df.adj = TRUE)


# ---------------------------------------
theta_starting <- c(b1 = 0, b2 = 0, b3 = 0)

gmm_model <- gmm4(onshored_patents ~ exp(b1 * N_inv_nonwestern + b2 * N_inv_anglosaxon + b3 * trend),
                  ~ imputed_N_inv_nonwestern + imputed_N_inv_anglosaxon + trend, # instruments
                  vcov = "CL", type = "twostep", 
                  vcovOptions=list(cluster=~regio_tech),
                  theta0=theta_starting, data=panel_dat)
summary(gmm_model, sandwich = TRUE, df.adj = TRUE)
