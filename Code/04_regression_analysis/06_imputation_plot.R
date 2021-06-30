##################################################################
# Description:  Script to generate correlation plot for imputed  #
#               and observed prevalence of non-western inventors #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 30.06.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for plotting: ------------------------------------------------
library("tidyverse")

# directories  -----------------------------------------------------------------
setwd("/scicore/home/weder/nigmat01/inventor_migration")
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
  print("Working directory corresponds to repository directory")}else{
    print("Make sure your working directory is the repository directory.")}

######################################################
####### Load data used in baseline regressions #######
######################################################

panel_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_baseline.csv"))
MIN_PERIOD <- 1984
panel_dat <- panel_dat %>% 
  filter(is.na(N_inv_nonwestern) == FALSE &
           TimePeriod > MIN_PERIOD)
# only use observations with at least T_min observations
T_min <- 2
keep_regiotech <- panel_dat %>% 
  group_by(regio_tech) %>% 
  summarise(count = n()) %>% 
  filter(count >= T_min)
keep_regiotech <- keep_regiotech$regio_tech
panel_dat <- panel_dat[panel_dat$regio_tech %in% keep_regiotech, ]
panel_dat <- panel_dat %>% select(N_inv_nonwestern, imputed_N_inv_nonwestern) %>% na.omit()
paste("Panel dataset with", nrow(panel_dat), "observations ready for regression analysis.")

#################################
####### Generate the plot #######
#################################

ggplot(panel_dat, aes(x = log(N_inv_nonwestern), y = log(imputed_N_inv_nonwestern)))+
  geom_point(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(x = "Prevalence of Non-Western Inventors (log-scale)", 
       y = "Imputed Prevalence of Non-Western Inventors (log-scale)")+
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
        legend.position = "bottom", legend.direction = "horizontal",
        axis.line = element_line(),
        axis.title = element_text(face="bold",size=10))
ggsave("/scicore/home/weder/nigmat01/imputation_plot.png")





