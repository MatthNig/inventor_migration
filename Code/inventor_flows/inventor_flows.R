#################################################################
# Description:    Script to evaluate migration flows of patent  #
#                 inventors into the U.S. and EU countries      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           16.10.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# directories  -----------------------------------------------------------------
#mainDir1 <- "/scicore/home/..."

# Load names of patents' inventors --------------------------------------------
#inv_reg <- readRDS(paste0(mainDir1, "/..."))
inv_dat <- readRDS("/scicore/home/weder/nigmat01/Data_inventor_migration/inventor_origin.rds")
print("Data on patent inventors loaded")

####################################################
## Count number of inventors per country and year ##
####################################################

panel_dat <- filter(inv_dat, Ctry_code == "CH")
origin_dat <- panel_dat %>% group_by(p_year, origin) %>% summarise(count = n())
total <- panel_dat %>% group_by(p_year) %>% summarise(total = n())

panel_dat <- merge(origin_dat, total, by = "p_year")
panel_dat$share <- panel_dat$count / panel_dat$total
panel_dat <- filter(panel_dat, p_year <= 2015)

# Domestic Share
ggplot(filter(panel_dat, origin == c("German")), aes(x = p_year, y = share, color = origin))+
        geom_line()

# Foreign Shares
ggplot(#panel_dat, 
       filter(panel_dat, !origin %in% c("German")), 
       aes(x = p_year, y = share, color = origin))+
        geom_line()

