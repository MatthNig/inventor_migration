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
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}
#setwd(...)

# Load names of patents' inventors --------------------------------------------
#inv_reg <- readRDS(paste0(mainDir1, "/..."))
inv_dat <- readRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds")) # random subset of 250k inventors
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
        geom_line()+ylim(0,0.8)

# Foreign Shares
ggplot(#panel_dat, 
       filter(panel_dat, origin %in% c("China", "India", "HispanicLatinAmerica",
                                       "Korea", "Japan", "Italian")), 
       aes(x = p_year, y = share, color = origin))+
        geom_line()+ ylim(0, 0.15)
View(inv_dat %>% filter(Ctry_code == "CH" & origin == "HispanicLatinAmerica") %>% select(name, origin))


# HispanicLatinAmerica seems to be badly identified with respect to Germans.
# maybe I could do the following on my model: take the predited classes only if it is reasonably save
# i.e. for example minimum probability & minimum distance to the second largest prediction.
# if this is not fullfilled, assign NA