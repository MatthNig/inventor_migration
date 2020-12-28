####################################################################
# Description:    Script to investigate on-/off-shoring of patents #
# Authors:        Matthias Niggli/CIEB UniBasel                    #
# Date:           02.11.2020                                       #
####################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("viridis")

# directories  -----------------------------------------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################################################
####### Load data and functions for data analysis #######
#########################################################

#### DATA
df <- readRDS(paste0(datDir, "/pat_dat_all.rds"))

#### FUNCTIONS
source(paste0(getwd(), "/Code/onshoring_analysis/onshoring_analysis_functions.R"))

# ##################################################
# ####### DOMESTIC PATENT OUTPUT (Rybczynski) ######
# ##################################################
# 
# # differentiate between high-tech and low-tech sectors 
# # e.g. based on the share of high-tech patents
# # then calculate the number of patents in both sectors
# # =>    this would be a proxy for domestic Rybczinsky-effects, that is the high-tech
# #       sector should increase in comparison to the low-tech sector.
# 
# # identify high-impact sectors as those with the highest shares of world_class patents
# winner_loser_fun <- function(df, country){
#         high_impact_pat <- setDT(df)[world_class_90 == 1 & country_firm == country, 
#                                      .(high_impact_pat = .N), 
#                                      by = .(tech_field)]
#         total_pat <- setDT(df)[country_firm == "US", 
#                                .(total_pat = .N), 
#                                by = .(tech_field)]
#         tmp <- merge(high_impact_pat, total_pat, by = "tech_field")
#         tmp$share_high_impact <- tmp$high_impact_pat / tmp$total_pat
#         return(tmp %>% arrange(-share_high_impact))
# }
# high_impact_sectors <- winner_loser_fun(df = df, country = "US")[1:5, "tech_field"]
# 
# # assign frontier-status 
# df <- df %>% mutate(high_impact_sector = ifelse(tech_field %in% high_impact_sectors, 
#                                                 "frontier", "non-frontier"))
# 
# # calculate the number of patents in both sectors
# plot_dat <- setDT(df)[p_year <= 2015 & country_firm == "US", .(N_pat = .N), 
#                              by = .(high_impact_sector, p_year)]
# ggplot(plot_dat, aes(x = p_year, y = log(N_pat), color = high_impact_sector))+
#         geom_line()
# # => kaum Veränderung
# 
# # message an RW: 
# # Schwierig da 1) sehr ungenau identifizierbar. Shares an high-impact patents sind bei allen
# # tech-sectors ähnlich hoch, da dies endogen ist. 
# # 2) macht auch nicht unbedingt Sinn, denn diese Sektoren machen nicht
# # "entweder oder" sondern meist sowohl gute wie auch schlechte Patente.

#####################################
############## ONSHORING ############
#####################################

#### Get onshoring shares for different onshoring countries -----------------

# choose countries
onshoring_countries <- c("US", "DE", "GB", "FR", "CH", "CN")

# indicate minimum number of domestic inventors
INVENTOR_NUMBER <- 1 

# indicate whether only onshoring of world class patents should be considered
WORLD_CLASS_INDICATOR  <- FALSE 
#WORLD_CLASS_INDICATOR <- "world_class_90"

plot_dat <- lapply(onshoring_countries, function(x){
        
        # identify offshored patents to country x (= inventor in x, but firm not in x, no collaborations)
        tmp <- country_onshoring(df = df,
                                 onshoring_country = x, 
                                 collaboration = FALSE, # indicate if collaborations with domestic firms should be excluded
                                 inventor_number = INVENTOR_NUMBER,
                                 world_class_indicator = WORLD_CLASS_INDICATOR)
        
        # calculate offshoring intensity to x among all patents by foreign firms per year
        tmp <- calc_onshoring_share(df = tmp)
        
        # indicate the offshoring destination country and return the data
        tmp <- tmp %>% mutate(country = x) %>% as.data.frame()
        return(tmp)
        }
        )

## Plot the offshoring intensities
plot_dat <- bind_rows(plot_dat)
plot_dat <- filter(plot_dat, p_year <= 2015)
ggplot(plot_dat, aes(x = p_year, y = share_onshored, color = country))+
        geom_line()+geom_point()+ylim(0, 0.1)+
        labs(title = " R&D offshoring from foreign firms to selected countries",
             subtitle =  " Offshored patents by foreign firms (in percent of total patents by foreign firms)",
             x = "Year", y = "Offshoring intensity")

#### Check for which tech_fields the onshoring into the U.S. occurs -----------------
COUNTRY <- "US"
TECHFIELDS <- c(4, 5, 6, 8, 13:16, 24)
TECHFIELDS <- seq(1, 34)

plot_dat <- techfield_onshoring_shares(df = country_onshoring(df, onshoring_country = COUNTRY))
plot_dat <- filter(plot_dat, p_year <= 2015 & tech_field %in% TECHFIELDS & 
                           total_patents >= 30) %>% 
        mutate(tech_field = as.factor(as.character(tech_field))) %>%
        as.data.frame()

ggplot(plot_dat, aes(x = p_year, y = share_onshored))+#, color = tech_field))+
        facet_wrap(.~tech_field)+geom_line(aes(color = tech_field))+scale_color_hue(guide = "none")+
        labs(title = paste(" R&D offshoring from foreign firms to", COUNTRY, "by technological fields"),
             subtitle = " Offshoring intensity to US by foreign firms and technological field",
             x = "Year", y = "Offshoring intensity")

#### Check to which regions the onshoring into the U.S. occurs -----------------
COUNTRY <- "US"
REGIONS <- c("California", "Massachusetts", "New Jersey", "Michigan", 
             "Texas", "New York", "Illinois", "Pennsylvania", "Connecticut")
plot_dat <- region_onshoring_shares(df = country_onshoring(df, onshoring_country = COUNTRY),
                                 onshoring_country = COUNTRY)
plot_dat <- filter(plot_dat, p_year <= 2015 & regio_inv %in% REGIONS) %>% as.data.frame()
ma_fun <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
plot_dat_ma <- plot_dat %>% group_by(regio_inv) %>% arrange(p_year) %>%
        mutate(onshored_patents = ma_fun(onshored_patents),
               total_onshored_patents = ma_fun(total_onshored_patents),
               regional_share = onshored_patents / total_onshored_patents)
ggplot(plot_dat_ma, aes(x = p_year, y = regional_share, color = regio_inv))+
        geom_line()+
        labs(title = " R&D Onshoring by region",
             subtitle =  paste(" Share among all onshored patents to the U.S. by region"),
             x = "Year", y = "Share among all onshored patents to the U.S.")




rm(list=ls())

# Ressources:
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html











