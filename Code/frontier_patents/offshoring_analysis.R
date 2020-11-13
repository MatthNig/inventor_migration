####################################################################
# Description:    Script to (1) determine the number of patents'   #
#                 forward citations (2) Classification of frontier #
#                 patents based on these received citations.       #
# Authors:        Matthias Niggli/CIEB UniBasel                    #
# Date:           02.11.2020                                       #
####################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}
#setwd(...)

################################################
####### Load functions for data analysis #######
################################################

source(paste0(getwd(), "/Code/frontier_patents/patent_analysis_functions.R"))

#########################
####### Load data #######
#########################

#### Load patent information ---------------------------------------------------
pat_dat <- readRDS(paste0(mainDir1,  "/created data/", "pat_dat.rds"))
print("Data on patent citations loaded")

#### Load company information --------------------------------------------------
firm_dat <- readRDS(paste0(mainDir1,  "/created data/", "firm_reg.rds"))
print("Firm data of patent ownership loaded")

#### Load inventor information -------------------------------------------------
inv_dat <- readRDS(paste0(mainDir1,  "/created data/", "inv_reg_CHcommute_adj.rds"))
print("Inventor data of patents loaded")

print("All data loaded")

#################################
####### Data processing #########
#################################

#### Patent information: -------------------------------------------------------

# Problem:
# Sometimes there several equivalent patents per p_key. E.g. the same invention 
# is protected by a US and European patent. In these cases, use the USPTO patents 
# with the most 5-year citations as the reference patent for this p_key.

# order by patent office and citations and choose the last obs per p_key:
tmp <- setDT(pat_dat)
keep_idx <- tmp[order(pat_off, fwd_cits5), .I[.N], keyby = p_key]$V1 
pat_dat <- pat_dat[keep_idx, ]
paste("Obtained patents information of", nrow(pat_dat), "different patents cleaned for equivalents.")
tmp <- NULL
keep_idx <- NULL

#### Inventor information: -----------------------------------------------------

# Problem:
# Sometimes there several equivalent patents per p_key and inventor. 
# E.g. the same invention is protected by a US and European patent. 
# To omit these duplicated patent-inventor pairs, only take one combination per p_key
# However, on EP patents there is information on regional location of inventors but not
# on coordinates. For USPTO its the opposite. Thus, before omitting duplicated patents
# assign the region, lat and long information to all observations per every inventor_name-p_key combination

# ## !!!! not run: inefficent... p_key time name is way too many observations..
# # idea: first identify the NAs, then search for the same inventor/p-key combination
# # only then do the computations
# assign_geo_info <- function(df, vars){
#         
#         # convert to data.table and keep row-indexes
#         df <- setDT(df, key = c("p_key", "name"))[, idx := rownames(df)]
# 
#         # filter to inventors where one patent equivalent has geographical 
#         # information and the other does not.
#         geo_info <- df[, .SD[.N > 1], .SDcols = c(vars, "idx"), by = .(p_key, name)]
#         # NA_obs <- geo_info[is.na(geo_info$lat), c("p_key", "name", "idx")]
#         NA_obs <- geo_info %>% 
#                 filter_at(vars(vars[1]), all_vars(is.na(.))) %>%
#                 select(p_key, name, idx)
#         geo_info <- geo_info[complete.cases(geo_info),]
#         
#         # assign the information from the one patent to the other
#         # and replace observations in the original data
#         NA_obs <- setDT(geo_info %>% select(-idx), key =c("p_key", "name"))[NA_obs]
#         NA_obs$idx <- as.numeric(NA_obs$idx)
#         NA_obs <- NA_obs[complete.cases(NA_obs), ] %>% as.data.frame()
#         
#         # return as a data.frame
#         df <- as.data.frame(df)
#         df[NA_obs$idx, c(vars)] <- NA_obs[, c(vars)]
#         
#         return(df)
# }
# 
# # Assign geo_information on all equivalent patents
# inv_dat <- assign_geo_info(df = inv_dat, vars = c("lat", "lng"))
# inv_dat <- assign_geo_info(df = inv_dat, vars = "regio_inv")

# Subset to unique patents
inv_dat <- inv_dat %>%
        distinct(p_key, name, .keep_all = TRUE) %>% as.data.frame()

#### firm information -----------------------------------------------------
# Problem:
# Sometimes there several equivalent patents per p_key and firm 
# E.g. the same invention is protected by a US and European patent. 
# To omit these duplicated patent-firm pairs, only take one combination per p_key
firm_dat <- firm_dat %>% distinct(p_key, organization, .keep_all = TRUE) %>% as.data.frame()

print("All data processing completed")

#################################
####### Data merging ############
#################################

#### Combine information on firms with patent characteristics ------------------
VARS <- colnames(firm_dat)[!names(firm_dat) %in% names(pat_dat)]
tmp <- firm_dat[, c("p_key", VARS)]
colnames(tmp)[-c(1:2)] <- paste0(colnames(tmp)[-c(1:2)], "_firm")
df <- merge(setDT(pat_dat, key = "p_key"), setDT(tmp, key = "p_key"), by = "p_key", all = TRUE) # merge.data.table
tmp <- NULL

#### Combine information on inventors with patent and firm characteristic-------
VARS <- colnames(inv_dat)[!names(inv_dat) %in% names(df)]
VARS <- VARS[!VARS %in% c("ipc_main", "pat_off_name")]
tmp <- inv_dat[, c("p_key", VARS)]
change_names <- colnames(tmp)[colnames(tmp) %in% c("Up_reg_code", "lat", "lng")]
change_names <- which(colnames(tmp) %in% change_names)
colnames(tmp)[change_names] <- paste0(colnames(tmp)[change_names], "_inv")
df <- merge(setDT(df, key = "p_key"), setDT(tmp, key = "p_key"), by = "p_key", all = TRUE) # merge.data.table
tmp <- NULL
print("Patent characteristics combined with firm and inventor information.")

#####################################
####### DOMESTIC: Data analysis #####
#####################################

# differentiate between high-tech and low-tech sectors 
# e.g. based on the share of high-tech patents
# then calculate the number of patents in both sectors
# =>    this would be a proxy for domestic Rybczinsky-effects, that is the high-tech
#       sector should increase in comparison to the low-tech sector.

# identify high-impact sectors as those with the highest shares of world_class patents
winner_loser_fun <- function(df, country){
        high_impact_pat <- setDT(df)[world_class_90 == 1 & country_firm == country, 
                                     .(high_impact_pat = .N), 
                                     by = .(tech_field)]
        total_pat <- setDT(df)[country_firm == "US", 
                               .(total_pat = .N), 
                               by = .(tech_field)]
        tmp <- merge(high_impact_pat, total_pat, by = "tech_field")
        tmp$share_high_impact <- tmp$high_impact_pat / tmp$total_pat
        return(tmp %>% arrange(-share_high_impact))
}
high_impact_sectors <- winner_loser_fun(df = df, country = "US")[1:5, "tech_field"]

# assign frontier-status 
df <- df %>% mutate(high_impact_sector = ifelse(tech_field %in% high_impact_sectors, 
                                                "frontier", "non-frontier"))

# calculate the number of patents in both sectors
plot_dat <- setDT(df)[p_year <= 2015 & country_firm == "US", .(N_pat = .N), 
                             by = .(high_impact_sector, p_year)]
ggplot(plot_dat, aes(x = p_year, y = log(N_pat), color = high_impact_sector))+
        geom_line()
# => kaum Veränderung

# @RW: schwierig da 1) sehr ungenau identifizierbar. Shares an high-impact sind bei allen
# tech-sectors sehr hoch. 2) macht auch nicht unbedingt Sinn, denn diese Sektoren machen nicht
# "entweder oder" sondern meist sowohl gute wie auch schlechte Patente.

#####################################
####### ONSHORING: Data analysis ####
#####################################

#### Get onshoring shares for different onshoring countries -----------------
onshoring_countries <- c("US", "DE", "GB", "FR", "CH", "CN", "JP")
INVENTOR_NUMBER <- 1
WORLD_CLASS_INDICATOR  <- FALSE
plot_dat <- lapply(onshoring_countries, function(x){
        
        # identify onshored patents for country x (= inventor in x, but firm not in x)
        tmp <- country_onshoring(df = df,
                                 onshoring_country = x,
                                 inventor_number = INVENTOR_NUMBER,
                                 world_class_indicator = WORLD_CLASS_INDICATOR)
        
        # calculate their share on all patents owned by foreign firms
        tmp <- calc_onshoring_share(df = tmp)
        
        # indicate the onshoring country and return the data
        tmp <- tmp %>% mutate(country = x) %>% as.data.frame()
        return(tmp)
        }
        )

## Plot the shares
plot_dat <- bind_rows(plot_dat)
plot_dat <- filter(plot_dat, p_year <= 2015)
ggplot(plot_dat, aes(x = p_year, y = share_onshored, color = country))+
        geom_line()+geom_point()+ylim(0, 0.15)+
        labs(title = " R&D offshoring intensity to selected countries",
             subtitle =  " Share of offshored patents to selected countries among \n all patents owned by foreign firms",
             x = "Year", y = "Offshoring intensity")

#### Check from which tech_fields the onshoring into the U.S. occurs -----------------
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
        labs(title = paste(" R&D offshoring intensity to", COUNTRY, "technological fields"),
             subtitle = " Share of offshored patents among \n all patents owned by foreign firms by technological field",
             x = "Year", y = "Share of foreign owned patents with domestic inventors")

#### Check to which regions the onshoring into the U.S. occurs -----------------
COUNTRY <- "US"
REGIONS <- c("California", "Massachusetts", "New Jersey", "Michigan", 
             "Texas", "New York")#, "Illinois", "Pennsylvania", "Connecticut",
plot_dat <- region_onshoring_shares(df = country_onshoring(df, onshoring_country = COUNTRY),
                                 onshoring_country = ctry)
plot_dat <- filter(plot_dat, p_year <= 2015 & regio_inv %in% REGIONS) %>% as.data.frame()
ggplot(plot_dat, aes(x = p_year, y = regional_share, color = regio_inv))+
        geom_line()+
        labs(title = " R&D Onshoring by region",
             subtitle =  paste(" Share among all onshored patents to the U.S. by region"),
             x = "Year", y = "Share among all onshored patents to the U.S.")





rm(list=ls())

# Ressources:
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html











