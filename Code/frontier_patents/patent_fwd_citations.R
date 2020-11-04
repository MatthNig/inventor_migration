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

#########################
####### Load data #######
#########################

#### Load patent information ---------------------------------------------------
#cit_dat <- readRDS(paste0(mainDir1,  "/created data/", "forward_cit_dat.rds"))
pat_dat <- readRDS(paste0(mainDir1,  "/created data/", "pat_dat.rds"))
print("Data on patent citations loaded")

#### Load company information --------------------------------------------------
firm_dat <- readRDS(paste0(mainDir1,  "/created data/", "firm_reg.rds"))
print("Firm data of patent ownership loaded")

#### Load inventor information -------------------------------------------------
inv_dat <- readRDS(paste0(mainDir1,  "/created data/", "inv_reg_CHcommute_adj.rds"))
print("Inventor data of patents loaded")

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
inv_dat <- inv_dat %>% distinct(p_key, name, .keep_all = TRUE) %>% as.data.frame()

# for every inventor_name / p_key combination assign the region, lat and long information to all patents
# ....

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

# Alternative attempts:
# df0 <- full_join(pat_dat, tmp, by = "p_key") # dplyr
# df1 <- setDT(pat_dat, key = "p_key")[setDT(tmp, key = "p_key")] # data.table right join

#### Combine information on inventors with patent and firm characteristic-------
VARS <- colnames(inv_dat)[!names(inv_dat) %in% names(df)]
VARS <- VARS[!VARS %in% c("ipc_main", "pat_off_name")]
tmp <- inv_dat[, c("p_key", VARS)]
change_names <- colnames(tmp)[colnames(tmp) %in% c("Up_reg_code", "lat", "lng")]
change_names <- which(colnames(tmp) %in% change_names)
colnames(tmp)[change_names] <- paste0(colnames(tmp)[change_names], "_inv")
df <- merge(setDT(df, key = "p_key"), setDT(tmp, key = "p_key"), by = "p_key", all = TRUE) # merge.data.table

# Alternative attempts:
#df <- full_join(df, tmp, by = "p_key") # dplyr

tmp <- NULL
print("Patent characteristics combined with firm and inventor information.")

##########################
####### Data analysis ####
##########################

# onshoring_fun_country <- function(df, onshoring_country = "US", world_class_indicator = "world_class_90"){
#         
#         # subset to firms that are not from the onshoring_country and to high-impact patents of this country
#         tmp <- df %>% filter(country_firm != onshoring_country)
#         tmp <- tmp %>% filter_at(vars(starts_with(world_class_indicator)), any_vars(. == 1))
#         # tmp <- df %>% filter(country_firm != "US" & world_class_90 == 1) # old without function
#         
#         # Step 1: For this country, calculate the number of patents by firm and year
#         total_pat_firms <- setDT(tmp)[, .(total_patents = uniqueN(p_key)), by = .(organization, p_year)]
#         total_pat_firms <- setDT(total_pat_firms, key = c("organization", "p_year"))
# 
#         # Step 2: For this country, calculate the number of patents with at least 
#         #         one domestic inventor by firm and year
#         total_pat_onshored <- setDT(tmp)[ctry_inv == onshoring_country, 
#                                    .(total_patents_onshored = uniqueN(p_key)), 
#                                    by = .(organization, p_year)]
#         total_pat_onshored <- setDT(total_pat_onshored, key = c("organization", "p_year"))
#         
#         # Step 3: Combine these two measures and calculate the share
#         tmp <- total_pat_onshored[total_pat_firms]
#         tmp$total_patents_onshored <- unlist(sapply(tmp$total_patents_onshored, 
#                                                     function(x)ifelse(is.na(x) == TRUE, 0, x)))
#         
#         # Step 4: calculate the overall share of onshored patents per country and year:
#         tmp <- tmp %>% group_by(p_year) %>%
#         summarise(total_patents_onshored = sum(total_patents_onshored),
#                   total_patents = sum(total_patents)) %>%
#         mutate(share_onshored = total_patents_onshored / total_patents)
#         
#         # Step 5: subset to 2015
#         tmp <- filter(tmp, p_year <= 2015)
#         
#         return(tmp)
# }
# 
# countries <- c("US", "DE", "GB", "FR", "CH", "CN", "JP")
# plot_dat <- lapply(countries,
#                    function(x)onshoring_fun_country(df = df, 
#                                                     onshoring_country = x, 
#                                                     world_class_indicator = "world_class_90")
#                    )
# for(i in 1:length(countries)){plot_dat[[i]]$country <- countries[i]}

# I could do the same thing for regions or tech-fields

#### Load functions
# (1) identify onshored high-impact patents from foreign firms
source(paste0(getwd(), "/Code/frontier_patents/country_onshoring_fun.R"))
# This function creates a new variable called 'onshored' which indicates if
# a patent has been offshored by foreign firms to a specific country. The 
# assignment of this status is conditional on the number of involved domestic inventors.

# (2) calculate the share of onshored high-impact patents from foreign firms
source(paste0(getwd(), "/Code/frontier_patents/calc_onshoring_share_fun.R"))
# This function returns a dataset which calculates the share of offshored patents
# to a specific country from firms by all foreign countries and year.

#### Get onshoring shares for different onshoring countries -----------------
countries <- c("US", "DE", "GB", "FR", "CH", "CN", "JP")
plot_dat <- lapply(countries, function(x){
        tmp <- country_onshoring_fun(df = df,
                                     onshoring_country = x,
                                     inventor_number = 3,
                                     world_class_indicator = "world_class_90")

        tmp <- calc_onshoring_share_fun(df = tmp)#,
                                        # onshoring_country = x)
        
        tmp <- tmp %>% mutate(country = x) %>% as.data.frame()
        
        return(tmp)
        }
        )

## Plot the shares
plot_dat <- bind_rows(plot_dat)
plot_dat <- filter(plot_dat, p_year <= 2015)
ggplot(plot_dat, aes(x = p_year, y = share_onshored, color = country))+
        geom_line()+geom_point()+ylim(0, 0.15)+
        labs(title = " R&D Onshoring",
             subtitle =  paste(" Share of High-impact patents (Top 10%) from foreign firms \n with at least two domestic inventor by country"),
             x = "Year", y = "Share of foreign owned patents with domestic inventors")


#### Check to which regions and tech_fields the onshoring into the U.S. occurs -----------------
source(paste0(getwd(), "/Code/frontier_patents/region_onshoring_fun.R"))
ctry <- "US"
regions <- c("California", "Massachusetts", "New Jersey", "Michigan", 
             "Texas", "New York")#, "Illinois", "Pennsylvania", "Connecticut",
plot_dat <- region_onshoring_fun(df = country_onshoring_fun(dat, onshoring_country = ctry),
                                 onshoring_country = ctry)
plot_dat <- filter(plot_dat, p_year <= 2015& regio_inv %in% regions) %>% as.data.frame()

ggplot(plot_dat, aes(x = p_year, y = regional_share, color = regio_inv))+
        geom_line()+
        #geom_area()+ #guides(fill = FALSE)
        labs(title = " Regional distribution of onshored patents",
             subtitle =  paste(" Share of High-impact patents (Top 10%) from foreign firms \n with at least two domestic inventor by country"),
             x = "Year", y = "Share of foreign owned patents with domestic inventors")










rm(list=ls())

# Ressources:
# https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/
# https://cloud.r-project.org/web/packages/data.table/vignettes/datatable-intro.html











