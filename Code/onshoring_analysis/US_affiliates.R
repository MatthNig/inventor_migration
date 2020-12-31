#################################################################
# Description:    Script to match non-US companies to their     #
#                 US-based affiliates.                          #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           30.01.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages ------------------------------------------------------------------
pkgs <- c("tidyverse", "data.table") # packages for data processing:
pkgs <- c(pkgs, "stringr", "stringdist", "RecordLinkage") # packages for string distance calculations

# install if necessary
inst_pkgs <- which(pkgs %in% rownames(installed.packages()) == FALSE)
inst_pkgs <- pkgs[inst_pkgs]
if(length(inst_pkgs) > 0){
        for(p in 1:length(inst_pkgs)){install.packages(inst_pkgs[p])}}

# load packages
for (p in 1:length(pkgs)) {
        p <- pkgs[p]
        library(p, character.only = TRUE)
}
print("All necessary packages installed and loaded")

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

#############################################################################
####### Get all non-US based and US-based companies and their main IPCs #####
#############################################################################
set.seed(30122020)
test <- df[sample(nrow(df), 10000), ]

removing_words <- c("pharmaceuticals", "health", "innovation", "laboratories", "healthcare",
                    "care", "medical", "pharma", " america"," americas", "chemical", 
                    "machines", "sales", "corportion", "operations", "enterprise",
                    "intellectual property", "licensing", " licence", "software",
                    "division", "services", "associates", "diagnostic")
                    
# -> check how I did it with Orbis
# remove special characters ä,ü etc. but also "ag", "a.g.", "s.a.", "ip"

firm_IPC <- function(df, US_based = TRUE){
        
        # subset to US based or non-US based firms
        if(US_based == TRUE){tmp <- setDT(df)[country_firm == "US", ]}else{
                tmp <- setDT(df)[country_firm != "US", ]}
        
        # retrieve their most frequent IPC class
        IPCs <- tmp %>% group_by(organization, ipc_main_firm) %>% 
                summarise(count = n()) %>% group_by(organization) %>% filter(count == max(count) & count > 1)
        
        # with DT instead
        # IPCs <- setDT(tmp)[, count := .N, by = c(organization, ipc_main_firm)]
        # IPCs <- IPCs %>% group_by(organization) %>% filter(count == max(count) & count > 1)
        
        # merge
        tmp <- tmp %>% distinct(organization, .keep_all = TRUE) %>% select(organization, country_firm)
        tmp <- merge(tmp, IPCs, by = "organization", all.x = TRUE) %>% select(-count)
        
        return(tmp)
}

US_based_firms <- firm_IPC(df = df, US_based = TRUE)        
non_US_based_firms <- firm_IPC(df = df, US_based = FALSE)

# for all non-US firms, subset to those with the same first two letters
# and most frequent tech_field

affiliate_search <- function(firm){
        
        # subset US firms to those with the same first two letters
        subset_letters <- substr(firm, 1, 2)
        comp_US_firms <- US_based_firms %>% filter(substr(organization, 1, 2) == subset_letters)
        
        # prepare dataset for comparison
        tmp <- non_US_based_firms %>% filter(organization == firm) %>%
                # select(organization, ipc_main_firm, country_firm) %>%
                rename(pot_parent = organization, pot_parent_IPC = ipc_main_firm,
                       pot_parent_country = country_firm)
        
        comp_US_firms <- cbind(comp_US_firms, tmp)
        
        comp_US_firms <- mutate(comp_US_firms, 
                        name_diff_osa = stringdist(organization, pot_parent, method = "osa"), 
                        name_diff_jaccard = stringdist(organization, pot_parent, method = "jaccard"),
                        name_diff_cosine = stringdist(organization, pot_parent, method = "cosine"), 
                        name_diff_jw = stringdist(organization, pot_parent, method = "jw"))
        
        comp_US_firms <- filter(comp_US_firms,
                                name_diff_osa < 2 &  name_diff_jaccard < 0.5,
                                name_diff_cosine < 0.4 & name_diff_jw < 0.25)

        return(comp_US_firms)
}

test <- affiliate_search(firm = "nestle")
# => das problem hier ist, dass ich so auch U.S. headquarters dem Ausland zuordnen würde..
# das geht also so nicht. evtl. grösste Firmen aus den europäischen und asiatischen Aktienindizes nehmen.
# ansonsten bräuchte ich zusätzliche Kriterien z.B., um zu schauen ob dann die Mehrheit aller Patente nicht in den USA sind


