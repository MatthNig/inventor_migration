#################################################################
# Description:    Script to find non-US companies derived from  #
#                 Wikipedia in patent applicants                #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   05.01.2021                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages ------------------------------------------------------------------
pkgs <- c("tidyverse", "data.table", "countrycode") # packages for data processing:
pkgs <- c(pkgs, "stringr", "stringi","stringdist") # packages for string distance calculations

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
setwd("/scicore/home/weder/nigmat01/inventor_migration")

if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#######################################
############## Load data ##############
#######################################

# FUNCTION TO CLEAN SPECIAL LETTERS
source(paste0(getwd(), "/Code/training_data/clear_chars_function.R"))
filter <- dplyr::filter

#### NON-US FIRMS FROM WIKIPEDIA
non_US_firms <- read.csv(paste0(getwd(), "/Data/patent_data/non_US_firms.csv"))
non_US_firms <- non_US_firms %>% filter(country_firm != "United States")
non_US_firms <- non_US_firms %>% mutate(country_firm = trimws(country_firm))
non_US_firms[non_US_firms$country_firm == "Nerlands", "country_firm"] <- "Netherlands"
non_US_firms <- rename(non_US_firms, GEO = country_firm) %>%
        mutate(country_firm = countrycode(GEO, origin = "country.name",
                                          destination = "iso2c"),
               organization = tolower(organization)) %>%
        distinct(organization, .keep_all = TRUE)

#### PATENT APPLICANT DATA
pat_dat <- readRDS(paste0(datDir, "/pat_dat_all.rds"))

print("All Data loaded")

#####################################################
###### Search for matches in patent applicants ######
#####################################################

# DEFINE STOP WORDS ------------------------------------------------------------
REMOVING_WORDS <- c("pharmaceuticals", " health", "innovation", "laboratories", "healthcare",
                    "care", "medical", "pharma", "america","americas", "chemical", 
                    "machines", "sales", "corporation", "operations", "enterprise",
                    "intellectual", "property", "licensing", "licence", "software",
                    "division", "services", "associates", "diagnostic", "gmbh", "ltd", 
                    "group", "ag", "sa", "a.g.", "s.a.", "spa", "sarl", "kg",
                    "societe", "produits", "telecommunications", "communication",
                    "holding", "engineering", "systems")

# DEFINE FUNCTION TO REMOVE STOPWORDS ------------------------------------------
removeWords <- function(name, removing_words){
        x <- unlist(strsplit(name, split = " "))
        x <- x[!x %in% removing_words]
        x <- paste(x, collapse = " ")
        return(x)
}

# CLEAN SPECIAL CHARACTERS FROM WIKIPEDIA NAMES: -------------------------------
non_US_firms$organization <- gsub("[[:punct:]]", "", non_US_firms$organization)
SPECIAL_CHARS <- gsub(" ", replacement = "", non_US_firms$organization)
SPECIAL_CHARS <- stri_extract_all(str = SPECIAL_CHARS, regex = "[^a-z0-9]")
SPECIAL_CHARS <- unique(unlist(SPECIAL_CHARS))
SPECIAL_CHARS <- SPECIAL_CHARS[is.na(SPECIAL_CHARS) == FALSE]
REPL_VEC <- iconv(SPECIAL_CHARS, to = "ASCII//TRANSLIT")
del_idx <- which(REPL_VEC == "?")
REPL_VEC <- REPL_VEC[- del_idx]
SPECIAL_CHARS <- SPECIAL_CHARS[- del_idx]
non_US_firms$organization <- clear_chars(non_US_firms$organization,
                                         special_chars = SPECIAL_CHARS,
                                         repl_vec = REPL_VEC)
print("cleaned wikipedia firm names")


# CLEAN SPECIAL CHARACTERS FROM PATENT APPLICANTS: -----------------------------
pat_dat$organization_cleaned <- gsub("[[:punct:]]", "", pat_dat$organization) # create new var to keep original names for merging
SPECIAL_CHARS <- gsub(" ", replacement = "", pat_dat$organization_cleaned)
SPECIAL_CHARS <- stri_extract_all(str = SPECIAL_CHARS, regex = "[^a-z0-9]")
SPECIAL_CHARS <- unique(unlist(SPECIAL_CHARS))
SPECIAL_CHARS <- SPECIAL_CHARS[is.na(SPECIAL_CHARS) == FALSE]
REPL_VEC <- iconv(SPECIAL_CHARS, to = "ASCII//TRANSLIT")
del_idx <- which(REPL_VEC == "?")
REPL_VEC <- REPL_VEC[- del_idx]
SPECIAL_CHARS <- SPECIAL_CHARS[- del_idx]
pat_dat$organization_cleaned <- clear_chars(pat_dat$organization_cleaned,
                                         special_chars = SPECIAL_CHARS,
                                         repl_vec = REPL_VEC)
pat_dat$organization_cleaned <- trimws(pat_dat$organization_cleaned)
print("cleaned patent applicants names")

#### SEARCH FOR MATCHES BETWEEN WIKIPEDIA FIRMS AND PATENT APPLICANTS ----------
matching_fun <- function(name, df = non_US_firms){
        
        # subset all data to country of the firm in question
        COUNTRY <- df[which(df$organization == name), "country_firm"]
        applicants <- pat_dat %>% filter(country_firm == COUNTRY) %>% 
                distinct(organization_cleaned, .keep_all = TRUE) %>% 
                select(organization, organization_cleaned)
        
        # clean firm name and subset applicants to those with the same first two letters in their name
        FIRM <- removeWords(name = name, removing_words = REMOVING_WORDS)
        applicants <- filter(applicants, 
                             substr(organization_cleaned, 1, 2) == substr(FIRM, 1,2))
        if(nrow(applicants) == 0){stop("No match")}
        applicants$organization_cleaned <- unlist(lapply(applicants$organization_cleaned, function(x){
                removeWords(name = x, removing_words = REMOVING_WORDS)}))
        
        # calculate string distance
        applicants$eq_firm <- FIRM
        applicants <- mutate(applicants, 
                             name_diff = stringdist(organization_cleaned, 
                                                    eq_firm, method = "lv"))

        # filter to relatively good matches
        applicants <- filter(applicants, name_diff < 2)

        # if no combination satisfies the conditions, return an empty df
        if(nrow(applicants) == 0){applicants = data.frame(organization = NA, 
                                                          organization_cleaned = NA,
                                                          eq_firm = name, name_diff = NA)}
        return(applicants)
        }

#### APPLY FUNCTION TO ALL NON-US FIRMS FROM WIKIPEDIA
# [5939:6174] # firm range for CH
res_dat <- lapply(non_US_firms$organization, function(x){

        # return an empty data.frame if there is no match at all
        res <- tryCatch(
                matching_fun(name = x, df = non_US_firms),
                error = function(e)data.frame(organization = NA,
                                              organization_cleaned = NA,
                                              eq_firm = x, name_diff = NA))
                }
        )
print("Matching of firms and applicants completed.")

## BUILD THE FINAL DATASET -----------------------------------------------------
res_dat <- bind_rows(res_dat)
res_dat <- res_dat[complete.cases(res_dat), ]
#df <- data.frame()

# find the perfect matches for wikipedia firms with multiple matches
perfect_matches <- res_dat %>% group_by(eq_firm) %>% mutate(count = n()) %>%
        filter(count > 1 & name_diff == 0) %>%
        distinct(organization_cleaned, .keep_all = TRUE) %>%
        select(-count) %>% mutate(match = 1)
#df <- rbind(df, perfect_matches)
df <- as.data.frame(perfect_matches)
        
# find perfect matches for all remaining firms
perfect_matches <- res_dat %>% filter(!eq_firm %in% df$eq_firm) %>%
        filter(name_diff == 0) %>% mutate(match = 1)
df <- rbind(df, perfect_matches)

# extract all remaining firms with imperfect matches for manual classification
df <- rbind(df, res_dat %>% filter(!eq_firm %in% df$eq_firm) %>% mutate(match = NA))

if(length(unique(res_dat$eq_firm)) == length(unique(df$eq_firm))){
        print("All wikipedia firms considered. Matching successful")
}

##############################
###### SAVE THE DATASET ######
##############################

write.csv(df, paste0(getwd(), "/Data/patent_data/non_US_firms_to_applicants.csv"))
print("Results saved as 'non_US_firms_to_applicants.csv'. Encode ambigious matches manually and save the results.")
