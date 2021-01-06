#################################################################
# Description:    Script to match non-US companies to their     #
#                 potential US-based affiliates.                #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   06.01.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages ------------------------------------------------------------------
pkgs <- c("tidyverse", "data.table", "countrycode") # packages for data processing:
pkgs <- c(pkgs, "stringr", "stringi", "stringdist") # packages for string distance calculations

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
#setwd("/scicore/home/weder/nigmat01/inventor_migration")

if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################################################
####### Load data and functions for data analysis #######
#########################################################

# FUNCTION TO CLEAN SPECIAL LETTERS
source(paste0(getwd(), "/Code/training_data/clear_chars_function.R"))
filter <- dplyr::filter

#### LIST OF NOTABLE NON-US PATENT APPLICANTS
non_US_firms <- read.csv(paste0(getwd(), "/Data/patent_data/non_US_firms.csv"))
non_US_firms <- non_US_firms %>% filter(country_firm != "United States")
non_US_firms <- non_US_firms %>% mutate(country_firm = trimws(country_firm))
non_US_firms[non_US_firms$country_firm == "Nerlands", "country_firm"] <- "Netherlands"
non_US_firms <- rename(non_US_firms, GEO = country_firm) %>%
        mutate(country_firm = countrycode(GEO, origin = "country.name",
                                          destination = "iso2c"),
               organization = tolower(organization)) %>%
        distinct(organization, .keep_all = TRUE) %>%
        rename(eq_firm = organization) %>% select(-GEO)

# DEFINE STOP WORDS ------------------------------------------------------------
REMOVING_WORDS <- c("pharmaceuticals", " health", "innovation", "laboratories", "healthcare",
                    "care", "medical", "pharma", "america","americas", "chemical", 
                    "machines", "sales", "corporation", "operations", "enterprise",
                    "intellectual", "property", "licensing", "licence", "software",
                    "division", "services", "associates", "diagnostics", "gmbh", "ltd", 
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

# CLEAN SPECIAL CHARACTERS FROM APPLICANT NAMES: -------------------------------
non_US_firms$eq_firm <- gsub("[[:punct:]]", "", non_US_firms$eq_firm)
SPECIAL_CHARS <- gsub(" ", replacement = "", non_US_firms$eq_firm)
SPECIAL_CHARS <- stri_extract_all(str = SPECIAL_CHARS, regex = "[^a-z0-9]")
SPECIAL_CHARS <- unique(unlist(SPECIAL_CHARS))
SPECIAL_CHARS <- SPECIAL_CHARS[is.na(SPECIAL_CHARS) == FALSE]
REPL_VEC <- iconv(SPECIAL_CHARS, to = "ASCII//TRANSLIT")
del_idx <- which(REPL_VEC == "?")
if(length(del_idx) > 0){
  REPL_VEC <- REPL_VEC[- del_idx]
  SPECIAL_CHARS <- SPECIAL_CHARS[- del_idx]
}
non_US_firms$eq_firm <- clear_chars(non_US_firms$eq_firm,
                                         special_chars = SPECIAL_CHARS,
                                         repl_vec = REPL_VEC)
non_US_firms$eq_firm <- unlist(lapply(non_US_firms$eq_firm,
              function(x)removeWords(name = x,
                                     removing_words = REMOVING_WORDS)))
print("Cleaned applicant names")

# add firms' countries of origin
non_US_applicants <- read.csv2(paste0(getwd(), "/Data/patent_data/non_US_firms_to_applicants_encoded.csv"))
non_US_applicants <- non_US_applicants %>% filter(match == 1)
non_US_applicants <- merge(non_US_applicants, non_US_firms, by = "eq_firm", all.x = TRUE) %>% 
        rename(wiki_firm = eq_firm) %>% select(-name_diff, -match, -classified) %>%
        filter(wiki_firm != "")
print("Retrieved countries of origin for notable non-U.S. based firms")

#### PATENT DATASET
df <- readRDS(paste0(datDir, "/pat_dat_all.rds"))
print("Patent data loaded")

############################################################
####### Identify notable non-US firms in patent data #######
############################################################

# find all patents by notable non-US companies
firm_dat <- setDT(df)[organization %in% non_US_applicants$organization, ]
firm_dat <- left_join(non_US_applicants, firm_dat, 
                  by = c("organization", "country_firm"), all.x = TRUE)

# TEST whether all notable non-US firms have been found:
n_not_found <- length(which(!non_US_applicants$organization %in% unique(firm_dat$organization)))
if(n_not_found == 0){print("All firms have been found in the patent data.")}else{
                paste(n_not_found, "firms could not be assigned.")}

# calculate the most frequent IPC class of these firms
Top_IPC_fun <- function(dat){
        
        Top_IPC <- setDT(dat)[, count := .N, by = c("organization", "ipc_main_firm")]
        Top_IPC <- Top_IPC %>% distinct(organization, ipc_main_firm, .keep_all = TRUE)
        Top_IPC <- Top_IPC[count > 0, .SD[order(count, decreasing = TRUE)], by = "organization"]
        Top_IPC <- Top_IPC[, .SD[1:3, ], by = "organization"] %>% select(organization, ipc_main_firm)
        
        firms <- unique(Top_IPC$organization)
        tmp <- lapply(firms, function(x){
                tmp <- data.frame(organization = x, IPC1 = NA, IPC2 = NA, IPC3 = NA)
                ipcs <- Top_IPC[Top_IPC$organization == x, ]$ipc_main_firm
                tmp[tmp$organization == x, 2:4] <- ipcs
                return(tmp)})
        tmp <- bind_rows(tmp)
        
        return(tmp)
}

Top_IPC <- Top_IPC_fun(dat = firm_dat)

# filter to unique firms & add IPC info
firm_dat <- firm_dat %>% distinct(organization, country_firm, .keep_all = TRUE) %>%
        select(organization, country_firm)
firm_dat <- merge(firm_dat, Top_IPC, by = "organization", all.x = TRUE)

# clean firm names
firm_dat$organization_cleaned <- gsub("[[:punct:]]", "", firm_dat$organization)
SPECIAL_CHARS <- gsub(" ", replacement = "", firm_dat$organization_cleaned)
SPECIAL_CHARS <- stri_extract_all(str = SPECIAL_CHARS, regex = "[^a-z0-9]")
SPECIAL_CHARS <- unique(unlist(SPECIAL_CHARS))
SPECIAL_CHARS <- SPECIAL_CHARS[is.na(SPECIAL_CHARS) == FALSE]
REPL_VEC <- iconv(SPECIAL_CHARS, to = "ASCII//TRANSLIT")
del_idx <- which(REPL_VEC == "?")
if(length(del_idx) > 0){
  REPL_VEC <- REPL_VEC[- del_idx]
  SPECIAL_CHARS <- SPECIAL_CHARS[- del_idx]
}
firm_dat$organization_cleaned <- clear_chars(firm_dat$organization_cleaned,
                                             special_chars = SPECIAL_CHARS,
                                             repl_vec = REPL_VEC)
firm_dat$organization_cleaned <- unlist(lapply(firm_dat$organization_cleaned,
                                               function(x)removeWords(name = x,
                                                                      removing_words = REMOVING_WORDS)))
paste(nrow(firm_dat), "non-US based firms ready for affiliate search.")

##############################
####### U.S. based firms #####
##############################

# subset to US_firms with at least 50 patents
US_firms <- setDT(df)[country_firm == "US", ]
US_firms <- US_firms[, count := .N, by = "organization"]
US_firms <- US_firms[count >= 20, ]
paste("Reduced sample to", length(unique(US_firms$organization)),
      "U.S. based firms owning a total of", nrow(US_firms), "patents.")

# calculate the most frequent IPC classes of U.S. based firms
Top_IPC <- Top_IPC_fun(dat = US_firms)
US_firms <- US_firms %>% distinct(organization)
US_firms <- merge(US_firms, Top_IPC, by = "organization", all.x = TRUE)
US_firms <- US_firms[US_firms$organization != "", ]
colnames(US_firms)[2:4] <- paste0(colnames(US_firms)[2:4], "_US")

# clean their organization names from special characters and removing words.
US_firms$organization_cleaned <- gsub("[[:punct:]]", "", US_firms$organization)
SPECIAL_CHARS <- gsub(" ", replacement = "", US_firms$organization_cleaned)
SPECIAL_CHARS <- stri_extract_all(str = SPECIAL_CHARS, regex = "[^a-z0-9]")
SPECIAL_CHARS <- unique(unlist(SPECIAL_CHARS))
SPECIAL_CHARS <- SPECIAL_CHARS[is.na(SPECIAL_CHARS) == FALSE]
REPL_VEC <- iconv(SPECIAL_CHARS, to = "ASCII//TRANSLIT")
del_idx <- which(REPL_VEC == "?")
if(length(del_idx) > 0){
  REPL_VEC <- REPL_VEC[- del_idx]
  SPECIAL_CHARS <- SPECIAL_CHARS[- del_idx]
}
US_firms$organization_cleaned <- clear_chars(US_firms$organization_cleaned,
                                             special_chars = SPECIAL_CHARS,
                                             repl_vec = REPL_VEC)
US_firms$organization_cleaned <- unlist(lapply(US_firms$organization_cleaned,
                                      function(x)removeWords(name = x,
                                                             removing_words = REMOVING_WORDS)))
paste(nrow(US_firms), "US.based firms ready for parent search.")

##############################
####### Affiliate search #####
##############################

#### DISTANCE MEASURES:
# https://www.rdocumentation.org/packages/stringdist/versions/0.9.6.3/topics/stringdist-metrics

firm_dat <- firm_dat %>% rename(pot_parent = organization, pot_parent_country = country_firm,
                                pot_parent_cleaned = organization_cleaned)

matching_fun <- function(firm){
  
  firm_info <- firm_dat[firm_dat$pot_parent_cleaned == firm, ]
  
  # filter U.S. based firms conditional on first two letters (and comparable string length?)
  affiliates <- filter(US_firms,
                       substr(organization_cleaned, 1, 2) == substr(firm, 1,2))
  affiliates <- merge(affiliates, firm_info, all.x = TRUE)
  #affiliates <- filter(affiliates, nchar(organization_cleaned) - nchar(pot_parent) <= 10)
  if(nrow(affiliates) == 0){stop("No match in U.S.")}

  # calculate string distances
  affiliates <- mutate(affiliates,
                       name_diff_lv = stringdist(organization_cleaned, 
                                                 pot_parent_cleaned, method = "lv"),
                       name_diff_osa = stringdist(organization_cleaned, 
                                                  pot_parent_cleaned, method = "osa"),
                       name_diff_dl = stringdist(organization_cleaned, 
                                                 pot_parent_cleaned, method = "dl"),
                       name_diff_cosine = stringdist(organization_cleaned, 
                                                     pot_parent_cleaned, method = "cosine"),
                       name_diff_jw = stringdist(organization_cleaned, 
                                                 pot_parent_cleaned, method = "jw"),
                       name_diff_jac = stringdist(organization_cleaned, 
                                                  pot_parent_cleaned, method = "jaccard")
                       )
  
  NAMES <- names(affiliates)
  
  # filter to relatively good matches
  affiliates <- filter(affiliates, name_diff_lv < 5)
        
  # if no combination satisfies the condition, return an empty data.frame
  if(nrow(affiliates) == 0){
    affiliates <- data.frame(matrix(rep(NA, length(NAMES)), nrow = 1))
    names(affiliates) <- NAMES
    affiliates[, c(names(firm_info))] <- firm_info[,]
    }
  
  return(affiliates)
}

NAMES <- c("organization", "IPC1_US", "IPC2_US", "IPC3_US", "organization_cleaned",
           "pot_parent", "pot_parent_country","IPC1", "IPC2", "IPC3", "name_diff_lv", 
           "name_diff_osa", "name_diff_dl", "name_diff_cosine", "name_diff_jw", "name_diff_jac")
EMPTY_DF <- data.frame(matrix(rep(NA, length(NAMES)), nrow = 1))
names(EMPTY_DF) <- NAMES

res_dat <- lapply(firm_dat$pot_parent_cleaned, function(x){
        
        # return an empty data.frame if there is no match at all
        res <- tryCatch(
                matching_fun(firm = x),
                error = function(e){
                  EMPTY_DF$pot_parent <- x
                  EMPTY_DF
                  })
})

res_dat <- bind_rows(res_dat)
print("Matching of non-U.S. firms and their potential U.S. branches completed.")

N_unmatched <- nrow(res_dat[is.na(res_dat$organization) == TRUE, ])
paste("No matches found for", N_unmatched, "non-U.S. companies")

# finalize dataset
res_dat <- res_dat[is.na(res_dat$organization) == FALSE, ]
res_dat <- mutate(res_dat, match = ifelse(name_diff_osa == 0, 1, NA))
paste("Number of perfect matches:", res_dat %>% subset(match == 1) %>% nrow())

##########################
####### SAVE DATASET #####
##########################

# FULL SAMPLE:
write.csv(x = res_dat, file = paste0(getwd(), "/Data/patent_data/pot_affiliates.csv"))

# RANDOM SAMPLE FOR CREATING A TRAINING DATA
set.seed(06012021)
train_dat <- res_dat[sample(nrow(res_dat), 5000), ]
write.csv2(x = train_dat, file = paste0(getwd(), "/Data/patent_data/pot_affiliates_train.csv"))



