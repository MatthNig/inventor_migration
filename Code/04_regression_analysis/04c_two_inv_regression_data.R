##################################################################
# Description:  Raise minimum number of US based inventors.      #
# Authors:      Matthias Niggli/CIEB UniBasel                    #
# Last revised: 20.05.2021                                       #
##################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

# directories  -----------------------------------------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
mainDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#########################################################
####### Load data and functions for data analysis #######
#########################################################

#### on-/off-shoring of patents
df <- readRDS(paste0(datDir, "/pat_dat_all_final.rds"))

#### baseline regression data
reg_dat <- read.csv(paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"))


##############################################################################################
## Patents offshored to US subsidiaries of non-US companies by technology area and State #####
##############################################################################################

country_onshoring <- function(df,
                              onshoring_country = "US",
                              collaboration = FALSE,
                              triadic_only = TRUE,
                              inventor_number = 1,
                              world_class_indicator = FALSE){
        
        # Step 1: check if collaborations with US firms should be excluded and
        # if so drop all entries from patents where a US firms was involved
        if(collaboration == FALSE){
                tmp <- setDT(df)[country_firm_adj == onshoring_country, p_key]
                tmp <- unique(tmp)
                tmp <- setDT(df)[!p_key %in% tmp, ]}else{tmp <- df}
        
        # Step 2: check if non-triadic patents should be exluded
        if(triadic_only == TRUE){
                tmp <- tmp[tri_pat_fam == 1, ]
        }
        
        # Step 3: Subset to entries from non-U.S. patents
        tmp <- tmp %>% filter(country_firm_adj != onshoring_country)
        
        # Step 4: check if only world class patents should be considered and,
        # if so, subset accordingly.
        if(world_class_indicator != FALSE){
                tmp <- tmp %>% filter_at(vars(starts_with(world_class_indicator)), any_vars(. == 1))
        }
        
        # Step 3: Assign 'onshoring'-status = 1 to all entires from patents with at least
        # (N = inventor_number) inventors located in the USA.
        onshoring_p_keys <- setDT(tmp)[ctry_inv == onshoring_country,
                                       .(onshored = ifelse(.N >= inventor_number, 1, 0)),
                                       by = .(p_key)]
        setkey(onshoring_p_keys, p_key)
        tmp <- onshoring_p_keys[setDT(tmp, key = "p_key")]
        tmp[is.na(tmp$onshored) == TRUE, "onshored"] <- 0
        
        return(tmp)
}

# (2) identify all patents that have been offshored to the U.S. by foreign firms:
MIN_INV <- 2 # raise minimum number of inventors to two
dat <- country_onshoring(df = df, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = MIN_INV, 
                         world_class_indicator = FALSE)

# (3) Define technological groups and assign patents to them:
assign_TechGroup <- function(df){
        
        df <- mutate(df, TechGroup = case_when(
                tech_field %in% c(1) ~ "Electrical Machinery",
                tech_field %in% c(2) ~ "Audiovisual Technologies",
                tech_field %in% c(3:5, 7) ~ "Information & Communication Technology",
                tech_field %in% c(6) ~ "Computer Science",
                tech_field %in% c(8) ~ "Semiconducturs",
                tech_field %in% c(9:12) ~ "Instruments",
                tech_field %in% c(13) ~ "Medical Technology",
                tech_field %in% c(14:16) ~ "Pharmaceuticals & Biotechnology",
                tech_field %in% c(18:23) ~ "Chemistry & Materials",
                tech_field %in% c(25, 26, 28, 29, 31) ~ "Machines & Mechanical Engineering",
                tech_field %in% c(24, 27, 30) ~ "Engines, Turbines, Thermal & Environmental Technologies",
                tech_field %in% c(32) ~ "Transport",
                tech_field %in% c(33:35) ~ "Consumer Goods & Civil Engineering"
        )
        )
}

dat <- assign_TechGroup(df = dat)

# (4) Define 3-year periods and assign patents to them:
assign_TimePeriod <- function(df){
        
        df <- mutate(df, TimePeriod = case_when(
                p_year %in% c(1978:1984) ~ "1980",
                p_year %in% c(1985:1988) ~ "1988",
                p_year %in% c(1989:1991) ~ "1991",
                p_year %in% c(1992:1994) ~ "1994",
                p_year %in% c(1995:1997) ~ "1997",
                p_year %in% c(1998:2000) ~ "2000",
                p_year %in% c(2001:2003) ~ "2003",
                p_year %in% c(2004:2006) ~ "2006",
                p_year %in% c(2007:2009) ~ "2009",
                p_year %in% c(2010:2012) ~ "2012",
                p_year %in% c(2013:2015) ~ "2015")
        )
}
dat <- assign_TimePeriod(df = dat)

#### evaluate
paste(nrow(dat), "patents used for analysis") 
# N = 13'268'547  
paste(nrow(dat[dat$p_year > 1984 & dat$p_year < 2016, ]), 
      "entries of patents between 1985 and 2015 used for analysis") 
# N = 11'892'539
paste(nrow(dat[dat$onshored == 1 & dat$ctry_inv == "US", ]),
      "entries stated on onshored patents to the U.S") 
# N = 554'223

# (5) calculate the number of unique offshored patents to the U.S. by TechGoup, Period and State:
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, TimePeriod)]
        
        return(onshored_pat)
}

dat <- onshoring_TechState(df = dat)

# (6) Summarize:
tmp <- dat %>% group_by(regio_inv) %>% 
        summarize(onshored_patents = sum(onshored_patents)) %>%
        mutate(share = onshored_patents / sum(onshored_patents)) %>%
        arrange(-onshored_patents)
paste0(tmp[is.na(tmp$regio_inv), ]$onshored_patents, 
       " onshored patents (", round(tmp[is.na(tmp$regio_inv), ]$share,3)*100,
       "%) had missing technology field or state information.") 
# N = 78'253  (37.8%)
paste(sum(tmp[is.na(tmp$regio_inv) == FALSE, ]$onshored_patents), 
      "onshored patents assigned to technology-state pairs.") 
# N = 128'863
tmp <- NULL

# (7) Discard NA's
dat <- dat %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
paste("Data on onshored patents prepared.", 
      sum(dat$onshored_patents), 
      "onshored patents used for analysis") 
# N = 114'749 (N = 165'532 in baseline). Difference = 50'783

##########################################################
####### Add to robustness check regression dataset #######
##########################################################

# (8) merge with inventor information
test <- names(reg_dat)
patent_dat <- patent_dat %>% 
        rename(onshored_patents_twoinv = onshored_patents) %>%
        select(regio_inv, TechGroup, TimePeriod, onshored_patents_twoinv)

patent_dat <- merge(reg_dat, patent_dat, by = c("regio_inv", "TechGroup", "TimePeriod"), all.x = TRUE)
if(sum(test %in% names(patent_dat)) != length(test)){warning("Not all variables present in regressiond data")}else{
        print("Regression data ready.")}

# (9) assign 0 number of patents to regio_tech's where no onshored patents could be found
patent_dat <- mutate(patent_dat, 
                     onshored_patents_twoinv = ifelse(is.na(onshored_patents_twoinv), 
                                                      0, onshored_patents_twoinv)
                     )

# test
if(identical(reg_dat, patent_dat[, !names(patent_dat) %in% "onshored_patents_twoinv"])){
        print("Robustness check dataset updated.")}else{warning("Loss of information. Do not update dataset")}

###################################################################
####### Update and save the dataset for regression analysis #######
###################################################################
write.csv(patent_dat, paste0(getwd(), "/Data/regression_data/regression_data_robustness_checks.csv"),
          row.names = FALSE)
print("Updated and saved 'regression_data_robustness_checks.csv'")
