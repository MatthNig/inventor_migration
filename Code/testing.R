# examine the function step by step:
onshoring_country = "US"
collaboration = FALSE
triadic_only = FALSE
inventor_number = 1
world_class_indicator = FALSE
# ---------------
# Step 1: check if collaborations with domestic firms should be excluded and
# if so drop all patents where a domestic firms was involved
if(collaboration == FALSE){
        tmp <- setDT(dat_adj)[country_firm == onshoring_country, p_key]
        tmp <- unique(tmp)
        tmp <- setDT(dat_adj)[!p_key %in% tmp, ]}else{tmp <- dat_adj}

# Step 2: check if non-triadic patents should be exluded
if(triadic_only == TRUE){
        tmp <- tmp[tri_pat_fam == 1, ]
}
# ---------------------
## as before....
tmp <- tmp %>% filter(country_firm_adj != onshoring_country)
        
# Step 2: check if only world class patents should be considered and,
# if so, subset accordingly.
if(world_class_indicator != FALSE){
        tmp <- tmp %>% filter_at(vars(starts_with(world_class_indicator)), any_vars(. == 1))
}

# Step 3: Assign 'onshoring'-status = 1 to all patents with at least 
# (N = inventor_number) inventors located in the onshoring country.
onshoring_p_keys <- setDT(tmp)[ctry_inv == onshoring_country,
                               .(onshored = ifelse(.N >= inventor_number, 1, 0)), 
                               by = .(p_key)]
setkey(onshoring_p_keys, p_key)
tmp <- onshoring_p_keys[setDT(tmp, key = "p_key")]
tmp[is.na(tmp$onshored) == TRUE, "onshored"] <- 0



# evaluate:
nrow(onshoring_p_keys)
nrow(tmp[tmp$onshored == 1, ])
paste("overall size of the df:", nrow(tmp))


# check if it is the same ---------------------------------
datDir <- "/scicore/home/weder/nigmat01/Data"
pat_dat <- readRDS(paste0(datDir, "/pat_dat_all_final.rds"))

# (1) without adjustment
dat_non_adj <- country_onshoring(df = pat_dat, onshoring_country = "US", collaboration = FALSE,
                         triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)

# with adjustment:
#dat_adj <- pat_dat %>% select(-country_firm) %>% rename(country_firm = country_firm_adj)
dat_adj <- pat_dat %>% select(-country_firm)
dat_adj <- rename(dat_adj, country_firm = country_firm_adj)
identical(pat_dat$country_firm_adj, dat_adj$country_firm)
dat_adj <- country_onshoring(df = pat_dat, onshoring_country = "US", collaboration = FALSE,
                             triadic_only = FALSE, inventor_number = 1, world_class_indicator = FALSE)
nrow(tmp) == nrow(dat_adj)




country_onshoring <- function(df,
                              onshoring_country = "US",
                              collaboration = FALSE,
                              triadic_only = TRUE,
                              inventor_number = 1,
                              world_class_indicator = FALSE){
        
        # ---------------
        # Step 1: check if collaborations with domestic firms should be excluded and
        # if so drop all patents where a domestic firms was involved
        if(collaboration == FALSE){
                tmp <- setDT(df)[country_firm_adj == onshoring_country, p_key]
                tmp <- unique(tmp)
                tmp <- setDT(df)[!p_key %in% tmp, ]}else{tmp <- df}
        
        # Step 2: check if non-triadic patents should be exluded
        if(triadic_only == TRUE){
                tmp <- tmp[tri_pat_fam == 1, ]
        }
        
        # ---------------------
        
        ## as before....
        tmp <- tmp %>% filter(country_firm_adj != onshoring_country)
        
        # Step 2: check if only world class patents should be considered and,
        # if so, subset accordingly.
        if(world_class_indicator != FALSE){
                tmp <- tmp %>% filter_at(vars(starts_with(world_class_indicator)), any_vars(. == 1))
        }
        
        # Step 3: Assign 'onshoring'-status = 1 to all patents with at least 
        # (N = inventor_number) inventors located in the onshoring country.
        onshoring_p_keys <- setDT(tmp)[ctry_inv == onshoring_country,
                                       .(onshored = ifelse(.N >= inventor_number, 1, 0)), 
                                       by = .(p_key)]
        setkey(onshoring_p_keys, p_key)
        tmp <- onshoring_p_keys[setDT(tmp, key = "p_key")]
        tmp[is.na(tmp$onshored) == TRUE, "onshored"] <- 0
        
        return(tmp)
}




