####################################################################
# Description:    Functions for analyzing onshoring of patents     #
# Authors:        Matthias Niggli/CIEB UniBasel                    #
# Date:           02.11.2020                                       #
####################################################################

#### COUNTRY_ONSHORING: -------------------------------------------------------

# This function creates a new variable called 'onshored' which indicates if
# a patent has been offshored by foreign firms to a specific country. The 
# assignment of this status is conditional on the number of involved domestic inventors.
# Furthermore, there is the possibility to only consider high-impcat patents.

country_onshoring <- function(df,
                              onshoring_country = "US",
                              collaboration = FALSE,
                              inventor_number = 1,
                              world_class_indicator = FALSE){
        
        # ---------------
        # Step 1: check if collaborations with domestic firms should be excluded and
        # if so drop all patents where a domestic firms was involved
        if(collaboration == FALSE){
                tmp <- setDT(df)[country_firm == onshoring_country, p_key]
                tmp <- unique(tmp)
                tmp <- setDT(df)[!p_key %in% tmp, ]}else{tmp <- df}
        # ---------------------
        
        ## as before....
        tmp <- tmp %>% filter(country_firm != onshoring_country)
        
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

#### CALC_ONSHORING_SHARE: -------------------------------------------------------

# This function calculates the share of patents that were offshored by foreign
# firms to a specific country. It also states the total number of patents owend
# by these firms and the total number of patents that they have offshored to the onshoring country.

calc_onshoring_share <- function(df){

        # Step 1: Calculate the total number of patents by foreign firms and year
        total_pat <- setDT(df)[, .(total_patents = uniqueN(p_key)),
                               by = .(p_year)]
        
        # Step 2: Calcluate the number of offshored patents by foreign firms
        onshored_pat <- setDT(df)[onshored == 1,
                                  .(total_patents_onshored = uniqueN(p_key)),
                                  by = .(p_year)]
        
        total_pat <- merge(onshored_pat, total_pat, by = "p_year")
        total_pat$share_onshored <- total_pat$total_patents_onshored / total_pat$total_patents
        
        return(total_pat)
}

#### TECHFIELD_ONSHORING_SHARES: -------------------------------------------------------

# This function calculates the share of patents that were offshored by foreign
# firms to a specific country for a given technological field. 
# It also states the total number of patents owned by foreign firms and 
# the total number of patents that they have offshored to the onshoring country
# by technological field.

techfield_onshoring_shares <- function(df){
        
        # Step 1:Calculate the total number of patents by foreign firms, year and techfield
        total_pat <- setDT(df)[, .(total_patents = uniqueN(p_key)),
                               by = .(tech_field, p_year)]
        
        # Step 2: Calculate the total number of on-shored patents by foreign firms, year and techfield
        onshored_pat <- setDT(df)[onshored == 1,
                                  .(total_patents_onshored = uniqueN(p_key)),
                                  by = .(tech_field, p_year)]
        
        total_pat <- merge(onshored_pat, total_pat, by = c("tech_field", "p_year"))
        total_pat$share_onshored <- total_pat$total_patents_onshored / total_pat$total_patents
        
        return(total_pat)
}


#### TECHFIELD_ONSHORING_SHARES: -------------------------------------------------------

# This function calculates the number of onshored patents to a specific country. 
# It then calculates how these patents are distributed regionally in this country.
# It returns the regional shares of onshored patents for a given country.

region_onshoring_shares <- function(df, onshoring_country = "US"){
        
        onshored_inv_region <- setDT(df)[onshored == 1 & ctry_inv == onshoring_country, 
                                         .(onshored_patents = uniqueN(p_key)),
                                         by = .(regio_inv, p_year)]
        
        onshored_inv_region <- onshored_inv_region %>% group_by(p_year) %>% 
                mutate(total_onshored_patents = sum(onshored_patents),
                       regional_share = onshored_patents / total_onshored_patents)
        
        return(onshored_inv_region)
}
