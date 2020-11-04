#################################################
# Description: Function to identify high-impact #
#              patents owned by foreign firms   #
#              but co-invented by domestic      # 
#              residents.                       #
# Authors:     Matthias Niggli/CIEB UniBasel    #
# Date:        02.11.2020                       #
#################################################

country_onshoring_fun <- function(df, 
                                   onshoring_country = "US",
                                   inventor_number = 1,
                                   world_class_indicator = "world_class_90"){
        
        # Step 1: subset to firms that are not from the onshoring country 
        # as well as to their high-impact patents
        tmp <- df %>% filter(country_firm != onshoring_country)
        tmp <- tmp %>% filter_at(vars(starts_with(world_class_indicator)), any_vars(. == 1))
        
        # Step 2: Assign 'onshoring'-status = 1 to all patents with at least 
        # (N = inventor_number) inventors located in the onshoring country.
        onshoring_p_keys <- setDT(tmp)[ctry_inv == onshoring_country,
                                       .(onshored = ifelse(.N >= inventor_number, 1, 0)), 
                                       by = .(p_key)]
        setkey(onshoring_p_keys, p_key)
        tmp <- onshoring_p_keys[setDT(tmp, key = "p_key")]
        tmp[is.na(tmp$onshored) == TRUE, "onshored"] <- 0
        
        return(tmp)
}
