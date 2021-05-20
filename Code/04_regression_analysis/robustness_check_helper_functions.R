#### search for onshored patents to a given country
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
                tmp <- tmp %>% filter_at(vars(starts_with("world_class_90")), any_vars(. == 1))
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

#### assign techgroups based on Schmoch
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

#### assign time Periods
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

#### Aggregate number of onshored patents at the regional and tech-field level
onshoring_TechState <- function(df){
        
        onshored_pat <- setDT(df)[onshored == 1 & ctry_inv == "US",
                                  .(onshored_patents = uniqueN(p_key)),
                                  by = .(regio_inv, TechGroup, TimePeriod)]
        
        return(onshored_pat)
}


#### process the raw data
data_processing_fun <- function(dat){
        
        tmp <- assign_TechGroup(df = dat)
        tmp <- assign_TimePeriod(df = tmp)
        tmp <- onshoring_TechState(df = tmp)
        tmp <- tmp %>% filter(!is.na(regio_inv), !is.na(TimePeriod), !is.na(TechGroup))
        
        print(paste("Data on onshored patents prepared.", 
              sum(tmp$onshored_patents), 
              "onshored patents used for analysis"))
        
        return(tmp)
}

#### combine with baseline regression data
combine_dat_fun <- function(dat, merge_df, new_var){
        
        tmp <- dat %>% dplyr::select(regio_inv, TechGroup, TimePeriod, onshored_patents)
        idx <- which(names(tmp) == "onshored_patents")
        names(tmp)[idx] <- new_var
        
        # add to merging data.frame
        tmp <- merge(merge_df, tmp, 
                     by = c("regio_inv", "TechGroup", "TimePeriod"), 
                     all.x = TRUE)
        
        # assign zero onshored patents to absent observations
        tmp[, new_var] <- unlist(lapply(tmp[, new_var], function(x)ifelse(is.na(x), 0, x))) 
        
        # Tests:
        if(sum(test_cols %in% names(tmp)) != length(test_cols)){
                warning("Not all original variables present in robustness data")}else{
                        print("Merging successful")}
        
        if(identical(tmp$onshored_patents, tmp[, new_var])){
                warning("Robustness and baseline data are identical")}else{
                        print("Added new data")}
        
        # Return
        return(tmp)
}