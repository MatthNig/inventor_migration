#########################################################################
# Description:    Functions for analyzing ethnic origins of inventors   #
# Authors:        Matthias Niggli/CIEB UniBasel                         #
# Date:           16.02.2021                                            #
#########################################################################

#### ETHNIC ORIGIN COMPOSITION: -------------------------------------------
# calculates weighted sum for all ethnic origins at the country level

inv_comp_ctry <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}


#### DOMINANT D0MESTIC ORIGIN SHARE: -------------------------------------------
# calculates the share of the dominant domestic origin in different countries.
# function requires a vector of countries and a list of their corresponding dominant
# domestic ethnic origins as an input.

dominant_domestic_comparison <- function(countries, domestic_origin){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], origin %in% domestic_origin[[i]])
                tmp <- tmp %>% mutate(dominant_share = share) %>%
                        select(p_year, dominant_share, country) %>%
                        filter(p_year <= 2015 & p_year >= 1980)
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}

#### CUMULATIVE ORIGIN SHARES: -------------------------------------------
# calculates the cumulative share of a selection of ethnic backgrounds for different countries.
# function requires a vector of countries and a vector of the selected ethnic origins as an input.

non_western_comparison <- function(countries, origins){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], origin %in% origins)
                tmp <- tmp %>% group_by(p_year) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}

#### SPECIFIC ORIGIN SHARES: -------------------------------------------
# calculates the individual share of each selected ethnic background for different countries.
# function requires a vector of countries and a vector of the selected ethnic origins as an input.

foreign_shares_fun <- function(COUNTRIES, ORIGIN){
        
        inv_origin_shares <- lapply(COUNTRIES, function(x) inv_comp_ctry(inv_dat, x))
        for (i in length(inv_origin_shares)) {
                inv_origin_shares[[i]]$country <- COUNTRIES[i]
        }
        inv_origin_shares <- bind_rows(inv_origin_shares)
        
        plot_data <- filter(inv_origin_shares, origin %in% ORIGIN & 
                                    p_year <= 2015 & p_year >= 1980)
        
        return(plot_data)
}

#### ORIGIN SHARES BY COUNTRY AND TECHFIELDS: -------------------------------------------
# calculates the ethnic origin composition within techfields and countries.
# function requires a vector of countires and specification if techfields should be
# grouped to major techfields.

inv_comp_techfield <- function(df, country, grouping = FALSE){
        
        if(grouping == FALSE){
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_field, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_field", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_field)
        }else{
                annual_total <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% summarise(total = n())
                
                tmp <- filter(df, Ctry_code == country) %>%
                        group_by(tech_group_name, p_year) %>% select(contains("prob")) %>%
                        summarise_all(.funs = sum)
                
                tmp <- merge(tmp, annual_total, by = c("tech_group_name", "p_year"))
                tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
                
                tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_group_name)  
        }
        
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# calculates the cumulative share of a selection of ethnic background for a specification of
# selected techfields and countries.

non_western_techfield <- function(countries, origins, techfields, min_inventors = 30){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_techfield(inv_dat, x, grouping = FALSE))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% origins & total >= min_inventors &
                                      tech_field %in% techfields)
                tmp <- tmp %>% group_by(p_year, tech_field) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}


