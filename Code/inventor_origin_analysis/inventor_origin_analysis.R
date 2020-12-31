#################################################################
# Description:    Script to evaluate migration flows of patent  #
#                 inventors into the U.S. and EU countries      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   10.11.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

# Load names of patents' inventors --------------------------------------------
# inv_dat <- readRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds")) # random subset of inventors
inv_dat <- readRDS(paste0(mainDir1, "/created data/inventor_origin.rds")) # full sample
print("Data on patent inventors loaded")

####################################################
## Count number of inventors per country and year ##
####################################################

panel_dat <- filter(inv_dat, Ctry_code == "US")
origin_dat <- panel_dat %>% group_by(p_year, origin) %>% summarise(count = n())
total <- panel_dat %>% group_by(p_year) %>% summarise(total = n())

panel_dat <- merge(origin_dat, total, by = "p_year")
panel_dat$share <- panel_dat$count / panel_dat$total
panel_dat <- filter(panel_dat, p_year <= 2015)

# Domestic Share
ggplot(filter(panel_dat, origin == c("AngloSaxon")), aes(x = p_year, y = share, color = origin))+
        geom_line()+ylim(0,0.8)

# Foreign Shares
ggplot(#panel_dat, 
       filter(panel_dat, origin %in% c("China", "India", "HispanicLatinAmerica",
                                       "Korea", "Japan", "Italian", "Russian&EastEurope")), 
       aes(x = p_year, y = share, color = origin))+
        geom_line()+ ylim(0, 0.15)

# HispanicLatinAmerica seems to be badly identified with respect to Germans.
# maybe I could do the following on my model: take the predited classes only if it is reasonably save
# i.e. for example minimum probability & minimum distance to the second largest prediction.
# if this is not fullfilled, assign NA

####################################################
## Create weighted sum for all origins #############
####################################################

inv_comp_ctry <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, c(-1, -17)] <- tmp[, c(-1, -17)] / tmp$total

        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country

        return(tmp)
}


#### Share of inventors with domestic origin by country: -----------------------
countries <- c("US", "GB", "FR", "DE", "JP")
inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
names(inv_origin_shares) <- countries

non_domestic_fun <- function(COUNTRY, ORIGIN){
        plot_data <- inv_origin_shares[[COUNTRY]] %>% 
                filter(origin == ORIGIN  & p_year <= 2015 & p_year >=1980)
        
        p <- ggplot(plot_data, aes(x = p_year, y = share))+
                geom_line()+ylim(0,1)+
                labs(x = "year", y = "share of domestic origin inventors",
                     title = paste0("Share of inventors with ",
                                    ORIGIN, " origin in ", COUNTRY))
        return(p)
}
        
non_domestic_fun("US", "AngloSaxon")


#### Shares of foreign origins by country: -------------------------------------
foreign_shares_fun <- function(COUNTRIES, ORIGIN){
        
        inv_origin_shares <- lapply(COUNTRIES, function(x) inv_comp_ctry(inv_dat, x))
        for (i in length(inv_origin_shares)) {
                inv_origin_shares[[i]]$country <- COUNTRIES[i]
        }
        inv_origin_shares <- bind_rows(inv_origin_shares)

        plot_data <- filter(inv_origin_shares, origin %in% ORIGIN & 
                                    p_year <= 2015 & p_year >= 1980)
        
        p <- ggplot(plot_data, aes(x = p_year, y = share, color = origin))+
                facet_wrap(.~ country)+
                scale_color_hue("Ethnic origin")+
                labs(x = "year", y = "share of ethnic background",
                     title = paste0(" Share of selected ethnic backgrounds \n in different countries"))+
                geom_line()+
                ylim(0, 0.12)
        
        return(p)
}
COUNTRIES <- c("US", "GB", "FR", "DE", "IT", "CH")
ORIGIN <- c("China", "India", "Russian&EastEurope", "SouthEastAsia")
foreign_shares_fun(COUNTRIES, ORIGIN)

#### Country comparison of overall foreign origin share of inventors:
foreign_country_comparison <- function(countries, domestic_origin){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], origin == domestic_origin[i])
                tmp <- tmp %>% mutate(foreign_share = 1 - share) %>%
                        select(p_year, foreign_share, country) %>%
                        filter(p_year <= 2015 & p_year >= 1980)
                country_diff <- rbind(country_diff, tmp)
        }
        
        p <- ggplot(country_diff, aes(x = p_year, y = foreign_share, color = country))+
                geom_line()+ scale_color_hue("Country")+
                labs(y = "Share of non-domestic origin", x = "Year",
                     title = paste0("Share of inventors with foreign origins in ", 
                                    paste(countries, collapse = ", ")))
        return(p)
}

COUNTRIES <- c("US", "GB", "FR", "DE", "JP")
DOMESTIC_ORIGIN <- c("AngloSaxon", "AngloSaxon", "French", "German", "Japan")

foreign_country_comparison(countries = COUNTRIES,
                           domestic_origin = DOMESTIC_ORIGIN)

###########################################################
## Domestic origin share by technological field ###########
###########################################################

# create a function that requires the country and it's origin-class as arguments and returns
# the shares of non-domestic origin inventors by technological field over time.
# e.g. the share of non-AngloSaxon inventors in the U.S. by technological field.

# calculates origin shares per tech_field
inv_comp_techfield <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(tech_field, p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(tech_field, p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = c("tech_field", "p_year"))
        tmp[, c(-1, -2, -18)] <- tmp[, c(-1, -2, -18)] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_field)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

# filters the data according to tech_fields and plots the shares
tech_field_plot <- function(COUNTRY, DOMESTIC_ORIGIN, 
                            TECHFIELDS, MIN_INVENTORS = 30){
        
        plot_dat <- inv_comp_techfield(df = inv_dat, country = COUNTRY) %>% 
                mutate(tech_field = as.character(tech_field))
        
        plot_dat <- filter(plot_dat, origin == DOMESTIC_ORIGIN & 
                                   p_year <= 2015 & p_year >= 1980 & 
                                   tech_field %in% TECHFIELDS & 
                                   total >= MIN_INVENTORS)
        
        p <- ggplot(plot_dat, aes(x = p_year, y = share))+
                facet_wrap(.~ tech_field)+
                geom_line(aes(color = tech_field))+scale_color_hue(guide = "none")+
                labs(y = "Share of domestic origin", 
                     x = "Year", title = paste0(" Share of ", DOMESTIC_ORIGIN, " origin in ",
                                                COUNTRY, "\n (by technological field)"))
        return(p)
}

# define parameters
TECHFIELDS <- as.character(c(4, 5, 6, 8, 13:16, 22, 24))
tech_field_plot(COUNTRY = "US", DOMESTIC_ORIGIN = "AngloSaxon",
                TECHFIELDS = TECHFIELDS)


###########################################################
## Foreign origin shares by technological field ###########
###########################################################

tech_field_foreign_plot <- function(COUNTRY, ORIGINS, 
                            TECHFIELDS, MIN_INVENTORS = 30){
        
        plot_dat <- inv_comp_techfield(df = inv_dat, country = COUNTRY) %>% 
                mutate(tech_field = as.character(tech_field))
        
        plot_dat <- filter(plot_dat, origin %in% ORIGINS & p_year <= 2015 & p_year >= 1980 &
                                   tech_field %in% TECHFIELDS & total >= MIN_INVENTORS)
        
        p <- ggplot(plot_dat, aes(x = p_year, y = share))+
                facet_wrap(.~ tech_field)+
                geom_line(aes(color = origin))+
                labs(y = "Share of foreign origins", 
                     x = "Year", title = paste0(" Share of foreign origin shares in ",
                                                COUNTRY, "\n (by technological field)"))
        return(p)
}
TECHFIELDS <- as.character(seq(1:34))
TECHFIELDS <- as.character(c(4, 5, 6, 8, 13:16, 22, 24))
ORIGINS <- c("China", "India", "HispanicLatinAmerica", "Russian&EastEurope", "SouthEastAsia")
tech_field_foreign_plot(COUNTRY = "US", ORIGINS = ORIGINS,
                        TECHFIELDS = TECHFIELDS, MIN_INVENTORS = 30)

### check geographical variation among highly treated: -------------------------
inv_comp_techfield_geo <- function(df, country, techfields, 
                                   Tech_group = FALSE, origins){
        
        annual_total <- filter(df, Ctry_code == country, 
                               tech_field %in% techfields) %>%
                group_by(Up_reg_label, p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country,
                      tech_field %in% techfields) %>%
                group_by(Up_reg_label, p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = c("Up_reg_label", "p_year"))
        tmp <- filter(tmp, total >= 10)
        share_vars <- names(tmp)[grepl(pattern = "prob_", x = names(tmp))]
        tmp[, share_vars] <- tmp[, share_vars] / tmp$total

        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -Up_reg_label)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp <- mutate(tmp, country = country, tech_field = paste(techfields, collapse = ", "))
        tmp <- filter(tmp, origin %in% origins)
        
        if(Tech_group == FALSE){
                plot_name <- paste(techfields, collapse = ", ")}else{
                        plot_name <- Tech_group}
        
        p <- ggplot(tmp, aes(x = p_year, y = share))+
                facet_wrap(.~ Up_reg_label)+
                geom_line(aes(color = origin))+
                labs(y = "Share of foreign origins", 
                     x = "Year", 
                     title = paste0("Share of origin shares in ",
                                    country, "-states"), 
                     subtitle = paste("in technological field:", plot_name))
        return(p)
}

TECHFIELDS <- as.character(c(13:17)) # MedTech, BioTech, Pharma, Chemicals
TECHFIELDS <- as.character(c(3:8)) # Telecommunications, IT and Software
TECHFIELDS <- as.character(c(1:2, 9:10)) # electrics, visual, measurement technology
TECHFIELDS <- as.character(seq(1:34)) # All technologies
TECHFIELDS <- as.character(c(19:21, 23)) # material sciences
TECHFIELDS <- as.character(c(24:32)) # machinery, transportation, pumps, energy & environment

ORIGINS <- c("China", "India", "HispanicLatinAmerica", "Russian&EastEurope", "SouthEastAsia")
ORIGINS <- "AngloSaxon"

inv_comp_techfield_geo(df = inv_dat, country = "US", 
                       Tech_group = "Machinery",
                       techfields = TECHFIELDS, origins = ORIGINS)

