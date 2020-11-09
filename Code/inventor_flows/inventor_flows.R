#################################################################
# Description:    Script to evaluate migration flows of patent  #
#                 inventors into the U.S. and EU countries      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           16.10.2020                                    #
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

panel_dat <- filter(inv_dat, Ctry_code == "DE")
origin_dat <- panel_dat %>% group_by(p_year, origin) %>% summarise(count = n())
total <- panel_dat %>% group_by(p_year) %>% summarise(total = n())

panel_dat <- merge(origin_dat, total, by = "p_year")
panel_dat$share <- panel_dat$count / panel_dat$total
panel_dat <- filter(panel_dat, p_year <= 2015)

# Domestic Share
ggplot(filter(panel_dat, origin == c("German")), aes(x = p_year, y = share, color = origin))+
        geom_line()+ylim(0,0.8)

# Foreign Shares
ggplot(#panel_dat, 
       filter(panel_dat, origin %in% c("China", "India", "HispanicLatinAmerica",
                                       "Korea", "Japan", "Italian", "Russian&EastEurope")), 
       aes(x = p_year, y = share, color = origin))+
        geom_line()+ ylim(0, 0.15)
View(inv_dat %>% filter(Ctry_code == "CH" & origin == "HispanicLatinAmerica") %>% select(name, origin))


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

countries <- c("US", "GB", "FR", "DE")
inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
names(inv_origin_shares) <- countries

# Domestic Share
ggplot(inv_origin_shares[["US"]] %>% filter(origin == "AngloSaxon" & p_year <= 2015), 
       aes(x = p_year, y = share))+
        geom_line()+ylim(0,0.8)

# Foreign Shares
ggplot(filter(inv_origin_shares[["US"]], origin %in% c("China", "India", "HispanicLatinAmerica",
                                        "Italian", "Russian&EastEurope", "Scandinavian", "SouthEastAsia") &
                      p_year <= 2015),
        aes(x = p_year, y = share, color = origin))+
        geom_line()+ ylim(0, 0.12)

# Foreign Shares
ggplot(filter(inv_origin_shares[["US"]], origin %in% c("SouthEastAsia") & p_year <= 2015),
       aes(x = p_year, y = share, color = origin))+
        geom_line()+ ylim(0, 0.12)


#### compare different countries with respect to their origin composition
domestic_origin <- c("AngloSaxon", "AngloSaxon", "French", "German")
country_diff <- data.frame()
for(i in 1:length(inv_origin_shares)){
        tmp <- filter(inv_origin_shares[[i]], origin == domestic_origin[i])
        tmp <- tmp %>% mutate(foreign_share = 1 - share) %>% 
                select(p_year, foreign_share, country) %>%
                filter(p_year <= 2015)
        country_diff <- rbind(country_diff, tmp)
        }

# plot:
ggplot(country_diff, aes(x = p_year, y = foreign_share, color = country))+
        geom_line()

#### relative to the U.S.
countries <- c("US", "GB", "FR", "DE", "IT", "KR", "JP")
domestic_origin <- c("AngloSaxon", "AngloSaxon", "French", "German", "Italian",
                     "Korea", "Japan")
inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
names(inv_origin_shares) <- countries
country_diff <- data.frame()
US_shares <- filter(inv_origin_shares[["US"]], origin == "AngloSaxon") %>%
        select(p_year, share) %>% mutate(US_foreign_share = 1 - share)
for(i in 2:length(inv_origin_shares)){
        tmp <- filter(inv_origin_shares[[i]], origin == domestic_origin[i]) %>%
                mutate(foreign_share = 1- share)
        tmp <- merge(tmp, US_shares, by = "p_year")
        tmp <- tmp %>% select(p_year, foreign_share, US_foreign_share, country) %>%
                mutate(foreign_share = foreign_share / US_foreign_share) %>%
                filter(p_year <= 2015)
        country_diff <- rbind(country_diff, tmp)
}

# plot:
ggplot(country_diff, aes(x = p_year, y = foreign_share, color = country))+
        geom_line()+ labs(y = "foreign origin share relative to the U.S.")+
        geom_smooth()


####################################################
## China, India & South-East-Asia only #############
####################################################

countries <- c("US", "GB", "DE", "CH", "NL", "CA", "AT", "IT")#, "ES")
migrant_countries <- c("China", "India", "SouthEastAsia", "Russia&EastEurope", "HispanicLatinAmerica")
inv_origin_shares <- lapply(countries, function(x) inv_comp_ctry(inv_dat, x))
names(inv_origin_shares) <- countries
country_diff <- data.frame()
for(i in 1:length(inv_origin_shares)){
        tmp <- filter(inv_origin_shares[[i]], origin %in% migrant_countries)
        tmp <- tmp %>% group_by(p_year, country) %>% summarise(share = sum(share))
        tmp <- tmp %>% select(p_year, share, country) %>%
                filter(p_year <= 2015)
        country_diff <- rbind(country_diff, tmp)
}
# plot:
ggplot(country_diff, aes(x = p_year, y = share, color = country))+
        geom_line()

####################################################
## Migrant inflow by technological field ###########
####################################################

# create a function that requires the country and it's origin-class as arguments and returns
# the shares of non-domestic origin inventors by technological field over time.
# e.g. the share of non-AngloSaxon inventors in the U.S. by technological field.

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

plot_dat <- inv_comp_techfield(df = inv_dat, country = "US") %>% 
        mutate(tech_field = as.character(tech_field))
TECHFIELDS <- as.character(c(4, 6, 13:16))
TECHFIELDS <- seq(1, 34)
plot_dat <- filter(plot_dat, origin == "AngloSaxon" & p_year <= 2015 &
                           tech_field %in% TECHFIELDS)

ggplot(plot_dat, aes(x = p_year, y = 1 - share, color = tech_field))+
        facet_wrap(.~ tech_field)+
        geom_line()

