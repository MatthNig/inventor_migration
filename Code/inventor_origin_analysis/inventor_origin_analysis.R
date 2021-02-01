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
library("viridis")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

# Load names of patents' inventors --------------------------------------------
inv_dat <- readRDS(paste0(mainDir1, "/created data/inventor_origin.rds"))
techfield_grouping <- read.csv("/scicore/home/weder/nigmat01/name_to_origin/Data/techfield_grouping.csv",
                               header = TRUE, sep = ";")
inv_dat <- merge(inv_dat, techfield_grouping, by = "tech_field", all.x = TRUE)
print("Data on patent inventors loaded")

#############################
######## Functions ##########
#############################

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

##############################################################
######## Figure 1: Dominant Domestic Ethnic Origin ###########
##############################################################

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

COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "IT")
DOMESTIC_ORIGIN <- list("AngloSaxon", "AngloSaxon", "French", 
                     "German", "Japan", "Italian")

plot_df <- dominant_domestic_comparison(countries = COUNTRIES,
                                      domestic_origin = DOMESTIC_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = dominant_share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0.3, 1))+
        labs(y = "Share of Dominant Domestic Origin", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(dominant_share)) %>%
        arrange(-change)

#######################################################################################
######## Figure 2: Cumulative Non-Western Origin Share in Western countries ###########
#######################################################################################

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


COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "IT")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "Arabic", "Persian", "Turkey")


plot_df <- non_western_comparison(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(share)) %>%
        arrange(-change)

#############################################################################
######## Figure 3: Non-Western Origin Shares in Western countries ###########
#############################################################################

#### Shares of foreign origins by country: -------------------------------------
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

COUNTRIES <- c("US", "GB", "FR", "DE")#, "JP", "IT")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "Arabic", "Persian", "Turkey")

plot_df <- foreign_shares_fun(COUNTRIES, WESTERN)#NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = origin))+
        facet_wrap(.~ country)+ 
        geom_line()+
        labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "", shape = "")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.125))+
        scale_color_viridis(option = "inferno", begin = 0, end = 1, discrete = TRUE)+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))


# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>%
        group_by(country, origin) %>% summarize(change = diff(share)) %>%
        arrange(-change)

#########################################################################################
######## (Figure 4): Non-Western Origin Shares in Western countries by Tech-Field #########
#########################################################################################

## foreign origin shares by technology field and country
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

#ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

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
        
        # # calculate 5year rolling average:
        # country_diff <- country_diff %>% 
        #         group_by(country, tech_field) %>%
        #         arrange(p_year) %>%
        #         mutate(five_y_ma_share = ma(share)) %>%
        #         filter(p_year > 1984)
        
        return(country_diff)
}

non_western_techgroup <- function(countries, origins){
        inv_origin_shares <- lapply(countries, function(x) inv_comp_techfield(inv_dat, x, grouping = TRUE))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% origins)%>% 
                        group_by(p_year, tech_group_name) %>% summarize(share = sum(share)) %>%
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        # calculate 5year rolling average:
        # country_diff <- country_diff %>% 
        #         group_by(country, tech_group_name) %>%
        #         arrange(p_year) %>%
        #         mutate(share = ma(share)) %>%
        #         filter(p_year > 1984)

        return(country_diff)
}

### FIGURE 4: USA emerging fields --------------------------------------------
COUNTRIES <- c("US")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "Arabic", "Persian", "Turkey")
#TECHFIELDS <- c(1:35)
# plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
#         group_by(tech_field) %>% summarize(diff = diff(share)) %>%
#         arrange(-diff) %>% View()

TECHFIELDS <- c(4, 6, 8) # hot emerging fields
TECHFIELDS <- c(TECHFIELDS, c(2, 26, 32)) # low traditional fields
TECHFIELDS <- as.character(TECHFIELDS)

plot_df <- non_western_techfield(countries = COUNTRIES, 
                                 origins = NON_WESTERN_ORIGIN,
                                 min_inventors = 30,
                                 techfields = TECHFIELDS)
plot_df <- merge(plot_df, techfield_grouping[, c("tech_field", "tech_field_name")], 
                 by = "tech_field", all.x = TRUE)

ggplot(plot_df, aes(x = p_year, y = share, color = tech_field_name, shape = tech_field_name))+
        # facet_wrap(.~tech_field_name)+
        geom_line()+ geom_point()+
        scale_color_hue("Technology Field")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
        labs(y = "Shares of Non-Western Origins", x = "Year", color = "", shape = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 2))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))


### Table A1: Western countries all tech_fields --------------------------------------------
COUNTRIES <- c("GB", "FR", "DE", "IT", "JP", "US")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "Arabic", "Persian", "Turkey")
TECHFIELDS <- c(4, 6, 8) # hot emerging fields
TECHFIELDS <- c(TECHFIELDS, c(2, 26, 32)) # low traditional fields
TECHFIELDS <- as.character(TECHFIELDS)
plot_df <- non_western_techfield(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN,
                                 techfields = TECHFIELDS, min_inventors = 0)
plot_df <- merge(plot_df, techfield_grouping, by = c("tech_field"), all.x = TRUE)

ggplot(plot_df, aes(x = p_year, y = share, color = tech_field_name))+
        facet_wrap(.~country)+
        geom_line()+ 
        scale_color_hue("Technology Field")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
        labs(y = "Shares of Non-Western Origins", x = "Year", color = "", shape = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 2))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# table:
tmp <- plot_df %>% filter(p_year %in% c(1985, 2015)) %>% 
        group_by(country, tech_field) %>% summarize(difference = diff(share)) %>%
        arrange(country, tech_field)
plot_df <- plot_df %>% filter(p_year %in% c(1985, 2015)) %>% 
        spread(p_year, share)
tmp <- NULL
plot_df <- merge(plot_df, techfield_grouping[, c("tech_field", "tech_field_name")], 
                 by = "tech_field", all.x = TRUE)
plot_df <- plot_df %>% mutate(`1985` = ifelse(is.na(`1985`), 0, `1985`),
                  difference = ifelse(is.na(difference), `2015`, difference),
                  tech_field = paste(tech_field, tech_field_name)) %>%
        select(- tech_field_name) %>% arrange(country, tech_field)
print(xtable(plot_df[, c(2,1,3,4,5)]),include.rownames = FALSE)


# ## tech groups ----------------------------------------
# plot_df <- non_western_techgroup(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN)
# 
# ggplot(plot_df, aes(x = p_year, y = share))+
#         facet_wrap(.~ tech_group_name)+
#         geom_line(aes(color = country))+
#         labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "")+
#         scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
#         scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
#         theme(panel.background = element_blank(),
#               panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#               legend.position = "bottom",
#               axis.line = element_line(),
#               axis.title = element_text(face="bold",size=10))
# 
# plot_df %>% filter(p_year %in% c(1985, 2015)) %>%
#         group_by(country, tech_field) %>% summarize(change = diff(share)) %>%
#         arrange(-change)
# 
# 
# ## emerging techfields ----------------------------------
# 
# # CHOOSE TECHFILEDS:
# 
# # unique(techfield_grouping$tech_group_name)
# # TECHFIELDS <- techfield_grouping %>% filter(tech_group_name == "Information & Communication Technology")
# # TECHFIELDS <- TECHFIELDS$tech_field
# plot_df <- non_western_techfield(countries = COUNTRIES, 
#                                  origins = NON_WESTERN_ORIGIN,
#                                  techfields = TECHFIELDS)
# 
# TECHFIELDS <- techfield_grouping %>% filter(tech_field %in% unique(plot_df$tech_field))
# # TECHFIELDS$Group <- TECHFIELDS$tech_field_name
# TECHFIELDS$Group <- paste(TECHFIELDS$tech_field, TECHFIELDS$tech_field_name)
# plot_df <- merge(plot_df, TECHFIELDS[, c("tech_field", "Group")], 
#                  by = "tech_field", all.x = TRUE)
# 
# ggplot(plot_df, aes(x = p_year, y = five_y_ma_share))+
#         facet_wrap(.~ Group)+
#         geom_line(aes(color = country))+
#         labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "")+
#         scale_y_continuous(labels = scales::percent, limits = c(0, 0.5))+
#         scale_color_viridis(option = "inferno", begin = 0.2, end = 0.8, discrete = TRUE)+
#         theme(panel.background = element_blank(),
#               panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
#               legend.position = "bottom",
#               axis.line = element_line(),
#               axis.title = element_text(face="bold",size=10))
# 
# plot_df %>% filter(p_year %in% c(1985, 2015)) %>%
#         group_by(country, tech_field) %>% summarize(change = diff(share)) %>%
#         arrange(-change)

############################################################################
######## Figure A1: Dominant Ethnic Origin in European Countries ###########
############################################################################

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

COUNTRIES <- c("CH", "NL", "SE", "DK", "AT", "ES", "BE")
DOMESTIC_ORIGIN <- list(c("German", "Italian", "French"), 
                        c("German", "AngloSaxon", "Scandinavian"), "Scandinavian", "Scandinavian",
                        "German", "Hispanic", c("German", "AngloSaxon", "French"))

plot_df <- dominant_domestic_comparison(countries = COUNTRIES,
                                        domestic_origin = DOMESTIC_ORIGIN)
plot_df <- plot_df %>% group_by(country, p_year) %>% summarize(dominant_share = sum(dominant_share))

ggplot(plot_df, aes(x = p_year, y = dominant_share, color = country, shape = country))+
        geom_line()+ geom_point()+ 
        scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
        labs(y = "Share of Dominant Domestic Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(dominant_share)) %>%
        arrange(-change)

############################################################################
######## Figure A2: Non-Western Ethnic Origin in European Countries ########
############################################################################

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


COUNTRIES <- c("CH", "NL", "SE", "DK", "AT", "ES", "BE")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        #"SouthEastAsia", 
                        "Arabic", "Persian", "Turkey")

plot_df <- non_western_comparison(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 1))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(share)) %>%
        arrange(-change)

###############################################################################################
######## Figure A3: Distribution of Non-Western Inventors across Advanced Economies ###########
###############################################################################################

non_western_inv_distribution <- function(countries, origins){
        N_non_western <- filter(inv_dat,
                                origin %in% origins &
                                        p_year %in% c(1980, 1990, 2000, 2010)) %>%
                group_by(p_year) %>% summarize(N_non_western = n())

        tmp <- filter(inv_dat,
                      origin %in% origins &
                              p_year %in% c(1980, 1990, 2000, 2010)) %>%
                mutate(country = ifelse(Ctry_code %in% countries, Ctry_code, "RoW")) %>%
                group_by(p_year, country) %>% summarize(non_western = n())
        
        tmp <- merge(tmp, N_non_western, by = "p_year") %>% 
                mutate(share = non_western / N_non_western)
        }

COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "CN", "IN")
NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "SouthEastAsia", "Arabic", "Persian", "Turkey")

plot_df <- non_western_inv_distribution(COUNTRIES, NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, fill = country))+
        geom_col(position = "dodge")+
        labs(x = "Year", y = "Share among Non-Western Ethnic Origin Inventors", fill = "")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.55))+
        scale_fill_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))


# -------------------------------- BRAINSTORM / PREVIOUS WORK ---------------------------------------

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


#### Share of inventors with domestic origin by country: -----------------------
countries <- c("US", "GB", "FR", "DE", "JP", "CN")
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

###########################################################
## Non-Domestic origin share by technological field ###########
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
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
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
        
        return(plot_dat)
}

# define parameters
TECHFIELDS <- as.character(c(4, 5, 6, 8, 24, 13:16))
plot_df <- tech_field_plot(COUNTRY = "US", DOMESTIC_ORIGIN = "AngloSaxon",
                TECHFIELDS = TECHFIELDS)

ggplot(plot_df, aes(x = p_year, y = 1-share))+
        facet_wrap(.~ tech_field, nrow = 3, ncol = 3)+
        geom_line(aes(color = tech_field))+
        scale_y_continuous(labels = scales::percent, limits = c(0.1, 0.8))+
        labs(y = "Share of non-domestic origin", x = "Year", color = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        guides(color = FALSE)+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

ggplot(plot_df, aes(x = p_year, y = 1-share, color = tech_field, shape = tech_field))+
        geom_line()+ geom_point()+
        scale_y_continuous(labels = scales::percent, limits = c(0.1, 0.8))+
        labs(y = "Share of non-domestic origin", x = "Year", 
             color = "", shape = "")+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        theme(panel.background = element_blank(),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))






# ### check geographical variation among highly treated: -------------------------
# inv_comp_techfield_geo <- function(df, country, techfields, 
#                                    Tech_group = FALSE, origins){
#         
#         annual_total <- filter(df, Ctry_code == country, 
#                                tech_field %in% techfields) %>%
#                 group_by(Up_reg_label, p_year) %>% summarise(total = n())
#         
#         tmp <- filter(df, Ctry_code == country,
#                       tech_field %in% techfields) %>%
#                 group_by(Up_reg_label, p_year) %>% select(contains("prob")) %>%
#                 summarise_all(.funs = sum)
#         
#         tmp <- merge(tmp, annual_total, by = c("Up_reg_label", "p_year"))
#         tmp <- filter(tmp, total >= 10)
#         share_vars <- names(tmp)[grepl(pattern = "prob_", x = names(tmp))]
#         tmp[, share_vars] <- tmp[, share_vars] / tmp$total
#         
#         tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -Up_reg_label)
#         tmp$origin <- gsub("prob_", "", tmp$origin)
#         tmp <- mutate(tmp, country = country, tech_field = paste(techfields, collapse = ", "))
#         tmp <- filter(tmp, origin %in% origins)
#         
#         if(Tech_group == FALSE){
#                 plot_name <- paste(techfields, collapse = ", ")}else{
#                         plot_name <- Tech_group}
#         
#         p <- ggplot(tmp, aes(x = p_year, y = share))+
#                 facet_wrap(.~ Up_reg_label)+
#                 geom_line(aes(color = origin))+
#                 labs(y = "Share of foreign origins", 
#                      x = "Year", 
#                      title = paste0("Share of origin shares in ",
#                                     country, "-states"), 
#                      subtitle = paste("in technological field:", plot_name))
#         return(p)
# }

TECHFIELDS <- as.character(c(13:17)) # MedTech, BioTech, Pharma, Chemicals
TECHFIELDS <- as.character(c(3:8)) # Telecommunications, IT and Software
TECHFIELDS <- as.character(c(1:2, 9:10)) # electrics, visual, measurement technology
TECHFIELDS <- as.character(seq(1:34)) # All technologies
TECHFIELDS <- as.character(c(19:21, 23)) # material sciences
TECHFIELDS <- as.character(c(24:32)) # machinery, transportation, pumps, energy & environment

NON_WESTERN_ORIGIN <- c("China", "India", "EastEurope", "Balkans", "Slawic",
                        "SouthEastAsia", "Arabic", "Persian", "Turkey")
ORIGINS <- "AngloSaxon"

inv_comp_techfield_geo(df = inv_dat, country = "US", 
                       Tech_group = "Machinery",
                       techfields = TECHFIELDS, origins = NON_WESTERN_ORIGIN)



#########################################################################
######## Figure 2: Dominant Domestic Ethnic Origin by Technology ########
#########################################################################

inv_comp_techfield <- function(df, country){
        
        annual_total <- filter(df, Ctry_code == country) %>%
                group_by(tech_group_name, p_year) %>% summarise(total = n())
        
        tmp <- filter(df, Ctry_code == country) %>%
                group_by(tech_group_name, p_year) %>% select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = c("tech_group_name", "p_year"))
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total, -tech_group_name)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        tmp$country <- country
        
        return(tmp)
}

dominant_domestic_techfield <- function(countries, domestic_origin){
        
        inv_origin_shares <- lapply(countries, function(x) inv_comp_techfield(inv_dat, x))
        names(inv_origin_shares) <- countries
        
        country_diff <- data.frame()
        
        for(i in 1:length(inv_origin_shares)){
                tmp <- filter(inv_origin_shares[[i]], 
                              origin %in% domestic_origin[[i]]) %>% 
                        filter(p_year <= 2015 & p_year >= 1980) %>%
                        mutate(country = names(inv_origin_shares)[i])
                country_diff <- rbind(country_diff, tmp)
        }
        
        return(country_diff)
}

COUNTRIES <- c("US", "GB", "FR", "DE")
DOMESTIC_ORIGIN <- list("AngloSaxon", "AngloSaxon", "French", 
                        "German", "Japan", "Italian")

plot_df <- dominant_domestic_techfield(countries = COUNTRIES,
                                       domestic_origin = DOMESTIC_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = 1 - share, color = country))+
        facet_wrap(.~ tech_group_name)+
        geom_line(aes(color = country))+
        labs(x = "Year", y = "Share of Dominant Domestic Origin", color = "")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.7))+
        scale_color_viridis(option = "inferno", end = 0.8, discrete = TRUE)+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))


# Changes from 1980 to 2015
plot_df %>% filter(p_year %in% c(1980, 2015)) %>% 
        group_by(country) %>% summarize(change = diff(dominant_share)) %>%
        arrange(-change)