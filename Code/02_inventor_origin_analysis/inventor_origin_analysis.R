#################################################################
# Description:    Script to evaluate migration flows of patent  #
#                 inventors into the U.S. and EU countries      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   30.06.2021                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("viridis")

# directories  -----------------------------------------------------------------
DatDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

# Load names of patents' inventors --------------------------------------------
inv_dat <- readRDS(paste0(DatDir, "/created data/inventor_origin.rds"))
techfield_grouping <- read.table(paste0(getwd(), "/Data/patent_data/techfield_grouping.txt"), 
                                 header = TRUE, sep = ";")
inv_dat <- merge(inv_dat, techfield_grouping, by = "tech_field", all.x = TRUE)
print("Data on patent inventors loaded")

# Load helper functions for analysis -------------------------------------------
source("Code/02_inventor_origin_analysis/inventor_analysis_functions.R")

##############################################################
##### Figure 1: Global Distribution of Ethnic Origins ########
##############################################################

origin_dist <- function(df){
        
        df <- df %>% filter(p_year >= 1980 & p_year <= 2015)
        
        annual_total <-  df %>% group_by(p_year) %>% summarise(total = n())
        
        tmp <- df %>% 
                group_by(p_year) %>% 
                select(contains("prob")) %>%
                summarise_all(.funs = sum)
        
        tmp <- merge(tmp, annual_total, by = "p_year")
        tmp[, grepl("prob", names(tmp))] <- tmp[, grepl("prob", names(tmp))] / tmp$total
        
        tmp <- gather(tmp, key = "origin", value = "share", -p_year, -total)
        tmp$origin <- gsub("prob_", "", tmp$origin)
        return(tmp)
}

plot_df <- origin_dist(df = inv_dat)

ggplot(plot_df, aes(x = p_year, y = share, fill = origin))+
        geom_bar(stat = "identity", width = 1) +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_viridis(option = "viridis", discrete = TRUE) +
        labs(x = "Year", y = "Share Among Patent Inventors",
             fill = "Ethnic Origin")+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
ggsave("/scicore/home/weder/nigmat01/origin_distribution.png")

##############################################################
######## Figure 2: Dominant Domestic Ethnic Origin ###########
##############################################################

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
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
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
######## Figure 3: Cumulative Non-Western Origin Share in Western countries ###########
#######################################################################################

COUNTRIES <- c("US", "GB", "FR", "DE", "JP", "IT")
unique(inv_dat$origin)
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")

plot_df <- non_western_comparison(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
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
######## Figure 4: Non-Western Origin Shares in Western countries ###########
#############################################################################

COUNTRIES <- c("US", "GB", "FR", "DE")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")

plot_df <- foreign_shares_fun(COUNTRIES, NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = origin))+
        facet_wrap(.~ country)+ 
        geom_line()+
        labs(x = "Year", y = "Shares of Non-Western Ethnic Origins", color = "", shape = "")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.125))+
        scale_color_viridis(option = "viridis", begin = 0, end = 0.8, discrete = TRUE)+
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
######## Figure 5: Non-Western Origin Shares in Western countries by Tech-Field #########
#########################################################################################

non_western_techfield <- function(countries, origins, techfields, 
                                  min_inventors = 30, MA_5 = FALSE){
        
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
        
        # calculate 5year rolling average:
        if(MA_5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, tech_field) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > 1984)}
        
        return(country_diff)
}

non_western_techgroup <- function(countries, origins, MA_5 = FALSE){
        inv_origin_shares <- lapply(countries, 
                                    function(x) inv_comp_techfield(inv_dat, x, grouping = TRUE))
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
        if(MA_5 == TRUE){
                ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}
                country_diff <- country_diff %>%
                        group_by(country, tech_field) %>%
                        arrange(p_year) %>%
                        mutate(five_y_ma_share = ma(share)) %>%
                        filter(p_year > 1984)}
        
        return(country_diff)
}

COUNTRIES <- c("US")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")

TECHFIELDS <- c(4, 6, 8) # hot emerging fields
TECHFIELDS <- c(TECHFIELDS, c(2, 26, 32)) # low traditional fields
TECHFIELDS <- as.character(TECHFIELDS)

plot_df <- non_western_techfield(countries = COUNTRIES, 
                                 origins = NON_WESTERN_ORIGIN,
                                 techfields = TECHFIELDS,
                                 min_inventors = 30)
plot_df <- merge(plot_df, techfield_grouping[, c("tech_field", "tech_field_name")], 
                 by = "tech_field", all.x = TRUE)

ggplot(plot_df, aes(x = p_year, y = share, color = tech_field_name, shape = tech_field_name))+
        geom_line()+ geom_point()+
        scale_color_hue("Technology Field")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
        labs(y = "Shares of Non-Western Origins", x = "Year", color = "", shape = "")+
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 2))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))

######################################################################################
######## Figure 9 (Appendix): Dominant Ethnic Origin in European Countries ###########
######################################################################################

COUNTRIES <- c("CH", "NL", "SE", "DK", "AT", "ES", "BE")
DOMESTIC_ORIGIN <- list(c("German", "Italian", "French"), 
                        c("German", "AngloSaxon", "Scandinavian"), "Scandinavian", "Scandinavian",
                        "German", "Hispanic-Iberian", c("German", "AngloSaxon", "French"))

plot_df <- dominant_domestic_comparison(countries = COUNTRIES,
                                        domestic_origin = DOMESTIC_ORIGIN)
plot_df <- plot_df %>% group_by(country, p_year) %>% summarize(dominant_share = sum(dominant_share))

ggplot(plot_df, aes(x = p_year, y = dominant_share, color = country, shape = country))+
        geom_line()+ geom_point()+ 
        scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
        labs(y = "Share of Dominant Domestic Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
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

####################################################################################
######## Figure 10 (Appendix): Non-Western Ethnic Origin in European Countries ######
####################################################################################

COUNTRIES <- c("CH", "NL", "SE", "DK", "AT", "ES", "BE")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")
plot_df <- non_western_comparison(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN)

ggplot(plot_df, aes(x = p_year, y = share, color = country, shape = country))+
        geom_line()+ geom_point() + scale_color_hue("Country")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.3))+
        labs(y = "Share of Non-Western Origins", x = "Year",
             shape = "", color = "")+
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
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

################################################################################
## Figure 11 (Appendix): Non-Western Ethnic Origin Shares Across Technologies ########
################################################################################

COUNTRIES <- c("GB", "FR", "DE", "IT", "JP", "US")
NON_WESTERN_ORIGIN <- c("China", "India", "Slavic-Russian",
                        "Arabic", "Persian", "Turkey", "SouthEastAsia")#, "Balkans", "EastEurope")
TECHFIELDS <- c(4, 6, 8) # hot emerging fields
TECHFIELDS <- c(TECHFIELDS, c(2, 26, 32)) # low traditional fields
TECHFIELDS <- as.character(TECHFIELDS)

plot_df <- non_western_techfield(countries = COUNTRIES, origins = NON_WESTERN_ORIGIN,
                                 techfields = TECHFIELDS, min_inventors = 30)
plot_df <- merge(plot_df, techfield_grouping, by = c("tech_field"), all.x = TRUE)

ggplot(plot_df, aes(x = p_year, y = share, color = tech_field_name))+
        facet_wrap(.~country)+
        geom_line()+ 
        scale_color_hue("Technology Field")+
        scale_y_continuous(labels = scales::percent, limits = c(0, 0.6))+
        labs(y = "Shares of Non-Western Origins", x = "Year", color = "", shape = "")+
        scale_color_viridis(option = "viridis", end = 0.8, discrete = TRUE)+
        guides(color = guide_legend(nrow = 2))+
        theme(panel.background = element_blank(),
              panel.grid.major.y = element_line(linetype = "dotted", color = "grey"),
              legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(),
              axis.title = element_text(face="bold",size=10))
