#################################################################
# Description:    Script to classify and enocode retrieved names#
#                 from nameprism for training the LSTM model.   #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           16.10.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("stringi")
library("keras")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# function to encode names -----------------------------------------------------
source(paste0(getwd(),"/Code/training_data/names_encoding_function.R"))
print("Functions are loaded")

###################################
############ Load data ############
###################################

df <- readRDS(paste0(mainDir1, "/created data/origin_training_sample.rds"))
print("Data is loaded")

######################################################
############ Create custom country groups ############
######################################################

name_prism_origin <- names(df)[3:41]
ref_list <- data.frame(name_prism_origin = name_prism_origin,
                       origin_group = NA)

ref_list[grep("Africa", ref_list$name_prism_origin), 
         "origin_group"] <- "Africa"
ref_list[grep("Europe", ref_list$name_prism_origin), 
         "origin_group"] <- "EastEurope"
ref_list[grep("Rus", ref_list$name_prism_origin), 
         "origin_group"] <- "Russian"
ref_list[grep("German", ref_list$name_prism_origin), 
         "origin_group"] <- "German"
ref_list[grep("Ital", ref_list$name_prism_origin), 
         "origin_group"] <- "Italian"
ref_list[grep("Fren", ref_list$name_prism_origin), 
         "origin_group"] <- "French"
ref_list[grep("Celtic", ref_list$name_prism_origin), 
         "origin_group"] <- "AngloSaxon"
ref_list[grep("Nordic", ref_list$name_prism_origin), 
         "origin_group"] <- "Scandinavian"

ref_list[grep("Muslim", ref_list$name_prism_origin), 
         "origin_group"] <- "MiddleEast"
ref_list[grep("Persian", ref_list$name_prism_origin), 
         "origin_group"] <- "Persian"
ref_list[grep("Turkey", ref_list$name_prism_origin), 
         "origin_group"] <- "Turkey"

ref_list[grep("EastAsia", ref_list$name_prism_origin), 
         "origin_group"] <- "SouthEastAsia"
ref_list[grep("Chin", ref_list$name_prism_origin), 
         "origin_group"] <- "China"

ref_list[grep("Jap", ref_list$name_prism_origin), 
         "origin_group"] <- "Japan"
ref_list[grep("Kore", ref_list$name_prism_origin), 
         "origin_group"] <- "Korea"
ref_list[grep("Hisp", ref_list$name_prism_origin), 
         "origin_group"] <- "HispanicLatinAmerica"
ref_list[grep("Phil", ref_list$name_prism_origin), 
         "origin_group"] <- "Philippines"
ref_list[grep("SouthAsia", ref_list$name_prism_origin), 
         "origin_group"] <- "India"
ref_list[grep("Jewish", ref_list$name_prism_origin), 
         "origin_group"] <- "Jewish"
ref_list[grep("Greek", ref_list$name_prism_origin), 
         "origin_group"] <- "Greek"

# re-group to larger regions
ref_list[ref_list$origin_group %in% c("Russian", "EastEurope"), "origin_group"] <- "Russian&EastEurope"
#ref_list[ref_list$origin_group %in% c("China", "SouthEastAsia"), "origin_group"] <- "China&SouthEastAsia"

# prepare for classification
N = nrow(df)
n_X <- length(unique(ref_list$origin_group))+1
origin_groups <- as.data.frame(matrix(rep(NA, N * n_X), nrow = N))
colnames(origin_groups) <- c(colnames(df)[1], 
                             unique(ref_list$origin_group))
origin_groups[, 1] <- df[, 1]

## sum the corresponding probabilities together --------------------------------
for(i in 2:ncol(origin_groups)){
        origin_group <- colnames(origin_groups)[i]
        origins <- ref_list[ref_list$origin_group == origin_group, "name_prism_origin"]
        
        if(is.data.frame(df[, origins]) == FALSE){
                origin_prob <- df[, origins]}else{
                        origin_prob <- rowSums(df[, origins])}
        
        origin_groups[, origin_group] <- origin_prob
}

df <- origin_groups
paste0("Custom class probabilities calculated for ", nrow(df), " observations")

#########################################################
############# Evaluate, subset and classify #############
#########################################################

## get maximum prediction
df$max_pred <- sapply(seq(1, nrow(df)), function(i){
        max_pred <- as.numeric(df[i, 2:ncol(df)])
        max_pred <- max(max_pred, na.rm = TRUE)
        return(max_pred)
}
)
# start.time <- Sys.time()
# max_name <- function(dat){
#   tmp <- t(dat)[-1, ]
#   tmp <- as.data.frame(tmp)
#   tmp <- data.frame(sapply(tmp, as.numeric))
#   max_pred <- sapply(tmp, function(x) max(x, na.rm = TRUE))
#   dat$max_pred <- max_pred
#   return(dat)
# }
# df <- max_name(df)
# end.time <- Sys.time()
# time.taken_transp <- end.time - start.time
# time.taken_transp
# => this would be much faster but it creates problems with rounded numerics

## calculate distance to the second highest prediction
# df$distance_2nd <- sapply(seq(1, nrow(df)), function(i){
#         distance_2nd <- as.numeric(df[i, 2:(ncol(df)-1)])
#         distance_2nd <- sort(distance_2nd, decreasing = TRUE)[1:2]
#         distance_2nd <- abs(diff(distance_2nd))
#         return(distance_2nd)
# }
# )
dist_2nd <- function(dat){
  tmp <- t(dat[, !colnames(dat) %in% c("full_name", "max_pred")])
  tmp <- as.data.frame(tmp)
  tmp <- data.frame(sapply(tmp, as.numeric))
  tmp <- sapply(tmp, function(x) sort(x, decreasing = TRUE))
  tmp <- as.data.frame(tmp[1:2, ])
  distance_2nd <- sapply(tmp, function(x)abs(diff(x)))
  dat$distance_2nd <- distance_2nd
  return(dat)
}
df <- dist_2nd(df)

## calculate entropy
# df$entropy <- sapply(seq(1, nrow(df)), function(i){
#         entropy <- as.numeric(df[i, 2:(ncol(df)-2)])
#         entropy <- entropy * log(entropy)
#         entropy <- -sum(entropy)
#         return(entropy)
# }
# )
entropy_fun <- function(dat){
  tmp <- t(dat[, !colnames(dat) %in% c("full_name", "max_pred", "distance_2nd")])
  tmp <- as.data.frame(tmp)
  tmp <- data.frame(sapply(tmp, as.numeric))
  entro <- sapply(tmp, function(x){
    entro <- x * log(x)
    entro <- - sum(entro)
    return(entro)}
    )
  dat$entropy <- entro
  return(dat)
}
df <- entropy_fun(df)

## subset to clearly identified names: -----------------------------------------
# HERE I COULD ALSO TRY OUT DIFFERENT COMBINATIONS OF PARAMETERS AND THEN ESTIMATE THE MODELS WITH
# THESE DIFFERNET DATASETS. USE THE DATASET WITH THE BEST CLASSIFICATION PERFORMANCE

min_pred <- 0.6
min_dist <- 0.2
max_entropy <- 2

df <- df %>% filter(max_pred >= min_pred,
                    distance_2nd >= min_dist,
                    entropy <= max_entropy)

paste0("Sample cleaned from ambigous origin classifications. Loosing ", 
       100 * (1-(nrow(df)/N)), "% of oberservations")

## classify "origin" to the maximum value --------------------------------------
df$origin <- sapply(seq(1, nrow(df)), function(i){
        tmp <- as.numeric(df[i, 2:(ncol(df)-3)])
        max_origin <- which(tmp == df[i, "max_pred"])
        origin <- gsub("," ,"_", names(df)[1+max_origin])
        return(origin)
}
)

paste0("Training data ready. ", nrow(df),
       " names classified to the highest origin probability")

############################################
############ BALANCE THE SAMPLE ############
############################################

## Drop, up-/downsample classes in the training data --------------------------------------------
df <- select(df, full_name, origin)
df %>% group_by(origin) %>% summarise(count = n())

set.seed(10082020)
df_train <- df %>% filter(origin == "AngloSaxon")
tmp <- df %>% filter(origin == "French")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "German")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "HispanicLatinAmerica")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "India")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "Italian")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "Korea")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "Japan")
df_train <- rbind(df_train, tmp)

tmp <- df %>% filter(origin == "MiddleEast")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "Turkey")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "Persian")
df_train <- rbind(df_train, tmp)

tmp <- df %>% filter(origin == "Scandinavian")
df_train <- rbind(df_train, tmp)

# tmp <- df %>% filter(origin == "China&SouthEastAsia")
# df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "China")
df_train <- rbind(df_train, tmp)
tmp <- df %>% filter(origin == "SouthEastAsia")
df_train <- rbind(df_train, tmp)

tmp <- df %>% filter(origin == "Russian&EastEurope")
df_train <- rbind(df_train, tmp)

## summarize: --------------------------------------------
df_train %>% group_by(origin) %>% summarise(count = n(),
                                            share = count / nrow(df_train))
df_train <- df_train[sample(nrow(df_train), nrow(df_train)), ]
paste0("training sample ready: ", nrow(df_train), " observations for training.")

#####################################################
############ CREATE A CHARACTER DICTIONARY ##########
#####################################################

# double-check: search for special characters ----------------------------------
special_chars <- stri_extract_all(str = df_train$full_name, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
if(special_chars == " "){print("no special chars left")}else{warning("Special characters in sample")}

## choose character dictionary -------------------------------------------------
char_dict <- NULL
for(i in 1:length(df_train$full_name)){
        chars <- unlist(str_extract_all(df_train$full_name[i], "[:print:]"))
        chars <- unique(chars)
        char_dict <- c(char_dict, chars[!chars %in% char_dict])
}
char_dict <- c(sort(char_dict)[-1], " ","END")
if(length(letters %in% char_dict) < 26){warning("Not all standard letters are included in the vocabulary")}else{
        print("All standard letters are included in the vocabulary")}
n_chars <- length(char_dict)

# choose sequence length -------------------------------------------------------
hist(nchar(df_train$full_name), main = "", 
     xlab = "Number of characters per name") # highlight name length distribution
tmp <- nchar(df_train$full_name)
seq_tresh <- 30
paste0("truncating ", length(tmp[tmp>seq_tresh]), " (", round(100-length(tmp[tmp<=seq_tresh]) / nrow(df_train) *100, 2),
       "%) names to ", seq_tresh, " characters")
max_char <- seq_tresh

#######################################
######### CHARACTER ENCODING ##########
#######################################

x_dat <- encode_chars(names = df_train$full_name, 
                      seq_max = max_char,
                      char_dict = char_dict,
                      n_chars = n_chars)

paste("names are one-hot-encoded with shape: ", 
      paste0("(", paste(dim(x_dat), collapse = ", "), ")")
)

####################################
######### ORIGIN ENCODING ##########
####################################

y_dat <- as.factor(df_train$origin)
y_classes <- data.frame(levels = levels(y_dat), 
                        numbers = seq(length(unique(y_dat)))
)
levels(y_dat) <- y_classes$numbers
y_dat <- as.numeric(as.character(y_dat))
y_dat <- to_categorical(y_dat)
y_dat <- y_dat[, -1]

print("All names classified and encoded for training the model.")
