#################################################################
# Description:    Script to classify inventors' origins based   #
#                 on the trained LSTM model.                    #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           17.10.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")
library("reticulate")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# load the trained LSTM model
origin_model <- load_model_hdf5("/scicore/home/weder/nigmat01/Data_inventor_migration/origin_class_model.h5")
origin_model <- load_model_hdf5("/scicore/home/weder/nigmat01/Data_inventor_migration/origin_class_model.h5")
origin_model <- load_model_hdf5("/scicore/home/weder/nigmat01/Data_inventor_migration/origin_class_model.h5")
if(is.null(origin_model) == FALSE){print("Classification model loaded")}else{"Model could not be loaded"}

# function to encode names -----------------------------------------------------
source(paste0(getwd(),"/Code/training_data/names_encoding_function.R"))
print("Functions are loaded")

# Load names of patents' inventors --------------------------------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))
print("Names of patent inventors loaded")

#######################################
########## Preprocess names ###########
#######################################

# inspect the data:
pred_dat <- inv_reg
head(pred_dat)
colnames(pred_dat)

#### transform everything to lowercase letters and remove punctuation
pred_dat$name <- tolower(pred_dat$name)
pred_dat$name <- gsub("[[:punct:]]", "", pred_dat$name)
pred_dat$name <- gsub("[0-9]", "", pred_dat$name)

#### identify all letters in the sample and replace special characters
special_chars <- gsub(" ", replacement = "", pred_dat$name)
special_chars <- stri_extract_all(str = special_chars, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
special_chars <- special_chars[is.na(special_chars) == FALSE]
special_chars

repl_vec <- c("o", "u", "o", "e", "a", "e", "a", "c", "e", "a", "ae",
              "i", "o", "i", "a", "i", "u", "n", "a", "o", "a", "o",
              "u", "o", "e", "a", "ss", "y", "i", "a", "o", "b", "c", "u",
              "e", "s", "u", "u", "z", "n", "r", "u", "s", "e", "y", "o",
              "e", "l", "a", "e", "z", "g", "i", "e", "s", "e", "z", "l",
              "t", "c", "s", "a", "t", "a", "i", "s", "d", "c", "g", "l", "th")
data.frame(spec = special_chars, rep = repl_vec)

# clean special characters from "names"
pred_dat$name <- unlist(lapply(
        pred_dat$name, function(x) stri_replace_all_fixed(str = x, 
                                                           pattern = special_chars,
                                                           replacement = repl_vec,
                                                           vectorize_all = FALSE)))
# drop NA's from sample
NA_obs <- which(is.na(pred_dat$name) == TRUE)
if(length(NA_obs) > 0){pred_dat <- pred_dat[-NA_obs, ]}
if(length(pred_dat$name[is.na(pred_dat$name) == TRUE]) != 0){
        warning("Some first names in the sample are NA")}else{
                print("Ready for classification. No names are NA")}

# ## clean special characters from "names" ---------------------------------
# # define a function:
# clear_chars <- function(names, special_chars, repl_vec){
#         
#         # encode special chars to a regex pattern
#         sc <- paste0(special_chars, collapse = "|")
#         
#         # find names with one of the special chars
#         repl_names <- grepl(sc, x = names)
#         repl_names <- which(repl_names == TRUE)
#         
#         # replace characters in those names
#         tmp <- unlist(lapply(names[repl_names],
#                              function(x) stri_replace_all_fixed(str = x,
#                                                                 pattern = special_chars,
#                                                                 replacement = repl_vec,
#                                                                 vectorize_all = FALSE)))
#         # replace all those names with special characters with the cleaned ones
#         names[repl_names] <- tmp
#         return(names)
# }
# 
# first_names <- clear_chars(names = first_names,
#                            special_chars = special_chars, 
#                            repl_vec = repl_vec)

#######################################
########## Encode all names ###########
#######################################

# make a subsample for testing the code
sample_idx <- sample(nrow(pred_dat), 100000)
pred_dat <- pred_dat[sample_idx, ]

## construct vocabulary and sequence length ------------------------------------
char_dict <- c(letters, " ","END") # this is the vocab that was used for training the model
n_chars <- length(char_dict) # number of features
max_char <- 30 # the model was trained on a maximum length 30 characters

## transform inventors' first names into one-hot-encoded tensors ---------------
names_encoded <- encode_chars(names = pred_dat$name, 
                                    seq_max = max_char, 
                                    char_dict = char_dict,
                                    n_chars = n_chars)

# This takes around 8sec for 100k observations. 
# => With 20Mio. this would mean around 15min 

print(paste("Names encoded as a 3D-tensor of shape:", 
            paste(dim(names_encoded), collapse = ", ")))
print("Data is ready for classification")

# names_encoded %>% saveRDS("/scicore/home/weder/nigmat01/Data_inventor_migration/inventor_encoded.rds")
# pred_dat %>% saveRDS("/scicore/home/weder/nigmat01/Data_inventor_migration/inventor_raw.rds")

#######################################
########## Classify inventors #########
#######################################

# predict
pred_dat$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))

# get classes:
df_train <- read.csv(file = "/scicore/home/weder/nigmat01/Data_inventor_migration/df_train.csv")
y_classes <- data.frame(
        levels = levels(as.factor(df_train$origin)),
        numbers = seq(length(unique(df_train$origin)))
)
df_train <- NULL

# assign origin
pred_dat$origin <- y_classes[pred_dat$origin + 1, ]$levels
paste0("Ethnical origin assigned to ", nrow(pred_dat), " inventors.")

############################
##### Save the dataset #####
############################

pred_dat %>% saveRDS("/scicore/home/weder/nigmat01/Data_inventor_migration/inventor_origin.rds")
print("Dataset saved as 'inventor_origin.rds'.")
