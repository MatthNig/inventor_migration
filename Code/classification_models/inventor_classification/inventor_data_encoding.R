#################################################################
# Description:    Encodes the data on patent inventors for      #
#                 classification of their ethnical origins      #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   20.11.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("stringi")

## directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
setwd("/scicore/home/weder/nigmat01/inventor_migration")
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

## function to encode names -----------------------------------------------------
source(paste0(getwd(),"/Code/classification_models/names_encoding_function.R"))
source(paste0(getwd(),"/Code/training_data/clear_chars_function.R"))
if(is.null(encode_chars) == FALSE){print("Encoding functions loaded")}else{print("Load encoding function")}

## Load names of patents' inventors --------------------------------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))
print("Names of patent inventors loaded")

## load encoding parameters ----------------------------------------------------
PARAMS <- read.csv(file = paste0(getwd(), "/Data/training_data/PARAMS.csv"))
SEQ_MAX <- PARAMS$SEQ_MAX
N_CHARS <- PARAMS$N_CHARS
CHAR_DICT <- read.csv(file = paste0(getwd(), "/Data/training_data/CHAR_DICT.csv"))
CHAR_DICT <- CHAR_DICT$x
print("Encoding parameters loaded.")

#######################################
########## Preprocess names ###########
#######################################

#### inspect the data:
pred_dat <- inv_reg
nrow(pred_dat)
head(pred_dat)
colnames(pred_dat)

#### transform everything to lowercase letters and remove punctuation
pred_dat$name <- tolower(pred_dat$name)
pred_dat$name <- gsub("[[:punct:]]", "", pred_dat$name)
pred_dat$name <- gsub("[0-9]", "", pred_dat$name)

# drop NA's from sample
NA_obs <- which(is.na(pred_dat$name) == TRUE)
if(length(NA_obs) > 0){pred_dat <- pred_dat[-NA_obs, ]}
if(length(pred_dat$name[is.na(pred_dat$name) == TRUE]) != 0){
        warning("Some first names in the sample are NA")}else{
                print("Ready to proceed. No names are NA")}

#### subset the data to unique inventors
pred_dat <- pred_dat %>% distinct(name, .keep_all = TRUE)
paste(nrow(pred_dat), "inventors' origins have to be classified")

#### clean special characters from "names" -------------------------------------

# identify all letters in the sample and replace special characters
special_chars <- gsub(" ", replacement = "", pred_dat$name)
special_chars <- stri_extract_all(str = special_chars, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
special_chars <- special_chars[is.na(special_chars) == FALSE]
special_chars

# manually create a correspondence to replace special characters
# repl_vec <- c("o", "u", "o", "e", "a", "e", "a", "c", "e", "a", "ae",
#               "i", "o", "i", "a", "i", "u", "n", "a", "o", "a", "o",
#               "u", "u", "o", "e", "a", "ss", "y", "i", "a", "o", "b", "c", "u",
#               "e", "s", "u", "z", "n", "r", "u", "s", "e", "y", "o",
#               "e", "l", "a", "e", "z", "g", "i", "e", "s", "e", "z", "l",
#               "t", "c", "s", "a", "t", "a", "i", "s", "d", "c", "g", "l", "th")
repl_vec <- c("o", "u", "o", "e", "e", "a", "c", "e", "a", "a",
              "o", "i", "a", "i", "ae", "i", "u", "n", "a", "o", "a", "o",
              "u", "u", "o", "e", "a", "ss", "y", "i", "a", "o", "b", "c", "u",
              "e", "s", "u", "z", "n", "r", "u", "s", "e", "y", "o",
              "d", "l", "a", "e", "z", "g", "i", "e", "s", "e", "z", "l",
              "t", "c", "s", "a", "t", "a", "g", "l", "th")

data.frame(spec = special_chars, rep = repl_vec)

# test:
if(special_chars[64] != "ă" | special_chars[35] != "ū"){warnings(
        "Replacemnet of special characters with an error")}else{
        print("Ready to clean special characters")}

# use the clear_chars() function
pred_dat$name <- clear_chars(names = pred_dat$name,
                             special_chars = special_chars,
                             repl_vec = repl_vec)

#######################################
########## Encode all names ###########
#######################################

# SUBSAMPLE for testing the code
# set.seed(19102020)
# sample_idx <- sample(nrow(pred_dat), 500000)
# pred_dat <- pred_dat[sample_idx, ]

## transform inventors' first names into one-hot-encoded tensors ---------------

# use 500k batches and save them separatly to save memory:
BATCH_SIZE <- 500000
BATCHES <- seq(0, nrow(pred_dat), by = BATCH_SIZE)
lapply(BATCHES, function(x){
        
        # define the batch
        start_idx <- x + 1
        end_idx <- ifelse(nrow(pred_dat) > (x + BATCH_SIZE), (x + BATCH_SIZE), nrow(pred_dat))
        
        # encode names of this batch
        names_encoded <- encode_chars(names = pred_dat$name[start_idx : end_idx],
                                      seq_max = SEQ_MAX,
                                      char_dict = CHAR_DICT,
                                      n_chars = N_CHARS)
        
        # save encoded names
        names_encoded %>% saveRDS(paste0(mainDir1,
                                         "/created data/inv_origins_temp/BATCH", 
                                         x + 1, "_encoded.rds"))
        
        # report result
        print(paste0("BATCH", x, " encoded and saved as a 3D-tensor of shape: ",
                     paste(dim(names_encoded), collapse = ", ")))
        }
       )

# names_encoded <- encode_chars(names = pred_dat$name, 
#                               seq_max = SEQ_MAX, 
#                               char_dict = CHAR_DICT,
#                               n_chars = N_CHARS)

# print(paste("Names encoded as a 3D-tensor of shape:", 
#             paste(dim(names_encoded), collapse = ", ")))
# print("Data is ready for classification")

############################
##### Save the dataset #####
############################

# save sample on the repository:
# names_encoded %>% saveRDS(paste0(getwd(), "/Data/training_data/inventor_encoded.rds"))
# pred_dat %>% saveRDS(paste0(getwd(), "/Data/training_data/inventor_raw.rds"))
# print("Encoded inventor sample saved.")

# save the full sample on the cluster
pred_dat %>% saveRDS(paste0(mainDir1, "/created data/inv_origins_temp/inventor_raw.rds"))
list(BATCH_SIZE, BATCHES) %>% saveRDS(paste0(mainDir1, "/created data/inv_origins_temp/batch_params.rds"))

print("Encoding inventors successfull and saved for origin classification.")
