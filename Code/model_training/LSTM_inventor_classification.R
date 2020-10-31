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
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}
#setwd("/scicore/home/weder/nigmat01/inventor_migration")

# load the trained LSTM model
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
if(is.null(origin_model) == FALSE){print("Classification model loaded")}else{"Model could not be loaded"}

# load encoded inventor data
names_encoded <- readRDS(paste0(getwd(), "/Data/training_data/inventor_encoded.rds"))
pred_dat <- readRDS(paste0(getwd(), "/Data/training_data/inventor_raw.rds"))
print("Encoded inventor data loaded.")

#######################################
########## Classify inventors #########
#######################################

# choose sample to predict:
set.seed(250000)
sample_idx <- sample(nrow(pred_dat), 250000, replace = FALSE)
names_encoded <- names_encoded[sample_idx, , ]
pred_dat <- pred_dat[sample_idx, ]

# get classes:
df_train <- read.csv(file = paste0(getwd(), "/Data/training_data/df_train.csv"))
y_classes <- data.frame(
        levels = levels(as.factor(df_train$origin)),
        numbers = seq(length(unique(df_train$origin)))
)
df_train <- NULL

#### (1) predict origin classes: ------------------------------------------------------
pred_dat$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))
pred_dat$origin <- y_classes[pred_dat$origin + 1, ]$levels
paste0("Ethnical origin assigned to ", nrow(pred_dat), " inventors.")

#### (2) predict origin probabilities
tmp <- origin_model %>% predict_proba(names_encoded[ , , ])
tmp <- as.data.frame(tmp)
names(tmp) <- paste0("prob_", y_classes$levels)
pred_dat <- cbind(pred_dat, tmp)
paste0("Probabilities for ethnical origins assigned to ", nrow(pred_dat), " inventors.")

############################
##### Save the dataset #####
############################

pred_dat %>% saveRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds"))
print("Dataset saved as 'inventor_origin.rds'.")
