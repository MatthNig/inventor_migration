#################################################################
# Description:    Script to classify inventors' origins based   #
#                 on the trained model.                         #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   20.11.2020                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages for data processing: ------------------------------------------------
library("tidyverse")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")
library("reticulate")

#### directories  -----------------------------------------------------------------
setwd("/scicore/home/weder/nigmat01/inventor_migration")
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#### load the trained LSTM model  -------------------------------------------------
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
origin_model <- load_model_hdf5(paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))
if(is.null(origin_model) == FALSE){print("Classification model loaded")}else{"Model could not be loaded"}

#### load inventor data & batch parameters -------------------------------------
# random sample:
# names_encoded <- readRDS(paste0(getwd(), "/Data/training_data/inventor_encoded.rds"))
# pred_dat <- readRDS(paste0(getwd(), "/Data/training_data/inventor_raw.rds"))

# full sample:
BATCH_PARAMS <- readRDS(paste0(mainDir1, "/created data/inv_origins_temp/batch_params.rds"))
pred_dat <- readRDS(paste0(mainDir1, "/created data/inv_origins_temp/inventor_raw.rds"))
print("Encoded inventor data loaded.")

#### get origin classes  -------------------------------------------------------
df_train <- read.csv(file = paste0(getwd(), "/Data/training_data/df_train.csv"))
y_classes <- data.frame(
        levels = levels(as.factor(df_train$origin)),
        numbers = seq(length(unique(df_train$origin)))
)
df_train <- NULL
print("All data loaded. Ready for classification.")

################################################
########## Classify inventors' origins #########
################################################

# choose sample to predict:
# set.seed(250000)
# N <- 500000
# sample_idx <- sample(nrow(pred_dat), N, replace = FALSE)
# names_encoded <- names_encoded[sample_idx, , ]
# pred_dat <- pred_dat[sample_idx, ]

res <- lapply(BATCH_PARAMS[[2]], function(x){
        
        # load encoded names
        names_encoded <- readRDS(paste0(mainDir1,
                                "/created data/inv_origins_temp/BATCH",
                                x + 1, "_encoded.rds"))

        # subset pred_dat to batch
        start_idx <- x + 1
        end_idx <- ifelse(nrow(pred_dat) > (x + BATCH_PARAMS[[1]]),
                          (x + BATCH_PARAMS[[1]]), nrow(pred_dat))
        res <- pred_dat[start_idx : end_idx, ]
        
        # classify origin classes of the batch
        res$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))
        res$origin <- y_classes[res$origin + 1, ]$levels
        
        # predict class probabilities of the batch
        tmp <- origin_model %>% predict_proba(names_encoded[ , , ])
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0("prob_", y_classes$levels)
        res <- cbind(res, tmp)
        
        # clean and return the predicted batch as a data.frame
        names_encoded <- NULL
        print(paste0("Predicted batch ", x + 1, "to", 
                     x + BATCH_PARAMS[[1]]))
        return(res)}
       )

# combine batches to one single data.frame and delete all batches
pred_dat <- bind_rows(res)
res <- NULL

########################### OLD: FOR SUBSAMPLE PREDICTION #########################
# ## (1) predict origin classes: ------------------------------------------------------
# pred_dat$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))
# pred_dat$origin <- y_classes[pred_dat$origin + 1, ]$levels
# paste0("Ethnical origin assigned to ", nrow(pred_dat), " inventors.")
# 
# #### (2) predict origin probabilities
# tmp <- origin_model %>% predict_proba(names_encoded[ , , ])
# tmp <- as.data.frame(tmp)
# names(tmp) <- paste0("prob_", y_classes$levels)
# pred_dat <- cbind(pred_dat, tmp)
# paste0("Probabilities for ethnical origins assigned to ", nrow(pred_dat), " inventors.")

############################
##### Save the dataset #####
############################

# random sample:
# pred_dat %>% saveRDS(paste0(getwd(), "/Data/patent_data/inventor_origin.rds"))

# full sample:
pred_dat %>% saveRDS(paste0(mainDir1, "/created data/inventor_origin.rds"))

print("Dataset saved as 'inventor_origin.rds'.")
