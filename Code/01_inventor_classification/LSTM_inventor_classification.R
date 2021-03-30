#################################################################
# Description:    Script to classify inventors' origins based   #
#                 on the trained model.                         #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Last revised:   27.03.2021                                    #
#################################################################

#######################################
## Load packages and set directories ##
#######################################

#### packages for data processing: ---------------------------------------------
library("tidyverse")

#### packages for making predictions using the trained network: -------------------
library("reticulate")
library("tensorflow")
library("keras")

#### use the same python virtual environment as for training the network
use_virtualenv("/scicore/home/weder/nigmat01/venv_inventor_migration", required = TRUE)

#### directories  -----------------------------------------------------------------
setwd("/scicore/home/weder/nigmat01/inventor_migration")
ModelDir <- "/scicore/home/weder/nigmat01/name_to_origin"
DatDir <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
        print("Working directory corresponds to repository directory")}else{
                print("Make sure your working directory is the repository directory.")}

#### load the trained LSTM model  -------------------------------------------------
origin_model <- load_model_hdf5(paste0(ModelDir, "/Classification_models/name_origin_lstm.h5"))
if(is.null(origin_model) == FALSE){print("Classification model loaded")}else{"Model could not be loaded"}

#######################################
###### Load inventor information ######
#######################################

# Load the inventor dataset to be complemented with ethnic origin information
pred_dat <- readRDS(paste0(DatDir, "/created data/inv_origins_temp/inventor_raw.rds"))
print(paste("Information on", nrow(pred_dat), "inventors loaded."))

# Load 17 Ethnic Origin Classes that were used to train the model
y_classes <- read.csv(paste0(ModelDir, "/Data/05_inventor_sample.csv"))
y_classes <- y_classes %>% distinct(origin_encoded, origin) %>% arrange(origin_encoded)
print(paste(nrow(y_classes), "Ethnic origin classes loaded."))

# Load batch size for the encoded names
BATCH_PARAMS <- readRDS(paste0(DatDir, "/created data/inv_origins_temp/batch_params.rds"))

print("All data loaded. Ready for classification.")

################################################
########## Classify inventors' origins #########
################################################

res <- lapply(BATCH_PARAMS[[2]], function(x){
        
        # load batch of encoded names
        names_encoded <- readRDS(paste0(DatDir,
                                "/created data/inv_origins_temp/BATCH",
                                x + 1, "_encoded.rds"))

        # subset the inventor dataset to the samples in the encoded batch
        start_idx <- x + 1
        end_idx <- ifelse(nrow(pred_dat) > (x + BATCH_PARAMS[[1]]),
                          (x + BATCH_PARAMS[[1]]), nrow(pred_dat))
        res <- pred_dat[start_idx : end_idx, ]
        
        # classify origin classes of the batch
        res$origin <- as.numeric(origin_model %>% predict_classes(names_encoded[ , , ]))
        res$origin <- y_classes[res$origin + 1, ]$origin
        
        # predict class probabilities of the batch
        tmp <- origin_model %>% predict_proba(names_encoded[ , , ])
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0("prob_", y_classes$origin)
        res <- cbind(res, tmp)
        
        # clean and return the predicted batch as a data.frame
        names_encoded <- NULL
        print(paste0("Predicted batch ", x + 1, "to", 
                     x + BATCH_PARAMS[[1]]))
        return(res)}
       )

# combine all batches to a single data.frame and delete all batches
pred_dat <- bind_rows(res)
res <- NULL

# full sample:
pred_dat %>% saveRDS(paste0(DatDir, "/created data/inventor_origin.rds"))

print("Dataset saved as 'inventor_origin.rds'.")
