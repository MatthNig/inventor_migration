#################################################################
# Description:    Script to train the LSTM model to classify    #
#                 inventor origins based on names.              #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           16.10.2020                                    #
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

################################
## Load the data for training ##
################################

df_train <- read.csv(file = "/scicore/home/weder/nigmat01/Data_inventor_migration/df_train.csv")
x_dat <- readRDS(file = "/scicore/home/weder/nigmat01/Data_inventor_migration/x_dat.rds")
y_dat <- readRDS(file = "/scicore/home/weder/nigmat01/Data_inventor_migration/y_dat.rds")

y_classes <- data.frame(
        levels = levels(as.factor(df_train$origin)),
        numbers = seq(length(unique(df_train$origin)))
        )

print("Data for training the model successfully loaded.")

################################################
######### split to train and test set ##########
################################################

train_frac <- 0.8 # fraction of data to train the model
N <- nrow(df_train)
set.seed(10082020)
train_idx <- sample(seq(N), N * train_frac)

x_train <- x_dat[train_idx, , ]
x_val <- x_dat[-train_idx, , ]
y_train <- y_dat[train_idx, ]
y_val <- y_dat[-train_idx, ]

#########################################
############ train the network ##########
#########################################

## build and compile the model -------------------------------------------------
model <- keras_model_sequential()
model %>%
        layer_lstm(units = 512, return_sequences = TRUE,
                   input_shape = c(ncol(x_train), dim(x_train)[3])) %>%
        layer_dropout(rate = 0.33) %>%
        layer_lstm(units = 256, return_sequences = TRUE) %>%
        layer_dropout(rate = 0.33) %>%
        layer_lstm(units = 64) %>%
        layer_dropout(rate = 0.1) %>%
        layer_dense(units = ncol(y_train), activation = "softmax")
summary(model)

model %>% compile(
        optimizer =  "adam",
        loss = "categorical_crossentropy",
        metrics = c("accuracy"))

## class_weights: --------------------------------------------------------------
y_classes$class_weights <- ifelse(y_classes$levels  == "AngloSaxon", 10, 1)
CLASS_WEIGHTS <- as.list(y_classes$class_weights)
names(CLASS_WEIGHTS) <- y_classes$numbers

## training parameters--------------------------------------------------------
EPOCHS <- 20
BATCH_SIZE <- 256

## fit the model
hist <- model %>% fit(
        x = x_train, y = y_train, 
        class_weights = CLASS_WEIGHTS,
        validation_data = list(x_val, y_val),
        callbacks = list(callback_early_stopping(monitor = "val_loss", 
                                                 patience = 3, 
                                                 restore_best_weights = TRUE)),
        epochs = EPOCHS, batch_size = BATCH_SIZE, verbose = 2)

## IDEA
# The goal is to have maximum accuracy for the Anglo-Saxons and relatively equal f1 for all others

## RESULTS  ----------------------------------
# (1)   weight AngloSaxon = 10, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 81.97%, TOTAL F1: 77.55%, 
#       ANGLOSAXON_ACC: 64.73%, ANGLOSAXON_F1: 78.59%, ANGLOSAXON_PRECISION: 82.05% 
#       REAMRKS: Model could further learn

# (2)   weight AngloSaxon = 15, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 80.84%, AVERAGE_F1: 76.10%, 
#       ANGLOSAXON_ACC: 63.42%, ANGLOSAXON_F1: 77.62%, ANGLOSAXON_PRECISION: 84.01% 
#       REAMRKS: Increasing weights did not help to raise accuracy

# (3)   weight AngloSaxon = 5, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 80.33%, AVERAGE_F1: 75.77%, 
#       ANGLOSAXON_ACC: 62.61%, ANGLOSAXON_F1: 77.01%, ANGLOSAXON_PRECISION: 73.3%, ANGLOSAXON_RECALL: 81.1% 
#       REAMRKS: lower accuracy. Somewhere between 5 and 15 seems to be optimal weight

#########################################
############ evaluate the model #########
#########################################

# for precision, recall and f1 in multiclass problems:
#https://towardsdatascience.com/multi-class-metrics-made-simple-part-ii-the-f1-score-ebe8b2c2ca1

## predict classes on validation set:
tmp <- df_train[-train_idx, ]
tmp$pred <- as.numeric(model %>% predict_classes(x_val[, ,]))
tmp$pred <- y_classes[tmp$pred + 1, "levels"]
tmp$res <- tmp$origin == tmp$pred

## overall accuracy of the model on evaluation set: -----------------------------------
table(tmp$res) / nrow(tmp)

# confusion matrix by origin -----------------------------------------------------------
conf_matrix_fun <- function(region){
        tmp <- tmp %>% filter(origin == region | pred == region)
        tmp <- tmp %>% mutate(origin = ifelse(origin == region, 1, 0),
                              pred = ifelse(pred == region, 1, 0))
        tmp <- table(tmp$origin, tmp$pred)
        return(tmp)
}
conf_matrix_fun("AngloSaxon")

## Precision by origin: --------------------------------------------------------
# i.e. "how many of the predicted origin are indeed of this origin?" (TP / TP + FP)
# "From all predicted AngloSaxons, how many are indeed AngloSaxons?"
origin_precision <- tmp %>% group_by(pred) %>% summarise(n_obs = n(), 
                                                         origin_precision = sum(res == TRUE) / n()) %>%
        rename(region = pred) %>%
        arrange(origin_precision)
origin_precision %>% arrange(origin_precision)
mean(origin_precision$origin_precision)

## Recall by origin: -----------------------------------------------------------
# "how many form a given origin are also predicted to be of this origin?" (TP / TP + FN)
# "how many AngloSaxons are indeed predicted as such?"
origin_recall <- tmp %>% group_by(origin) %>% summarise(n_obs = n(), 
                                                        origin_recall = sum(res == TRUE) / n()) %>%
        rename(region = origin) %>%
        arrange(origin_recall)
origin_recall %>% arrange(origin_recall)
mean(origin_recall$origin_recall)

## F1 by origin ----------------------------------------------------------------
origin_eval <- merge(origin_precision[, c("region", "origin_precision")],
                     origin_recall[, c("region", "origin_recall")], by = "region")
origin_eval$f1 <- 2 * (origin_eval$origin_precision * origin_eval$origin_recall) / 
        (origin_eval$origin_precision + origin_eval$origin_recall)
origin_eval %>% arrange(f1)
mean(origin_eval$f1)

# Accuracy by origin: ------------------------------------------
acc_fun <- function(ctry){
        conf_matrix <- conf_matrix_fun(ctry)
        acc <- sum(diag(conf_matrix)) / sum(conf_matrix)
        return(acc)
}
origin_eval$accuracy <- unlist(lapply(origin_eval$region, acc_fun))
origin_eval %>% arrange(accuracy)

## median values and range of values across classes:
sapply(origin_eval[,-1], median)
sapply(origin_eval[,-1], range)

# GET MORE SAMPLES FROM IRAN & SOUTH-EAST ASIA

####################################
######### SAVE THE MODEL ###########
####################################

model %>% save_model_hdf5(file = "/scicore/home/weder/nigmat01/Data_inventor_migration/origin_class_model.h5")


## To-Do's----------------------------------------------
## 1) weight initialization
## 2) higher drop-out to prevent overfitting but be careful
## 3) larger batch size to ensure that there are a few labels of each class per batch
## 4) add early stopping & weight decay
## 5) maybe make a smaller model
## 6) maybe make class_weights proportional to share? e.g. weight_i = (1 / N_i)*N/2 
## https://www.tensorflow.org/tutorials/structured_data/imbalanced_data
## https://karpathy.github.io/2019/04/25/recipe/#2-set-up-the-end-to-end-trainingevaluation-skeleton--get-dumb-baselines
