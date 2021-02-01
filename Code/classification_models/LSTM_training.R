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
if(substr(x = getwd(), 
          nchar(getwd())-17, nchar(getwd())) == "inventor_migration"){
  print("Working directory corresponds to repository directory")}else{
    print("Make sure your working directory is the repository directory.")}
#setwd(...)

# set seed for reproducibility --------------------------------------------
set.seed(10082020)
#tensorflow::tf$random$set_seed(10082020) # this disables GPU
#https://github.com/rstudio/keras/issues/890

# load encoding function and function parameters--------------------------------
source(file = paste0(getwd(), "/Code/classification_models/names_encoding_function.R"))
PARAMS <- read.csv(file = paste0(getwd(), "/Data/training_data/PARAMS.csv"))
SEQ_MAX <- PARAMS$SEQ_MAX
N_CHARS <- PARAMS$N_CHARS
CHAR_DICT <- read.csv(file = paste0(getwd(), "/Data/training_data/CHAR_DICT.csv"))
CHAR_DICT <- CHAR_DICT$x
print("Function and function parameters loaded.")


################################
## Load the data for training ##
################################

df_train <- read.csv(file = paste0(getwd(), "/Data/training_data/df_train.csv"))
#df_olympic <-
names(df_olympic)[1] <- "full_name"
df_train <- rbind(df_train, df_olympic)
unique(df_train$origin)
y_classes <- data.frame(
        levels = levels(as.factor(df_train$origin)),
        numbers = seq(length(unique(df_train$origin)))
        )
print("Data for training the model successfully loaded.")

#####################################################
######### encode the features and outcomes ##########
#####################################################

#### outcome: origin classes
y_dat <- as.factor(df_train$origin)
levels(y_dat) <- y_classes$numbers
y_dat <- as.numeric(as.character(y_dat))
y_dat <- to_categorical(y_dat)
y_dat <- y_dat[, -1]
print("All names classified and encoded for training the model.")

#### features: names encoding
x_dat <- encode_chars(names = df_train$full_name,
                      seq_max = SEQ_MAX,
                      char_dict = CHAR_DICT,
                      n_chars = N_CHARS)
paste("names are one-hot-encoded with shape: ", 
      paste0("(", paste(dim(x_dat), collapse = ", "), ")")
)

################################################
######### split to train and test set ##########
################################################

train_frac <- 0.8 # fraction of data to train the model
N <- nrow(df_train)
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
BATCH_SIZE <- 512#256

## fit the model
hist <- model %>% fit(
        x = x_train, y = y_train, 
        # class_weights = CLASS_WEIGHTS,
        validation_data = list(x_val, y_val),
        callbacks = list(callback_early_stopping(monitor = "val_loss", 
                                                 patience = 3, 
                                                 restore_best_weights = TRUE)),
        epochs = EPOCHS, batch_size = BATCH_SIZE, verbose = 2)

## IDEA:
# The goal is to have maximum accuracy for the Anglo-Saxons and relatively equal f1 for all others

## RESULTS  ----------------------------------
# (1)   weight AngloSaxon = 10, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 81.5%, TOTAL F1: 77.81%, 
#       ANGLOSAXON_ACC: 65.15%, ANGLOSAXON_F1: 78.89%, ANGLOSAXON_PRECISION: 75.9%, ANGLOSAXON_RECALL: 82.2%  
#       REAMRKS: Model could further learn
#       => saved this model

# (2)   weight AngloSaxon = 15, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 80.84%, AVERAGE_F1: 76.10%, 
#       ANGLOSAXON_ACC: 63.42%, ANGLOSAXON_F1: 77.62%, ANGLOSAXON_PRECISION: 84.01% 
#       REAMRKS: Increasing weights did not help to raise accuracy

# (3)   weight AngloSaxon = 5, all separated except Russia/EastEurope no weights, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 80.33%, AVERAGE_F1: 75.77%, 
#       ANGLOSAXON_ACC: 62.61%, ANGLOSAXON_F1: 77.01%, ANGLOSAXON_PRECISION: 73.3%, ANGLOSAXON_RECALL: 81.1% 
#       REAMRKS: lower accuracy. Somewhere between 5 and 15 seems to be optimal weight

# (4)   MORE DATA: weight AngloSaxon = 10, all separated except Russia/EastEurope, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 81.9%, AVERAGE_F1: 80.7%, 
#       ANGLOSAXON_ACC: 63.9%, ANGLOSAXON_F1: 78.0%, ANGLOSAXON_PRECISION: 72.3%, ANGLOSAXON_RECALL: 84.7%  
#       REAMRKS: Model could further learn, F1 on small groups (SEA, Persian mostly) are still not so good.
# => saved this model as current classification model

## 21.11.2020
# (5)   EVEN MORE DATA: weight AngloSaxon = 10, all separated except Russia/EastEurope, BATCH = 256, EPOCH = 20, DROPOUT = 0.33 / 0.1
#       TOTAL_ACCURACY: 82.7%, AVERAGE_F1: 80.8%, 
#       ANGLOSAXON_ACC: 64.9%, ANGLOSAXON_F1: 78.7%, ANGLOSAXON_PRECISION: 78.9%, ANGLOSAXON_RECALL: 78.6%  
#       REAMRKS: maybe adjust weights for anglosaxons, Hispanics have high precision now, which ist good.
#                Turkey improved drastically. weighted F1 would be even higher because only SEA is lower than 0.78 on F1

## 22.01.2021
# (6)   SAME AS (5) BUT NO CLASS WEIGHTS
#       TOTAL_ACCURACY: 82.7%, AVERAGE_F1: 80.8%, 
#       ANGLOSAXON_ACC: 64.9%, ANGLOSAXON_F1: 78.7%, ANGLOSAXON_PRECISION: 78.9%, ANGLOSAXON_RECALL: 78.6%  
#       REAMRKS: maybe adjust weights for anglosaxons, Hispanics have high precision now, which ist good.
#                Turkey improved drastically. weighted F1 would be even higher because only SEA is lower than 0.78 on F1


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
res <- evaluate(model, x = x_val, y = y_val, verbose = 0)
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

# weighted average in test data:
weights <- df_train %>% group_by(origin) %>% 
  summarize(N = n(),
            weight = N/nrow(df_train)) %>% 
  rename(region = origin)
origin_eval <- merge(origin_eval, weights, by = "region")
weighted.mean(x = origin_eval$f1, w = origin_eval$weight)

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
sapply(origin_eval[,-1], mean)
sapply(origin_eval[,-1], range)

####################################
######### SAVE THE MODEL ###########
####################################

model %>% save_model_hdf5(file = paste0(getwd(), "/Data/classification_model/origin_class_model.h5"))


## To-Do's----------------------------------------------
## 1) weight initialization
## 2) higher drop-out to prevent overfitting but be careful
## 3) larger batch size to ensure that there are a few labels of each class per batch
## 4) add early stopping & weight decay
## 5) maybe make a smaller model

## resources
## https://www.tensorflow.org/tutorials/structured_data/imbalanced_data
## https://karpathy.github.io/2019/04/25/recipe/#2-set-up-the-end-to-end-trainingevaluation-skeleton--get-dumb-baselines
