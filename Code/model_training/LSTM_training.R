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
library("stringi")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")
library("reticulate")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-23, nchar(getwd())) != "/innoscape-data-creation"){
        print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
                print("Directories are set and packages are loaded")}

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
        layer_lstm(units = 512, return_sequences = TRUE, # original: 512 hidden units
                   input_shape = c(ncol(x_train), dim(x_train)[3])) %>%
        layer_dropout(rate = 0.33) %>% # original: 0.2
        layer_lstm(units = 256, return_sequences = TRUE) %>% # original: 256 hidden units
        layer_dropout(rate = 0.33) %>% # original: 0.2
        layer_lstm(units = 64) %>% # original: 64 hidden units
        layer_dropout(rate = 0.1) %>% # original: 0.1
        layer_dense(units = ncol(y_train), activation = "softmax")
summary(model)

model %>% compile(
        optimizer =  "adam",
        loss = "categorical_crossentropy",
        metrics = c("accuracy"))

## class_weights: --------------------------------------------------------------
y_classes$class_weights <- 1
y_classes$class_weights <- ifelse(y_classes$levels %in% c("Turkey", "Persion", "MiddleEast", "SouthEastAsia"),
                                  10, 1)
# y_classes$class_weights <- ifelse(y_classes$levels %in% c("AngloSaxon", "German"), 10, 1)
y_classes[y_classes$levels == "AngloSaxon", "class_weights"] <- 5
# y_classes[y_classes$levels == "German", "class_weights" ] <- 10
CLASS_WEIGHTS <- as.list(y_classes$class_weights)
names(CLASS_WEIGHTS) <- y_classes$numbers

## fit the model ---------------------------------------------------------------
EPOCHS <- 20
BATCH_SIZE <- 256 # original = 128

hist <- model %>% fit(
        x = x_train, y = y_train, 
        class_weights = CLASS_WEIGHTS,
        validation_data = list(x_val, y_val),
        callbacks = list(callback_early_stopping(monitor = "val_loss", 
                                                 patience = 3, 
                                                 restore_best_weights = TRUE)),
        epochs = EPOCHS, batch_size = BATCH_SIZE, verbose = 2)

# Results: ---------------------------------------------------------------------
# => 80.01% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k)
# => 79.23% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights = both 5)
# => 83.33% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights = both 10) => overfitting and little learning
# => 78.61% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights Germany only = 15 & reduced model complexity)
# => 79.64% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights 7.5/15)
# => 78.33% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights both 7.5)
# => 82.93% accuracy with 10 epochs (RUS/EE & China/SEA combined, maximum sample i.e. 32k + class_weights Germany only = 10) => overfitting and some learning
# => 75.13% accuracy with 10 epochs (RUS/EE & China/SEA combined, enhanced sample + class_weights Germany only = 10, BATCH_SIZE = 256)
# => 81.60% accuracy with 10 epochs (RUS/EE & China/SEA combined, enhanced sample, no weights, BATCH_SIZE = 256)

# => 81.87% accuracy with 20 epochs (RUS/EE & China/SEA combined, full sample, no weights, BATCH = 256): all classes with minimum recall/precision of 75% 
# => 82.51% accuracy with 20 epochs (RUS/EE & China/SEA combined, full sample, no weights, BATCH = 256, dropout = 0.33): recall range is slightly larger than before

## IDEA
# The goal is that recall/precision is similar for all classes. then we have no bias when calculating shares
# => Thus, increase class weights for those classes where recall is lower than average

## EXTENSIONS  ----------------------------------

## (1) results with China & SE-Asia separated:
# => 81.70% accuracy with 20 epochs (full sample, no weights, BATCH = 256, dropout = 0.33(0.1 at last stage))
#    Model would maybe even learn more with more than 20 eepochs
#    China: 84.19% f1, SEAsia: 37.62% f1, JPN: 88.16% f1, KOR: 88.78% f1
#    PERFORMANCES DO NOT DECREASE FOR OTHER ASIAN COUNTRIES BUT VERY LOW FOR SE-ASIA. This does not make sense

## (2) results with Persia & Turkey separated:
# => 80.99% accuracy with 20 epochs (full sample, BATCH = 256, dropout = 0.33(0.1 at last stage))
#    MiddleEast: 64.8% f1, Turkey: 76.05% f1, Persia: 70.47% f1
#    AngloSaxon: f1: 0.7867784 acc: 0.6485035

## (3) results with all of them separated (weights = 10) AND weights for AngloSaxon (weight = 5)
# => 80.62% accuracy with 20 epochs (full sample, weight = 5 for Turkey, MiddleEast, Persia & SEASia, 
#                                     BATCH = 256, dropout = 0.33(0.1 at last stage))
# => Model would maybe even learn more with more than 20 epochs
#    China: xxxx acc, SEAsia: xxxx acc, JPN: xxxx acc, KOR: xxxx acc
#    MiddleEast: xxxx acc, Turkey: xxxx acc, Persia: xxxx acc 
#    AngloSaxon: f1: xxxx acc: xxxx

#########################################
############ evaluate the model #########
#########################################

tmp <- df_train[-train_idx, ]
tmp$pred <- as.numeric(model %>% predict_classes(x_val[, ,]))
tmp$pred <- y_classes[tmp$pred + 1, "levels"]
tmp$res <- tmp$origin == tmp$pred

## overall accuracy: -----------------------------------------------------------
table(tmp$res)/nrow(tmp)

# confusion matrix by origin -----------------------------------------------------------
conf_matrix_fun <- function(region){
        tmp <- tmp %>% filter(origin == region | pred == region)
        tmp <- tmp %>% mutate(origin = ifelse(origin == region, 1, 0),
                              pred = ifelse(pred == region, 1, 0))
        tmp <- table(tmp$origin, tmp$pred)
        return(tmp)
}
conf_matrix_fun("AngloSaxon")

## precision by origin: --------------------------------------------------------
# => i.e. "how many of the predicted class are also from this class?" (TP / TP + FP)
origin_precision <- tmp %>% group_by(pred) %>% summarise(n_obs = n(), 
                                                         origin_precision = sum(res == TRUE) / n()) %>%
        rename(region = pred) %>%
        arrange(origin_precision)
origin_precision %>% arrange(origin_precision)

## recall by origin: -----------------------------------------------------------
# "how many form one classes are predicted in this class?" (TP / TP + FN)
origin_recall <- tmp %>% group_by(origin) %>% summarise(n_obs = n(), 
                                                        origin_recall = sum(res == TRUE) / n()) %>%
        rename(region = origin) %>%
        arrange(origin_recall)
origin_recall %>% arrange(origin_recall)

## F1 by origin ----------------------------------------------------------------
origin_eval <- merge(origin_precision[, c("region", "origin_precision")],
                     origin_recall[, c("region", "origin_recall")], by = "region")
origin_eval$f1 <- 2 * (origin_eval$origin_precision * origin_eval$origin_recall) / 
        (origin_eval$origin_precision + origin_eval$origin_recall)
origin_eval %>% arrange(f1)

# accuracy of all origin classes ------------------------------------------
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


# Remarks -------------------------------------------
# weighting seems to make things worse because it always makes precision increasing
# => challenge is that German is relatively bad. This is a problem because of our focus on CH
# => maybe also weight French

## To-Do's----------------------------------------------
## 1) weight initialization
## 2) higher drop-out to prevent overfitting but be careful
## 3) larger batch size to ensure that there are a few labels of each class per batch
## 4) add early stopping & weight decay
## 5) maybe make a smaller model
## 6) maybe make class_weights proportional to share? e.g. weight_i = (1 / N_i)*N/2 
## https://www.tensorflow.org/tutorials/structured_data/imbalanced_data
## https://karpathy.github.io/2019/04/25/recipe/#2-set-up-the-end-to-end-trainingevaluation-skeleton--get-dumb-baselines
