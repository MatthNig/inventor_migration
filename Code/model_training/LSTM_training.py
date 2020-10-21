#################################################################
# Description:    Script to train the LSTM model to classify    #
#                 inventor origins based on names.              #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           21.10.2020                                    #
#################################################################

# Import packages
import tensorflow as tf
import numpy as np
import pandas as pd
import pyreadr
import os
from matplotlib import pyplot as plt
print("All libraries loaded.")

# Set directory---------------------------------------------------------------
path = "C:/Users/Matthias/Documents/GithubRepos/inventor_migration"
os.chdir(path)
os.getcwd()
print("Directories specified")

## Load the data -------------------------------------------------------------
df_train = pd.read_csv("Data/training_data/df_train.csv")
x_dat = pyreadr.read_r("Data/training_data/x_dat.rds")
y_dat = pyreadr.read_r("Data/training_data/y_dat.rds")
print("Data for training the model successfully loaded.")

## construct class levels -----------------------------------------------------

# y_classes <- data.frame(
#         levels = levels(as.factor(df_train$origin)),
#         numbers = seq(length(unique(df_train$origin)))
#         )
# print("Data for training the model successfully loaded.")
