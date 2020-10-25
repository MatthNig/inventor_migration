#################################################################
# Description:    Script to train the LSTM model to classify    #
#                 inventor origins based on names.              #
# Authors:        Matthias Niggli/CIEB UniBasel                 #
# Date:           21.10.2020                                    #
#################################################################

#### Import packages ---------------------------------------------------------
import tensorflow as tf
import numpy as np
import pandas as pd
import pyreadr
import os
import sys
from matplotlib import pyplot as plt
print("All packages loaded.")

#### Set directory-------------------------------------------------------------
path = "C:/Users/Matthias/Documents/GithubRepos/inventor_migration"
#path = "/scicore/home/weder/nigmat01/inventor_migration"
os.chdir(path)
os.getcwd()
print("Directories specified")

#### Load the data ------------------------------------------------------------
df_train = pd.read_csv("Data/training_data/df_train.csv")
print("Data for training the model successfully loaded.")

#### Encode data for training the model ---------------------------------------
function_path = path+"/Code/model_training"
if function_path not in sys.path:
    sys.path.append(function_path)
from names_encoding_function import encode_chars

## Specify parameters for encoding:
CHAR_DICT = list([chr(i) for i in range(97,123)])+[" ", "END"]
SEQ_MAX = 30
N_CHARS = 28
NAMES = df_train["full_name"]

## encode names
x_dat = encode_chars(names = ["matthias", "martina"], char_dict = CHAR_DICT,
             seq_max = SEQ_MAX, n_chars = N_CHARS)

## construct class levels
y_classes = {"regions": sorted(list(df_train["origin"].unique())),
             "numbers": [i +1 for i in range(len(df_train["origin"].unique()))]}

y_classes = pd.DataFrame(y_classes)
print("Data for training the model successfully transformed.")


#### Define train and test set ------------------------------------------------

