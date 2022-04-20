# Import os 
from collections import UserString
import os

# Print the current working directory 
print("Current working directory: {0}".format(os.getcwd()))

# Change the current working directory source to file location 
path = os.chdir('/Users/hobbsd/Documents/GitHub/DS-SupervisedLearning/')

# Import libraries
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib as mpl
import matplotlib.pyplot as plt

# Set up matplotlib 
mpl.rc('axes', labelsize=14)
mpl.rc('xtick', labelsize=12)
mpl.rc('ytick', labelsize=12)

# set a random seed
np.random.seed(42)

# Load data
df = pd.read_csv(r'./Data/data_for_python.csv')

# Explore the Data
print("df.keys():\n", df.keys())
df.head()
df.tail()
df.info() # Checks for missing data

# How are sex and race coded in the data? 
print(df["SEX"].value_counts())
print(df["RACE"].value_counts())

# Recode sex and race as numberical
df["SEX"]=df["SEX"].replace(("Male","Female"),(0,1)) # Males: 0, Females: 1
df["RACE"]=df["RACE"].replace(("White","non-White"),(0,1)) # White: 0, non-White: 1
df.head()

df.describe()

df.hist(bins=50, figsize=(20,15));





























